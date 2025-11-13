# ============================================================
# Commune-level Dengue Forecast Model Comparison (Baseline vs SpatioTemporal vs Full Mod3+)
# ============================================================

library(sf)
library(spdep)
library(INLA)
library(tidyverse)
library(lubridate)
library(vroom)
library(scoringRules)
library(scoringutils)
library(tmap)
library(purrr)
library(dplyr)
library(zoo)

# ============================================================
# Load and prepare data
# ============================================================

dengue <- vroom::vroom("./Data/case_data.csv.gz") %>%
  mutate(date = as.Date(date)) %>%
  arrange(fcode, date) %>%
  group_by(fcode) %>%
  mutate(
    y = obs_dengue_cases,
    lag1_y = lag(log(y + 1), 1),
    lag2_y = lag(log(y + 1), 2),
    lag3_y = lag(log(y + 1), 3),
    lag1_monthly_cum_ppt = lag(lag3_monthly_cum_ppt, 1),
    lag2_monthly_cum_ppt = lag(lag3_monthly_cum_ppt, 2),
    lag3_monthly_cum_ppt = lag(lag3_monthly_cum_ppt, 3),
    sin12 = sin(2 * pi * (as.integer(interval(min(date), date) %/% months(1)) + 1) / 12),
    cos12 = cos(2 * pi * (as.integer(interval(min(date), date) %/% months(1)) + 1) / 12),
    t = as.integer(interval(min(date), date) %/% months(1) + 1),
    log_cum_inc_12m = log(rollapplyr(obs_dengue_cases + 1, 12, sum, fill = NA)),
    log_cum_inc_24m = log(rollapplyr(obs_dengue_cases + 1, 24, sum, fill = NA)),
    log_cum_inc_36m = log(rollapplyr(obs_dengue_cases + 1, 36, sum, fill = NA))
  ) %>%
  ungroup()

# ============================================================
# Load shapefile and adjacency graph
# ============================================================

commune_sf <- readRDS("./Data/inla_shp_file.rds") %>%
  mutate(fcode = as.character(fcode)) %>%
  arrange(fcode)

row.names(commune_sf) <- commune_sf$fcode
commune_sf <- sf::st_make_valid(commune_sf)

nb_commune <- spdep::poly2nb(commune_sf, row.names = commune_sf$fcode)
nb2INLA("commune_adj.graph", nb_commune)
graph_commune <- inla.read.graph("commune_adj.graph")

# Ensure BYM2 recognizes names
graph_commune$names <- as.character(commune_sf$fcode)
graph_commune$nnodes <- length(graph_commune$names)

commune_sf <- commune_sf %>%
  mutate(fcode_id = as.numeric(factor(fcode, levels = fcode)))

dengue <- dengue %>%
  mutate(fcode = as.character(fcode)) %>%
  left_join(commune_sf %>% st_drop_geometry() %>% select(fcode, fcode_id), by = "fcode")

# ============================================================
# Helper — Negative Binomial scoring params
# ============================================================

compute_nb_params <- function(pred_mean, pred_sd) {
  var_pred <- pred_sd^2
  size_param <- pmax(pred_mean^2 / (var_pred - pred_mean), 1e-3)
  prob_param <- size_param / (size_param + pred_mean)
  list(size_param = size_param, prob_param = prob_param)
}

# ============================================================
# Model functions
# ============================================================

# --- BASELINE MODEL (IID + AR1) ---
inla_commune_baseline <- function(data, cutoff_date, forecast_horizon = 3) {
  message("Running Baseline model for cutoff: ", cutoff_date)
  cutoff_date <- as.Date(cutoff_date, origin = "1970-01-01")
  
  df <- data %>%
    filter(date <= cutoff_date %m+% months(forecast_horizon)) %>%
    drop_na(lag1_y, lag2_y, lag3_y) %>%
    arrange(fcode_id, date)
  
  formula_inla <- obs_dengue_cases ~
    1 + lag1_y + lag2_y + lag3_y + sin12 + cos12 +
    f(fcode_id, model = "iid") +
    f(t, model = "ar1")
  
  mod <- inla(
    formula_inla,
    data = df,
    family = "nbinomial",
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, waic = TRUE),
    num.threads = 6
  )
  
  df <- df %>%
    mutate(
      pred_mean = mod$summary.fitted.values$mean,
      pred_sd = mod$summary.fitted.values$sd
    )
  
  nbp <- compute_nb_params(df$pred_mean, df$pred_sd)
  
  df <- df %>%
    mutate(
      size_param = nbp$size_param,
      prob_param = nbp$prob_param,
      CRPS = scoringRules::crps_nbinom(y = obs_dengue_cases, size = size_param, prob = prob_param)
    )
  
  df_eval <- df %>%
    filter(date > cutoff_date) %>%
    summarise(
      RMSE = sqrt(mean((obs_dengue_cases - pred_mean)^2, na.rm = TRUE)),
      CORR = cor(obs_dengue_cases, pred_mean, use = "pairwise.complete.obs"),
      mean_CRPS = mean(CRPS, na.rm = TRUE)
    )
  
  list(model = mod, eval = df_eval, predictions = df)
}

# --- SPATIO-TEMPORAL MODEL (BYM2 + shared AR1) ---
inla_commune_spatiotemp <- function(data, graph_commune, cutoff_date, forecast_horizon = 3) {
  message("Running Spatio-Temporal model for cutoff: ", cutoff_date)
  cutoff_date <- as.Date(cutoff_date, origin = "1970-01-01")
  
  df <- data %>%
    filter(date <= cutoff_date %m+% months(forecast_horizon)) %>%
    drop_na(lag1_y, lag2_y, lag3_y) %>%
    arrange(fcode_id, date)
  
  formula_inla <- obs_dengue_cases ~
    1 + lag1_y + lag2_y + lag3_y +
    lag3_monthly_cum_ppt + lag3_avg_min_daily_temp +
    sin12 + cos12 +
    f(fcode_id, model = "bym2", graph = graph_commune, scale.model = TRUE) +
    f(t, model = "ar1")
  
  mod <- inla(
    formula_inla,
    data = df,
    family = "nbinomial",
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, waic = TRUE),
    num.threads = 6
  )
  
  df <- df %>%
    mutate(pred_mean = mod$summary.fitted.values$mean,
           pred_sd = mod$summary.fitted.values$sd)
  
  nbp <- compute_nb_params(df$pred_mean, df$pred_sd)
  
  df <- df %>%
    mutate(size_param = nbp$size_param,
           prob_param = nbp$prob_param,
           CRPS = scoringRules::crps_nbinom(y = obs_dengue_cases, size = size_param, prob = prob_param))
  
  df_eval <- df %>%
    filter(date > cutoff_date) %>%
    summarise(
      RMSE = sqrt(mean((obs_dengue_cases - pred_mean)^2, na.rm = TRUE)),
      CORR = cor(obs_dengue_cases, pred_mean, use = "pairwise.complete.obs"),
      mean_CRPS = mean(CRPS, na.rm = TRUE)
    )
  
  list(model = mod, eval = df_eval, predictions = df)
}

# --- FULL MOD3+ MODEL (BYM2 + replicate AR1 + RW1 + PC priors + offset + spatially varying coefficients) ---
inla_commune_mod3 <- function(data, graph_commune, cutoff_date, forecast_horizon = 3) {
  message("Running Full Mod3+ model for cutoff: ", cutoff_date)
  cutoff_date <- as.Date(cutoff_date, origin = "1970-01-01")
  
  df <- data %>%
    filter(date <= cutoff_date %m+% months(forecast_horizon)) %>%
    drop_na(lag3_y, lag3_monthly_cum_ppt, lag3_avg_min_daily_temp,
            log_cum_inc_12m, log_cum_inc_24m, log_cum_inc_36m) %>%
    arrange(fcode_id, date)
  
  formula_inla <- obs_dengue_cases ~
    1 + lag3_y +
    log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
    f(fcode_id, model = "bym2", graph = graph_commune, scale.model = TRUE, constr = TRUE,
      hyper = list(
        theta1 = list(prior = "pc.prec", param = c(1, 0.01)),
        theta2 = list(prior = "pc", param = c(0.5, 0.5))
      )) +
    # spatially varying climate effects
    f(fcode_id, lag3_monthly_cum_ppt, model = "bym2", graph = graph_commune, scale.model = TRUE, constr = TRUE) +
    f(fcode_id, lag3_avg_min_daily_temp, model = "bym2", graph = graph_commune, scale.model = TRUE, constr = TRUE) +
    f(t, replicate = fcode_id, model = "ar1",
      hyper = list(
        rho = list(prior = "pc.cor1", param = c(0.9, 0.9)),
        prec = list(prior = "pc.prec", param = c(1, 0.01))
      ), constr = TRUE) +
    f(month(date), model = "rw1", cyclic = TRUE, replicate = fcode_id, scale.model = TRUE,
      hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))
  
  mod <- inla(
    formula_inla,
    data = df,
    family = "nbinomial",
    E = df$population / 100000,
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, waic = TRUE),
    num.threads = 6
  )
  
  df <- df %>%
    mutate(pred_mean = mod$summary.fitted.values$mean,
           pred_sd = mod$summary.fitted.values$sd)
  
  nbp <- compute_nb_params(df$pred_mean, df$pred_sd)
  
  df <- df %>%
    mutate(size_param = nbp$size_param,
           prob_param = nbp$prob_param,
           CRPS = scoringRules::crps_nbinom(y = obs_dengue_cases, size = size_param, prob = prob_param))
  
  df_eval <- df %>%
    filter(date > cutoff_date) %>%
    summarise(
      RMSE = sqrt(mean((obs_dengue_cases - pred_mean)^2, na.rm = TRUE)),
      CORR = cor(obs_dengue_cases, pred_mean, use = "pairwise.complete.obs"),
      mean_CRPS = mean(CRPS, na.rm = TRUE)
    )
  
  list(model = mod, eval = df_eval, predictions = df)
}

# ============================================================
# Rolling forecast loop
# ============================================================

test_dates <- seq(as.Date("2018-01-01"), as.Date("2023-07-01"), by = "6 months")

results_baseline <- list()
results_spatiotemp <- list()
results_mod3 <- list()

for (cutoff in test_dates) {
  base <- inla_commune_baseline(dengue, cutoff)
  spa  <- inla_commune_spatiotemp(dengue, graph_commune, cutoff)
  mod3 <- inla_commune_mod3(dengue, graph_commune, cutoff)
  
  results_baseline[[as.character(cutoff)]] <- base
  results_spatiotemp[[as.character(cutoff)]] <- spa
  results_mod3[[as.character(cutoff)]] <- mod3
}

# ============================================================
# Combine predictions and compute CRPS/Brier
# ============================================================

all_preds <- bind_rows(
  map(results_baseline, "predictions") %>% bind_rows(.id = "cutoff") %>% mutate(model = "Baseline"),
  map(results_spatiotemp, "predictions") %>% bind_rows(.id = "cutoff") %>% mutate(model = "SpatioTemporal"),
  map(results_mod3, "predictions") %>% bind_rows(.id = "cutoff") %>% mutate(model = "Mod3Plus")
) %>%
  mutate(cutoff = as.Date(as.numeric(cutoff), origin = "1970-01-01"))

commune_thresholds <- dengue %>%
  group_by(fcode) %>%
  summarise(mean_cases = mean(obs_dengue_cases, na.rm = TRUE),
            sd_cases = sd(obs_dengue_cases, na.rm = TRUE)) %>%
  mutate(threshold = mean_cases + 2 * sd_cases)

scored <- all_preds %>%
  mutate(fcode = as.character(fcode)) %>%
  left_join(commune_thresholds, by = "fcode") %>%
  mutate(is_epidemic = obs_dengue_cases > threshold,
         p_epidemic = 1 - pnbinom(threshold, size = size_param, mu = pred_mean))

brier_summary <- scored %>%
  group_by(model) %>%
  summarise(mean_CRPS = mean(CRPS, na.rm = TRUE),
            mean_Brier = mean((p_epidemic - is_epidemic)^2, na.rm = TRUE))

print(brier_summary)

# ============================================================
# Diagnostics: CRPS and Brier over time
# ============================================================

eval_time <- scored %>%
  group_by(model, cutoff) %>%
  summarise(CRPS = mean(CRPS, na.rm = TRUE),
            Brier = mean((p_epidemic - is_epidemic)^2, na.rm = TRUE)) %>%
  ungroup()

ggplot(eval_time, aes(x = cutoff, y = CRPS, color = model)) +
  geom_line(size = 1) + geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  labs(title = "CRPS over Time", x = "Forecast cutoff", y = "Mean CRPS")

ggplot(eval_time, aes(x = cutoff, y = Brier, color = model)) +
  geom_line(size = 1) + geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  labs(title = "Brier Score over Time", x = "Forecast cutoff", y = "Mean Brier")

ggplot(scored, aes(x = model, y = CRPS, fill = model)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal(base_size = 13) +
  labs(title = "Distribution of CRPS across all cutoffs", y = "CRPS")

ggplot(scored, aes(x = model, y = (p_epidemic - is_epidemic)^2, fill = model)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal(base_size = 13) +
  labs(title = "Distribution of Brier Scores", y = "Brier")
