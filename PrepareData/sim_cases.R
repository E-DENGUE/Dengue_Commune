library(INLA)
library(dplyr)
library(sf)
library(spdep)

id_key <- vroom::vroom('../Data/inla_id_key.csv')

# Set parameters
set.seed(12345)
n_regions <- 489
start_year <- 2010
end_year <- 2024
n_months <- (end_year - start_year + 1) * 12  # 180 months

# Create time index
dates <- seq(as.Date("2010-01-01"), as.Date("2024-12-01"), by = "month")
time_df <- data.frame(
  time_idx = 1:n_months,
  date = dates,
  month = as.numeric(format(dates, "%m")),
  year = as.numeric(format(dates, "%Y"))
)

cat("Simulating data for", n_regions, "regions and", n_months, "months\n")

# Load and process temperature data
temperature_data <- vroom::vroom('./Data/meteorological.csv.gz') %>%
  select(monthdate, commune_id, t2m_avg) %>%
  mutate(date = as.Date(monthdate)) %>%
  arrange(commune_id, date) %>%
  group_by(commune_id) %>%
  mutate(temp3 = lag(t2m_avg, 3)) %>%  # 3-month lag
  ungroup() %>%
  select(date, commune_id, temp3) %>%
  right_join(id_key, by = c('commune_id' = 'l2_code')) %>%
  filter(!is.na(fcode))

# Read in neighbors
g <- inla.read.graph("../Data/MDR.graph.commune")
nb_list <- g$nbs

# Simulation parameters
params <- list(
  alpha_region = rnorm(n_regions, mean = -2, sd = 0.5),
  beta_cos = 0.3,
  beta_sin = 0.2,
  beta_temperature = 0.15,
  beta_spatial = 0.02,
  beta_temporal = 0.05
)

# Create base data frame
sim_data <- expand.grid(
  fcode = 1:n_regions,
  time_idx = 1:n_months
) %>%
  left_join(time_df, by = "time_idx") %>%
  left_join(temperature_data, by = c("date", "fcode")) %>%
  arrange(fcode, time_idx) %>%
  filter(date >= '2010-01-01')

# Add harmonic terms and pre-allocate columns
sim_data <- sim_data %>%
  mutate(
    cos_term = cos(2 * pi * month / 12),
    sin_term = sin(2 * pi * month / 12),
    obs_dengue_cases = 0,
    neighbor_cases_lag3 = 0,  # Cases in neighbors 3 months ago
    owobs_dengue_cases_lag3 = 0,       # Cases in own region 3 months ago
    log_lambda = NA_real_,
    lambda = NA_real_
  )

# Create lookup matrix for efficiency
region_time_matrix <- matrix(NA, nrow = n_regions, ncol = n_months)
for(i in 1:nrow(sim_data)) {
  region <- sim_data$fcode[i]
  time <- sim_data$time_idx[i]
  region_time_matrix[region, time] <- i
}

cat("Starting case simulation...\n")

# Simulate cases month by month
for(t in 1:n_months) {
  if(t %% 12 == 0) cat("Processing month", t, "of", n_months, "\n")
  
  current_indices <- region_time_matrix[, t]
  valid_current <- !is.na(current_indices)
  
  for(region in which(valid_current)) {
    row_idx <- current_indices[region]
    current_row <- sim_data[row_idx, ]
    
    # CORRECTED: Cases in neighboring regions exactly 3 months ago
    neighbor_cases_lag3 <- 0
    if(t > 3) {  # Need at least 3 months of history
      neighbors <- nb_list[[region]]
      if(length(neighbors) > 0) {
        # Get cases from exactly 3 months ago (t-3)
        lag3_time <- t - 3
        neighbor_indices_lag3 <- region_time_matrix[neighbors, lag3_time]
        valid_neighbor_indices <- neighbor_indices_lag3[!is.na(neighbor_indices_lag3)]
        
        if(length(valid_neighbor_indices) > 0) {
          # Sum of cases in neighboring regions 3 months ago
          neighbor_cases_lag3 <- sum(sim_data$obs_dengue_cases[valid_neighbor_indices])
        }
      }
    }
    
    # CORRECTED: Cases in own region exactly 3 months ago
    owobs_dengue_cases_lag3 <- 0
    if(t > 3) {
      lag3_time <- t - 3
      own_index_lag3 <- region_time_matrix[region, lag3_time]
      
      if(!is.na(own_index_lag3)) {
        # Cases in this region 3 months ago
        owobs_dengue_cases_lag3 <- sim_data$obs_dengue_cases[own_index_lag3]
      }
    }
    
    # Store lagged variables
    sim_data$neighbor_cases_lag3[row_idx] <- neighbor_cases_lag3
    sim_data$owobs_dengue_cases_lag3[row_idx] <- owobs_dengue_cases_lag3
    
    # Calculate log(lambda)
    log_lambda <- params$alpha_region[region] +
      params$beta_cos * current_row$cos_term +
      params$beta_sin * current_row$sin_term +
      params$beta_temperature * current_row$temp3 / 10 +
      params$beta_spatial * neighbor_cases_lag3 / 10 +
      params$beta_temporal * owobs_dengue_cases_lag3 / 10
    
    # Handle missing temperature data
    if(is.na(log_lambda)) {
      log_lambda <- params$alpha_region[region] +
        params$beta_cos * current_row$cos_term +
        params$beta_sin * current_row$sin_term +
        params$beta_spatial * neighbor_cases_lag3 / 10 +
        params$beta_temporal * owobs_dengue_cases_lag3 / 10
    }
    
    # Calculate lambda and simulate cases
    lambda <- exp(log_lambda)
    obs_dengue_cases <- rpois(1, lambda)
    
    # Store results
    sim_data$log_lambda[row_idx] <- log_lambda
    sim_data$lambda[row_idx] <- lambda
    sim_data$obs_dengue_cases[row_idx] <- obs_dengue_cases
  }
}

cat("Simulation complete!\n")

# Add derived variables
sim_data <- sim_data %>%
  mutate(
    temp3_std = as.numeric(scale(temp3)),
    neighbor_cases_lag3_std = as.numeric(scale(neighbor_cases_lag3)),
    owobs_dengue_cases_lag3_std = as.numeric(scale(owobs_dengue_cases_lag3)),
    time_linear = time_idx,
    time_scaled = as.numeric(scale(time_idx))
  ) %>%
  mutate(pop_total = 100000)

# Summary statistics
cat("\n=== SIMULATION SUMMARY ===\n")
cat("Total observations:", nrow(sim_data), "\n")
cat("Missing temperature values:", sum(is.na(sim_data$temp3)), "\n")

summary_stats <- sim_data %>%
  summarise(
    total_cases = sum(obs_dengue_cases, na.rm = TRUE),
    meaobs_dengue_cases = mean(obs_dengue_cases, na.rm = TRUE),
    mediaobs_dengue_cases = median(obs_dengue_cases, na.rm = TRUE),
    max_cases = max(obs_dengue_cases, na.rm = TRUE),
    prop_zero = mean(obs_dengue_cases == 0, na.rm = TRUE),
    mean_neighbor_lag3 = mean(neighbor_cases_lag3, na.rm = TRUE),
    mean_own_lag3 = mean(owobs_dengue_cases_lag3, na.rm = TRUE)
  )
print(summary_stats)

# Verify the lag structure with a sample
cat("\nVerifying lag structure for first region:\n")
verification <- sim_data %>%
  filter(fcode == 1) %>%
  select(date, time_idx, obs_dengue_cases, neighbor_cases_lag3, owobs_dengue_cases_lag3) %>%
  head(10)
print(verification)

# Save results
vroom::vroom_write(sim_data, "../Data/simulated_spatio_temporal_data_with_temp.csv.gz")

sim_data