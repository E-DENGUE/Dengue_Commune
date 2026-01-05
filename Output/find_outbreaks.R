library(dplyr)
library(tidyr)
library(readr)  # For saving CSV files
library(leaflet)
library(dplyr)
library(lubridate)
library(sf)
library(dplyr)
set.seed(10000)


obs_case <- readRDS('./Model/Data/Full_data_set_with_covariates_and_lags.rds') %>%
  dplyr::select(fcode,date, obs_dengue_cases, pop_total)



a<- read.csv('./Output/Results_summary/risk_score_and_z_scores_with_prob.csv') ##this file is generated in fun_threshold_risk_scores.R


a <- a %>%
  mutate(
    selected_date = as.Date(selected_date),                # convert to Date
    selected_horizon = as.numeric(selected_horizon),       # convert to numeric
    issue_date = selected_date %m-% months(selected_horizon)  # calculate issue date
  )


a <- a %>%
  mutate(selected_horizon = case_when(
    selected_horizon == 1 ~ "1 month",
    selected_horizon == 2 ~ "2 months",
    selected_horizon== 3 ~ "3 months",
    TRUE ~ as.character(selected_horizon)
  ))



dim(a)
names(a)
##Dengue incidence mean 
quantile_summary <- read.csv('./Output/Results_summary/final_summary_quantiles_match_fcode_2025.csv')

quantile_summary <- quantile_summary %>%
  mutate(date = as.Date(date))


print(quantile_summary)



final_summary_grouped <- quantile_summary %>%
  mutate(horizon = case_when(
    horizon == 1 ~ "1 month",
    horizon == 2 ~ "2 months",
    horizon== 3 ~ "3 months",
    TRUE ~ as.character(horizon)
  ))


a <- a %>%
  mutate(selected_date = as.Date(selected_date))

##add the dengue incidence to the threshold detection
a<- inner_join(a,final_summary_grouped , by= c("selected_date"="date",  "selected_fcode" = "fcode",'selected_horizon'='horizon'))
unique(a$selected_date)

d<- inner_join(a,obs_case,by= c("selected_date"="date",  "selected_fcode" = "fcode"))

d <-d %>%
  dplyr::select(-obs_dengue_cases.y, -pop_total.y) %>%
  rename(
    obs_dengue_cases = obs_dengue_cases.x,
    pop_total = pop_total.x
  )

d$pred_mean_inc <- d$mean/d$pop_total *100000

d <- d%>%
  mutate(
    year = lubridate::year(selected_date),
    month = lubridate::month(selected_date)
  )


d$ensemble_dist_wgt<- d$mean
d$ensemble_dist_wgt_inc<- d$mean/d$pop_total*100000

# Define the threshold
threshold_incidence <- 20



a_flagged <- d[,-c(1)] %>%
  # filter(month %in% rainy_season_months1)%>% # Focus on rainy season only
  group_by(selected_date, issue_date,selected_fcode, selected_horizon) %>%
  mutate(
    epidemic_flag_risk_incidence = as.numeric(if_else(risk.threshold1_max_point > ucl & pred_mean_inc > threshold_incidence, 1, 0)),
    epidemic_flag_z_score_incidence = as.numeric(if_else(risk.threshold1z_max_point > ucl & pred_mean_inc > threshold_incidence, 1, 0)),
    epidemic_flag_pred_mean = as.numeric(ifelse(ensemble_dist_wgt > ucl & pred_mean_inc > threshold_incidence, 1, 0)),
    
    # Count the number of methods flagging an epidemic
    epidemic_methods_count = rowSums(across(c(epidemic_flag_risk_incidence, epidemic_flag_z_score_incidence, epidemic_flag_pred_mean)), na.rm = TRUE),
    
    # Create new columns based on the count
    epidemic_flag_from_two_methods = as.numeric(if_else(epidemic_methods_count >= 2, 1, 0)),
    epidemic_flag_from_three_methods = as.numeric(if_else(epidemic_methods_count >= 3, 1, 0))
  ) %>%
  ungroup()

dim(a_flagged)

alerts_only <- (a_flagged) %>%
  dplyr::select(
    issue_date, selected_date, selected_fcode, selected_horizon, year,
    epidemic_flag_risk_incidence,
    epidemic_flag_z_score_incidence,
    epidemic_flag_pred_mean,
    epidemic_flag_from_two_methods,
    epidemic_flag_from_three_methods,
    mean,median  ,obs_dengue_cases,
    pop_total=pop_total
  ) %>% distinct()

dim(alerts_only)


# Save to CSV
write_csv(alerts_only, "./Output/Results_summary/epidemic_alerts_threshold20_three_methods.csv")


# 
# ####With reductions
# rainy_season_months1 <- c(6, 7, 8, 9, 10, 11)
# rainy_season_months2 <- c(5, 6, 7, 8, 9, 10, 11, 12)
# methods_setups <- list("Two_Methods" = "epidemic_flag_from_two_methods",
#                        "Three_Methods" = "epidemic_flag_from_three_methods")
# # Filter for rainy season and calculate epidemic flags
# a_flagged <- d[, -c(1)] %>%
#   filter(month %in% rainy_season_months1) %>%
#   group_by(selected_date, selected_fcode, selected_horizon) %>%
#   mutate(
#     epidemic_flag_risk_incidence = as.numeric(
#       risk.threshold1_max_point > ucl &
#         pred_mean_inc > threshold_incidence
#     ),
#     epidemic_flag_z_score_incidence = as.numeric(
#       risk.threshold1z_max_point > ucl &
#         pred_mean_inc > threshold_incidence
#     ),
#     epidemic_flag_pred_mean = as.numeric(
#       ensemble_dist_wgt > ucl_inc & pred_mean_inc > threshold_incidence
#     ),
#     
#     epidemic_methods_count = rowSums(across(
#       c(
#         epidemic_flag_risk_incidence,
#         epidemic_flag_z_score_incidence,
#         epidemic_flag_pred_mean
#       )
#     ), na.rm = TRUE),
#     
#     epidemic_flag_from_two_methods = as.numeric(epidemic_methods_count >= 2),
#     epidemic_flag_from_three_methods = as.numeric(epidemic_methods_count >= 3)
#   ) %>%
#   ungroup()
# 
# base_data <- st_drop_geometry(a_flagged) %>%
#   arrange(selected_fcode, selected_date) %>%
#   mutate(district = selected_fcode)
# 
# results_list <- list()
# 
# for (setup_name in names(methods_setups)) {
#   epidemic_flag_column <- methods_setups[[setup_name]]
#   updated_data <- base_data %>%
#     mutate(
#       epidemic_flag_intervention = !!sym(epidemic_flag_column),
#       obs_dengue_cases_adjusted = obs_dengue_cases
#     )
#   
#   # Apply one-month skip rule
#   for (j in 2:nrow(updated_data)) {
#     if (updated_data$epidemic_flag_intervention[j - 1] == 1 &&
#         updated_data$district[j] == updated_data$district[j - 1]) {
#       updated_data$epidemic_flag_intervention[j] <- 0
#     }
#   }
#   
#   # Apply reductions
#   for (j in 1:nrow(updated_data)) {
#     # Current month flagged: reduce by 50%
#     if (updated_data$epidemic_flag_intervention[j] == 1) {
#       updated_data$obs_dengue_cases_adjusted[j] <-
#         updated_data$obs_dengue_cases[j] * (1 - 0.50)
#     }
#     
#     # Previous month flagged: reduce by 25%
#     if (j > 1 &&
#         updated_data$epidemic_flag_intervention[j - 1] == 1 &&
#         updated_data$district[j] == updated_data$district[j - 1]) {
#       updated_data$obs_dengue_cases_adjusted[j] <-
#         updated_data$obs_dengue_cases_adjusted[j] * (1 - 0.25)
#     }
#     
#     # Next month flagged: reduce by 25%
#     if (j < nrow(updated_data) &&
#         updated_data$epidemic_flag_intervention[j + 1] == 1 &&
#         updated_data$district[j] == updated_data$district[j + 1]) {
#       updated_data$obs_dengue_cases_adjusted[j] <-
#         updated_data$obs_dengue_cases_adjusted[j] * (1 - 0.25)
#     }
#   }
#   
#   # Save CSV
#   # filename <- paste0("without_ROC_", setup_name, "_threshold_", threshold_incidence, ".csv")
#   #write_csv(updated_data, filename)
#   
#   # Summarize reductions
#   reduction_by_year <- updated_data %>%
#     group_by(year) %>%
#     summarise(
#       total_cases_before = sum(obs_dengue_cases, na.rm = TRUE),
#       total_cases_after = sum(obs_dengue_cases_adjusted, na.rm = TRUE),
#       sum_intervention_alert = sum(epidemic_flag_intervention, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       reduction_percentage = round(((total_cases_before - total_cases_after) / total_cases_before
#       ) * 100, 1),
#       threshold_incidence = threshold_incidence,
#       setup = setup_name
#     )
#   
#   results_list[[setup_name]] <- reduction_by_year
# }
# 
# # Combine and save
# final_results <- bind_rows(results_list)
# 
# # Display
# print(final_results)
# 
# write_csv(final_results,
#           "./Output/Results_summary/whole_year_25_50_25.csv")
