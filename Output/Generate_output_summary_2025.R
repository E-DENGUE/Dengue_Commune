library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(spdep)
library(sp)
library(MASS)
library(surveillance)
library(tidyr)
library(tidyverse)
library(sf)
library(scales)
library(dplyr)
library(lubridate)
library(readr)
library(dplyr)
library(writexl)
library(fs)
library(scales)
###Read the quantile summary
## Please note:  
#- Horizon = 1 represents a 1-month ahead prediction.  
#- Horizon = 2 indicates a 2-month ahead forecast.  
#- Horizon = 3 corresponds to a 3-month ahead forecast.  

## The "date" in the quantile summary refers to the target date for which the prediction is made.

quantile_summary <- read.csv('./Output/Results_summary/final_summary_quantiles_match_fcode_2025.csv')

quantile_summary <- quantile_summary %>%
  mutate(date = as.Date(date))

###Join the quantile summary with actual data to see dengue cases at that date along with the populaiton
obs_case <- readRDS('./Model/Data/Full_data_set_with_covariates_and_lags.rds') %>%
  dplyr::select(fcode,date, obs_dengue_cases, pop_total)

quantiles_and_obs_cases <- left_join(quantile_summary,obs_case,by=c('date','fcode'))

quantiles_and_obs_cases$date <- as.Date(quantiles_and_obs_cases$date)

####add also the mean and sd of the historical for each row
all.baselines <- readRDS('./Model/Data/all_baselines_2025.rds') 

set.seed(10000)

all.baselines$baseline_mean <- NA
all.baselines$baseline_sd <- NA

for (i in 1:nrow(all.baselines)) {
  set.historic_log_mean <- all.baselines$mean_log_baseline[i]
  set.historic_log_sd <- all.baselines$sd_log_baseline[i]
  
  historic_samp_mu <- rnorm(10000, mean = set.historic_log_mean, sd = set.historic_log_sd)
  historic_samp <- rpois(10000, lambda = exp(historic_samp_mu))
  
  all.baselines$baseline_mean[i] <- mean(historic_samp)  
  all.baselines$baseline_sd[i] <- sd(historic_samp)
}

quantiles_and_obs_cases<- left_join(quantiles_and_obs_cases,all.baselines,by=c('date','fcode'))

quantiles_and_obs_cases$issue_date <- with(quantiles_and_obs_cases, date %m-% months(horizon))


#### in horizon use 1=1month, 2=2 months, 3= 3months 
quantiles_and_obs_cases <- quantiles_and_obs_cases %>%
  mutate(horizon = case_when(
    horizon == 1 ~ "1 month",
    horizon == 2 ~ "2 months",
    horizon == 3 ~ "3 months",
    TRUE ~ as.character(horizon)
  ))


# Rename columns
quantiles_and_obs_cases <- quantiles_and_obs_cases %>%
  rename(
    pred_date = date,
    Forecast_horizon = horizon,
    pred_cases = mean,
    pred_cases_95lb = lower_95CI,
    pred_cases_95ub = upper_95CI,
    hmean_cases=baseline_mean,
    hsd_cases=baseline_sd
  )

# Check the updated column names
names(quantiles_and_obs_cases)

quantiles_and_obs_cases <- quantiles_and_obs_cases %>%
  dplyr::select(-obs_dengue_cases.y, -pop_total.y) %>%
  rename(
    pop_total = pop_total.x
  )


# Reorder columns
quantiles_and_obs_cases_subset <- quantiles_and_obs_cases %>%
  dplyr::select(fcode, issue_date, pred_date, year,Forecast_horizon, hmean_cases, hsd_cases,
                pred_cases, pred_cases_95lb, pred_cases_95ub, obs_dengue_cases, pop_total)




# Check the updated column order
names(quantiles_and_obs_cases_subset)

########################add the risk score, z-score and probabilities for the year 2023
##selected date here is the forecasted date
#high_risk_threshold: the threshold used for intervention i.e. mean + 2sd
##low_risk_threshold :the low risk threshold (historical mean)
##medium_risk_threshold :the medium risk threshold ( historical mean + 1 std)
##probs_low_risk	P(forecast_dist ≤ mean) — probability that cases fall below or at the historical mean
##probs_med_risk	P(mean < forecast_dist ≤ mean + 1 SD) — probability that cases fall in the medium-risk range
## probs_high_risk	P(mean + 1 SD < forecast_dist ≤ mean + 2 SD) — probability of moderately elevated outbreak risk
## probs_outbreak_risk	P(forecast_dist > mean + 2 SD) — probability that cases are significantly above mean+2SD, indicating outbreak risk

risklevel<- read.csv('./Output/Results_summary/risk_score_and_z_scores_with_prob.csv') ##this file is generated in fun_threshold_risk_scores.R
risklevel <- risklevel %>%
  mutate(selected_horizon = case_when(
    selected_horizon == 1 ~ "1 month",
    selected_horizon == 2 ~ "2 months",
    selected_horizon== 3 ~ "3 months",
    TRUE ~ as.character(selected_horizon)
  ))




risklevel <- risklevel %>%
  dplyr::select(
    selected_fcode,
    selected_date,
    selected_horizon,
    risk_value_absoulte = risk.threshold1,  # Risk score absolute value
    risk_prob_absoulte = probability_matching_max_threshold,  # Probability P(x >= risk_value)
    risk_value_max_absolute = risk.threshold1_max_point,  # the number of cases/incidence rate corresponding to the maximum risk score
    risk_value_z = risk.threshold1z,  # Z-score risk value
    risk_prob_z = probability_matching_max_threshold_z , # Probability P(x >= risk_value) for z-score
    risk_value_max_z = risk.threshold1z_max_point,  # the number of cases/incidence rate corresponding to the maximum z-risk score
    high_risk_threshold = ucl,
    medium_risk_threshold = med,
    low_risk_threshold = historic_mean,
    outbreak_risk_prob= probs_outbreak_risk,
    high_risk_prob= probs_high_risk,
    medium_risk_prob= probs_med_risk,
    low_risk_prob= probs_low_risk
    )

risklevel$selected_date<- as.Date(risklevel$selected_date)

data_summary <- left_join(
  quantiles_and_obs_cases_subset, risklevel , by = c("pred_date" = "selected_date", 
         "Forecast_horizon" = "selected_horizon", 
         "fcode" = "selected_fcode"))
dim(data_summary)


data_summary <- data_summary %>%
  arrange(issue_date) %>%
  distinct()

dim(data_summary)
##save the results for each file separately 

output_dir <- "./Output/Output_Forecasted_by_Issue_Date"
dir_create(output_dir)

for (yr in unique(data_summary$year)) {
  
  year_folder <- file.path(output_dir, as.character(yr))
  dir_create(year_folder)
  
  year_data <- data_summary %>% 
    filter(issue_date >= as.Date(paste0(yr, "-01-01")) & issue_date <= as.Date(paste0(yr, "-12-01")))
  
  for (iss_date in unique(year_data$issue_date)) {
    
    # Subset data for the issue_date
    subset_data <- year_data %>% filter(issue_date == iss_date)
    
    file_name <- paste0("ed_", format(as.Date(iss_date), "%Y_%m"), "_pred.csv")
    file_path <- file.path(year_folder, file_name)
    
    write_csv(subset_data, file_path)
    
    print(paste("Saved:", file_path))
  }
}




