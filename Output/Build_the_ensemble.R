library(dplyr)
library(parallel)
library(ggplot2)
library(tidyverse)
library(broom)
library(plotly)
library(viridis)
library(lubridate)
library(pbapply)
library(scoringutils)
library(stringr)
options(dplyr.summarise.inform = FALSE)
library(reshape2)

set.seed(10000000)
##cleaned data set 
obs_case <- readRDS('./Model/Data/Full_data_set_with_covariates_and_lags.rds') %>%
  select(date, fcode, obs_dengue_cases, pop_total)



##Results from spatio-temporal models, pca and hhh4 these files include the output of running the 5 top models
file.names1 <- list.files('./Output/Results/Results_spacetime/')
file.names2 <- paste0('./Output/Results/Results_pca/',list.files('./Output/Results/Results_pca'))
file.names3 <- list.files('./Output/Results/Results_hhh4/')




process_file_INLA <- pblapply(file.names1, function(X) {
  d1 <- readRDS(file = paste0('./Output/Results/Results_spacetime/', file.path(X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  #   # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','fcode','horizon')) %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)
  
  return(pred.iter)
})



# # Process HHH4 model files
process_file_hhh4 <- lapply(file.names3,function(X){
  #   
  d1 <- readRDS(file=file.path(paste0('./Output/Results/Results_hhh4/',X)))
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  #   
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  #   
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','fcode','horizon')) %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)
  
  return(pred.iter)
})


process_file_pca <- pblapply(file.names2, function(X) {
  d1 <- readRDS(file = file.path(X))
  
  if (grepl("PC_lags_weather", X)) {
    modN <- "PC_lags_weather"
  } else if (grepl("PC_lags", X)) {
    modN <- "PC_lags"
  } else if (grepl("PC_weather", X)) {
    modN <- "PC_weather"
  } else {
    modN <- NA  # Handle other cases if necessary
  }
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  # Extract the date from the string using regmatches
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','fcode','horizon')) %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)
  
  return(pred.iter)
})
# 


summary <-  bind_rows(process_file_INLA,process_file_hhh4, process_file_pca)

#  exclude mod4 result form the data process summary because this modes is the reference model that we use for comparison 
summary <- summary %>%
  filter(!modN %in% c("mod1_", "mod2_", "mod3_"))

# summary <- summary %>% 
#   filter(!modN %in% c("mod4_", "mod5_", "mod6_"))

##these weights obtained from the TSCV period and it has been used in the evaluation period as well as for new data set 

weights <- data.frame(
  horizon = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3),
  modN = c("mod6_", "mod5_", "modhhh4_power_precip_temp_",  "PC_lags", "mod4_" ,
           "mod6_", "mod5_", "modhhh4_power_precip_temp_",  "PC_lags", "mod4_",
           "mod6_", "mod5_", "modhhh4_power_precip_temp_",  "PC_lags", "mod4_"),
  w_i2 = c(0.232, 0.223, 0.220, 0.212, 0.113,
           0.234, 0.221, 0.205, 0.203, 0.137,
           0.235, 0.219, 0.197, 0.188, 0.161))
  
  
  # weights <- data.frame(
  #   horizon = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3),
  #   modN = c("mod3_", "modhhh4_power_precip_temp_", "mod2_", "PC_lags", "mod1_",
  #            "mod3_", "mod2_", "modhhh4_power_precip_temp_", "PC_lags", "mod1_",
  #            "mod3_", "mod2_", "modhhh4_power_precip_temp_", "PC_lags", "mod1_"),
  #   w_i2 = c(0.243, 0.221, 0.212, 0.196, 0.128, 
  #            0.253, 0.207, 0.203, 0.181, 0.156, 
  #            0.263, 0.203, 0.189, 0.174, 0.171)
  # )

##Please not the samples will take long time to gerate in the local computer and to read the files I used HPC
# Sample 10000 samples for each modN based on the weights
sampled_data <- summary %>%
  left_join(weights, by = c("horizon", "modN")) %>%
  group_by(fcode,vintage_date, date, horizon, modN) %>%
  sample_n(size = round(unique(w_i2) * 10000), replace = TRUE) %>%
  ungroup()

sampled_data <- sampled_data%>%
  left_join(obs_case, by = c("date", "fcode"))

saveRDS(sampled_data,'./Output/Results_summary/draws_from_the_ensemble_2025.rds')

##the value (draws) are given as log(obs_dengue_cases/pop_total *100k), to find the mean, lower cI and upper CI we need to find it in its original scale 
quantile_summary <- sampled_data %>%
  group_by(date, fcode, horizon,vintage_date) %>%
  summarise(
    mean = mean(exp(value) * pop_total, na.rm = TRUE) / 100000,
    median = quantile(exp(value) * pop_total / 100000, probs = 0.5, na.rm = TRUE),
    lower_95CI = quantile(exp(value) * pop_total / 100000, probs = 0.025, na.rm = TRUE),
    upper_95CI = quantile(exp(value) * pop_total / 100000, probs = 0.975, na.rm = TRUE),
    lower_50CI = quantile(exp(value) * pop_total / 100000, probs = 0.25, na.rm = TRUE),
    upper_50CI = quantile(exp(value) * pop_total / 100000, probs = 0.75, na.rm = TRUE),
    lower_80CI = quantile(exp(value) * pop_total / 100000, probs = 0.10, na.rm = TRUE),
    upper_80CI = quantile(exp(value) * pop_total / 100000, probs = 0.90, na.rm = TRUE),
    lower_75CI = quantile(exp(value) * pop_total / 100000, probs = 0.125, na.rm = TRUE),
    upper_75CI = quantile(exp(value) * pop_total / 100000, probs = 0.875, na.rm = TRUE),
    lower_85CI = quantile(exp(value) * pop_total / 100000, probs = 0.075, na.rm = TRUE),
    upper_85CI = quantile(exp(value) * pop_total / 100000, probs = 0.925, na.rm = TRUE),
    lower_99CI = quantile(exp(value) * pop_total / 100000, probs = 0.005, na.rm = TRUE),
    upper_99CI = quantile(exp(value) * pop_total / 100000, probs = 0.995, na.rm = TRUE),
    .groups = 'drop'
  )

quantile_summary <- quantile_summary %>%
  mutate(date = as.Date(date))

## The "date" in the quantile summary refers to the target date for which the prediction is made.
##extract summary statistics mean, UCI, and LCI
final_summary <- quantile_summary %>%
  left_join(obs_case, by = c("date", "fcode")) %>%
  dplyr::select(date,vintage_date, fcode, horizon, median,mean, lower_95CI, upper_95CI, obs_dengue_cases, pop_total,lower_50CI,upper_50CI,lower_75CI,
                upper_75CI,lower_80CI, upper_80CI,
                lower_85CI, upper_85CI,lower_99CI, upper_99CI)

write.csv(final_summary,'./Output/Results_summary/final_summary_quantiles_match_fcode_2025.csv')
