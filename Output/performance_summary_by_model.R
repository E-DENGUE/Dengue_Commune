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
file.names2 <- list.files('./Output/Results/Results_pca', full.names = TRUE)
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
  d1 <- readRDS(X)
  
  if (grepl("PC_lags_weather", X)) {
    modN <- "PC_lags_weather"
  } else if (grepl("PC_lags", X)) {
    modN <- "PC_lags"
  } else if (grepl("PC_weather", X)) {
    modN <- "PC_weather"
  } else {
    modN <- NA
  }
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(id.vars = c('date','fcode','horizon')) %>%
    mutate(
      vintage_date = as.Date(date.test.in),
      modN = modN,
      form = d1$form
    )
  
  return(pred.iter)
})



summary <-  bind_rows(process_file_INLA,process_file_hhh4, process_file_pca)

#  exclude mod4 result form the data process summary because this modes is the reference model that we use for comparison 
summary <- summary %>% 
  filter(!modN %in% c("mod1_", "mod2_", "mod3_"))


sampled_data <- summary%>%
  left_join(obs_case, by = c("date", "fcode"))



##the value (draws) are given as log(obs_dengue_cases/pop_total *100k), to find the mean, lower cI and upper CI we need to find it in its original scale 
quantile_summary <- sampled_data %>%
  group_by(date, fcode, horizon,vintage_date,modN) %>%
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

write.csv(final_summary,'./Output/Results_summary/prediction_summary_by_model.csv')