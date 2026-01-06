##In console:
#salloc
#module load  R/4.2.3-foss-2022b
# R


library(dplyr)
library(parallel)
library(ggplot2)
library(tidyverse)
library(broom)
library(plotly)
library(viridis)
library(lubridate)
#library(gganimate)
library(pbapply)
library(scoringutils)
library(stringr)
options(dplyr.summarise.inform = FALSE)


N_cores = detectCores()

obs_epidemics <- readRDS( './Model/Data/observed_alarms_2025.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  rename(case_vintage=obs_dengue_cases) %>%
  dplyr::select(date, fcode,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))


##cleaned data set 
obs_case <- readRDS('./Model/Data/Full_data_set_with_covariates_and_lags.rds') %>%
  dplyr::select(date, fcode, obs_dengue_cases, pop_total)

obs_epidemics<- inner_join(obs_case,obs_epidemics,by=c("fcode"="fcode","date"="date"))




file.names1 <- list.files('./Output/Results/Results_spacetime/')
file.names2 <- list.files('./Output/Results/Results_pca', full.names = TRUE)
file.names3 <- list.files('./Output/Results/Results_hhh4/')




# process_file_INLA <- pblapply(file.names1, function(X) {
#   d1 <- readRDS(file = paste0('./Output/Results/Results_spacetime/', file.path(X)))
#   
#   date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
#   
#   #   # Find the position of the date pattern in the input string
#   date_match <- str_locate(X, date_pattern)
#   
#   modN <- str_sub(X, end = date_match[,'start'] - 1)
#   
#   date.test.in <- regmatches(X, regexpr(date_pattern, X))
#   
#   pred.iter <- d1$log.samps.inc %>%
#     reshape2::melt(., id.vars=c('date','fcode','horizon')) %>%
#     mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
#            modN=modN,
#            form=d1$form)
#   
#   return(pred.iter)
# })
# 
# 
# 
# # # Process HHH4 model files
# process_file_hhh4 <- lapply(file.names3,function(X){
#   #   
#   d1 <- readRDS(file=file.path(paste0('./Output/Results/Results_hhh4/',X)))
#   date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
#   # Find the position of the date pattern in the input string
#   date_match <- str_locate(X, date_pattern)
#   #   
#   modN <- str_sub(X, end = date_match[,'start'] - 1)
#   # Extract the date from the string using gsub
#   date.test.in <- regmatches(X, regexpr(date_pattern, X))
#   #   
#   pred.iter <- d1$log.samps.inc %>%
#     reshape2::melt(., id.vars=c('date','fcode','horizon')) %>%
#     mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
#            modN=modN,
#            form=d1$form)
#   
#   return(pred.iter)
# })
# 
# 
# process_file_pca <- pblapply(file.names2, function(X) {
#   d1 <- readRDS(X)
#   
#   if (grepl("PC_lags_weather", X)) {
#     modN <- "PC_lags_weather"
#   } else if (grepl("PC_lags", X)) {
#     modN <- "PC_lags"
#   } else if (grepl("PC_weather", X)) {
#     modN <- "PC_weather"
#   } else {
#     modN <- NA
#   }
#   
#   date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
#   date.test.in <- regmatches(X, regexpr(date_pattern, X))
#   
#   pred.iter <- d1$log.samps.inc %>%
#     reshape2::melt(id.vars = c('date','fcode','horizon')) %>%
#     mutate(
#       vintage_date = as.Date(date.test.in),
#       modN = modN,
#       form = d1$form
#     )
#   
#   return(pred.iter)
# })




###########################
#First extract the CRPS summaries
############################
##summaries
ds.list1.summary <- lapply(file.names1,function(X){
  
  d1 <- readRDS(file = paste0('./Output/Results/Results_spacetime/', file.path(X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  preds_df <- d1$scores %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)
  return(preds_df)
})



ds.list2_summary <- lapply(file.names2,function(X){
  
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
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  preds_df <- d1$scores %>%
    mutate(vintage_date=as.Date(date.test.in) , #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           date.test.in=date.test.in,
           form=paste(d1$form, collapse=' '))
  
  return(preds_df)
})



ds.list3_summary <- lapply(file.names3,function(X){
  
  d1 <- readRDS(file=file.path(paste0('./Output/Results/Results_hhh4/',X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  
  preds_df <- d1$scores %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)
  
  return(preds_df)
})

summary1 <- lapply(ds.list1.summary, function(X){
  X$forecast=as.factor(X$forecast)
  return(X)
}) %>%
  bind_rows()


summary2 <- bind_rows(ds.list2_summary)

summary3 <- lapply(ds.list3_summary , function(X){
  X$forecast=as.factor(X$forecast)
  return(X)
}) %>%
  bind_rows()


bind_rows(summary1,summary2,summary3) %>%
  filter(horizon>=1) %>%
  saveRDS( "./Output/Results_summary/crps_NEW_models.rds")

#########################################
## BRIER SCORES
#########################################
brier1 <- pblapply(file.names1, function(X) {
  d1 <- readRDS(file = paste0('./Output/Results/Results_spacetime/', file.path(X))) 
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Extract date from filename
  date.test.in <- as.Date(regmatches(X, regexpr(date_pattern, X)))
  
  
  if (date.test.in >= as.Date("2025-06-01")) {
    return(NULL)
  }
  
  date_match <- str_locate(X, date_pattern)
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(id.vars = c('date','fcode','horizon')) %>%
    left_join(obs_epidemics, by = c('date','fcode')) %>%
    mutate(pred_epidemic_2sd = value > log(threshold / pop_total * 100000),
           pred_epidemic_nb = value > log(threshold_nb / pop_total * 100000),
           vintage_date = date.test.in) %>%
    group_by(date, vintage_date, fcode, horizon) %>%
    summarize(
      prob_pred_epidemic_2sd = mean(pred_epidemic_2sd, na.rm = TRUE),
      prob_pred_epidemic_nb  = mean(pred_epidemic_nb,  na.rm = TRUE),
      obs_epidemic_2sd       = mean(epidemic_flag, na.rm = TRUE),  
      obs_epidemic_nb        = mean(epidemic_flag_nb,  na.rm = TRUE),
      .groups = "drop"
    )
  
  
  brier_2sd <- brier_score(pred.iter$obs_epidemic_2sd, pred.iter$prob_pred_epidemic_2sd)
  brier_nb <- brier_score(pred.iter$obs_epidemic_nb, pred.iter$prob_pred_epidemic_nb)
  
  brier.out <- cbind.data.frame('date' = pred.iter$date,
                                'modN' = modN,
                                'fcode' = pred.iter$fcode,
                                'horizon' = pred.iter$horizon,
                                brier_nb,
                                brier_2sd)
  return(brier.out)
})


brier_summary <- c(brier1) %>% 
  bind_rows() %>%
  mutate(monthN=month(date))%>%
  ungroup() 

saveRDS(brier_summary, "./Output/Results_summary/brier_NEW_models.rds")

brier2 <- pblapply(file.names2, function(X) {
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
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','fcode','horizon')) %>%
    left_join(obs_epidemics, by=c('date','fcode')) %>%
    mutate(pred_epidemic_2sd = value > log( threshold/pop_total_total*100000),
           pred_epidemic_nb = value > log( threshold_nb/pop_total*100000),
           vintage_date=date.test.in) %>%
    group_by(date,vintage_date, fcode, horizon) %>%
    summarize( prob_pred_epidemic_2sd = mean(pred_epidemic_2sd),
               prob_pred_epidemic_nb= mean(pred_epidemic_nb),
               obs_epidemic_2sd=mean(epidemic_flag),
               obs_epidemic_nb = mean(epidemic_flag_nb))
  
  brier_2sd <- brier_score( pred.iter$obs_epidemic_2sd,pred.iter$prob_pred_epidemic_2sd )
  brier_nb <- brier_score( pred.iter$obs_epidemic_nb,pred.iter$prob_pred_epidemic_nb )
  
  brier.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'fcode'=pred.iter$fcode, 'horizon'=pred.iter$horizon, brier_nb, brier_2sd)
})



brier3 <- lapply(file.names3,function(X){
  
  d1 <- readRDS(file=file.path(paste0('./Output/Results/Results_hhh4/',X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','fcode','horizon')) %>%
    left_join(obs_epidemics, by=c('date','fcode')) %>%
    mutate(pred_epidemic_2sd = value > log( threshold/pop_total*100000),
           pred_epidemic_nb = value > log( threshold_nb/pop_total*100000),
           vintage_date=date.test.in) %>%
    group_by(date,vintage_date, fcode, horizon) %>%
    summarize( prob_pred_epidemic_2sd = mean(pred_epidemic_2sd),
               prob_pred_epidemic_nb= mean(pred_epidemic_nb),
               obs_epidemic_2sd=mean(epidemic_flag),
               obs_epidemic_nb = mean(epidemic_flag_nb))
  
  brier_2sd <- brier_score( pred.iter$obs_epidemic_2sd,pred.iter$prob_pred_epidemic_2sd )
  brier_nb <- brier_score( pred.iter$obs_epidemic_nb,pred.iter$prob_pred_epidemic_nb )
  
  brier.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'fcode'=pred.iter$fcode, 'horizon'=pred.iter$horizon, brier_nb, brier_2sd)
})


#0=perfect prediction,1=bad
brier_summary <- c(brier1, brier2,brier3) %>% 
  bind_rows() %>%
  mutate(monthN=month(date))%>%
  ungroup() 

saveRDS(brier_summary, "./Output/Results_summary/brier_NEW_models.rds")



