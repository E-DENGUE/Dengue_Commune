library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)


set.seed(100000)
# Load the data
all.baselines <- readRDS('./Model/Data/all_baselines_2025.rds') 

obs_case <- readRDS('./Model/Data/Full_data_set_with_covariates_and_lags.rds') %>%
  dplyr::select(fcode,date, obs_dengue_cases, pop_total)


grouped_data1 <- readRDS('./Output/Results_summary/draws_from_the_ensemble_2025.rds')


grouped_data2<- bind_rows(grouped_data1)

grouped_data2$cases<- exp(grouped_data2$value) * grouped_data2$pop_total / 100000

grouped_data2$date<- as.Date(grouped_data2$date)
all.baselines$date <- as.Date(all.baselines$date)

calculate_risk_scores <- function(selected_fcode, selected_date, selected_horizon) {
  # Filter data
  grouped_data_filter <- grouped_data2 %>%
    dplyr::filter(date == as.Date(selected_date), fcode == selected_fcode, horizon == selected_horizon) %>%
    slice_head(n = 10000) %>%  # Keep only the first 10,000 rows per modN
    ungroup()
  
  baseline_filter <- all.baselines %>%
    dplyr::filter(date == as.Date(selected_date, origin = "1970-01-01"), fcode == selected_fcode)
  
 
  sd_historic <- baseline_filter$sd_log_baseline
#  lambda2 <- baseline_filter$mean_log_baseline
  
  # Generate samples
  # b1a <- tibble(.rows = 10000) %>%
  #   mutate(
  #     forecast1 =   grouped_data_filter$cases,
  #     historic1 = rpois(10000, exp(rnorm(10000, lambda2, sd = sd_historic))),
  #     RR1 = (forecast1 + 1) / (historic1 + 1)
  #   )
  # 
  
  #log_mean_baseline <- baseline_filter$mean_log_baseline
  
  b1a <- tibble(.rows = 10000) %>%
    mutate(
      forecast1 =   grouped_data_filter$cases, #OK
      lambda2 =    exp(rnorm(10000,  baseline_filter$mean_log_baseline, sd = sd_historic))* grouped_data_filter$pop_total/100000, #Predicted cases
  historic1 = rpois(10000,  lambda2 ),
  RR1 = (forecast1 + 1) / (historic1 + 1)
  )

    b1 <- b1a
  
  
  # Calculate thresholds and risk scores
    ucl <- mean(b1$historic1, na.rm = TRUE)+ 2 *sd (b1$historic1)
       med <- mean(b1$historic1, na.rm = TRUE)+ 1 *sd (b1$historic1)
        historic_mean <- mean(b1$historic1, na.rm = TRUE)
        historic_sd   <- sd(  b1$historic1, na.rm = TRUE)
   
  
  prob_RR1_gt_1 <- mean(b1$RR1 > 1)
  
  pop1_scores <- b1 %>%
    group_by(forecast1) %>%
    summarize(
      N_obs = n(),
      .groups = "drop"
    ) %>%
    mutate(
      probability1 = N_obs / sum(N_obs)
    )
  
    probs_outbreak_risk <- pop1_scores %>%
       filter(forecast1 > ucl) %>%
       summarize(probs_outbreak_risk = sum(probability1, na.rm = TRUE)) %>%
       pull(probs_outbreak_risk)

     probs_high_risk <- pop1_scores %>%
      filter(forecast1 > med & forecast1 <= ucl) %>%
      summarize(probs_high_risk = sum(probability1, na.rm = TRUE)) %>%
      pull(probs_high_risk)
  
    probs_med_risk <- pop1_scores %>%
       filter(forecast1 > historic_mean & forecast1 <= med) %>%
       summarize(probs_med_risk = sum(probability1, na.rm = TRUE)) %>%
       pull(probs_med_risk)
  
  
     probs_low_risk <- pop1_scores %>%
       filter(forecast1 <= historic_mean ) %>%
       summarize(probs_low_risk = sum(probability1, na.rm = TRUE)) %>%
      pull(probs_low_risk)
  
  
    # Initialize risk results dataframe
    risk_results <- data.frame(Threshold = numeric(), Probability = numeric(), RiskScore = numeric())
    
    # Define thresholds
    thresholds <- 1:max(b1$forecast1, na.rm = TRUE)
    
    # Compute risk scores while storing thresholds and probabilities
    risk_results <- data.frame(
      Threshold = thresholds,
      Probability = sapply(thresholds, function(threshold) {
        sum(pop1_scores$probability1[pop1_scores$forecast1 > threshold])
      }),
      RiskScore = sapply(thresholds, function(threshold) {
        sum(pop1_scores$probability1[pop1_scores$forecast1 > threshold]) * threshold
      })
    )
    
    
    # Extract index of maximum risk score
    index_max_risk <- which.max(risk_results$RiskScore)
    
    # Extract values using the correct index
    risk.threshold1 <- max(risk_results$RiskScore)  # The max risk score
    risk.threshold1_max_point <- risk_results$Threshold[index_max_risk]  # The threshold at max risk
    probability_matching_max_threshold <- risk_results$Probability[index_max_risk]
    
    # Compute standardized risk threshold
    risk.threshold1z <- sapply(thresholds, function(threshold) {
      sum(pop1_scores$probability1[pop1_scores$forecast1 > threshold]) * (threshold - historic_mean) /  historic_sd 
    })
    
    # Extract index of maximum standardized risk score
    index_max_risk_z <- which.max(risk.threshold1z)
    
    # Return results as a data frame
    out <- data.frame(
      prob_RR1_gt_1 = prob_RR1_gt_1,
      risk.threshold1 = risk.threshold1,
      risk.threshold1_max_point = risk.threshold1_max_point,
      probability_matching_max_threshold = probability_matching_max_threshold,
      risk.threshold1z = max(risk.threshold1z),
      risk.threshold1z_max_point = thresholds[index_max_risk_z],
      probability_matching_max_threshold_z = risk_results$Probability[index_max_risk_z],
      ucl = ucl,
          med=med,
           historic_mean = historic_mean,
          probs_outbreak_risk = probs_outbreak_risk,
          probs_high_risk = probs_high_risk,
          probs_med_risk = probs_med_risk,
           probs_low_risk=probs_low_risk,
      selected_fcode = selected_fcode,
      selected_date = selected_date,
      selected_horizon = selected_horizon
    
    
  )
  return(out)
}


selected_fcode<-"ED_AN_GIANG_AN_PHU_DISTRICT" 
selected_date<- '2025-07-01'

selected_horizon<- 1



# Unique values
selected_fcodes <- unique(grouped_data2$fcode)
selected_vintages <- unique(grouped_data2$vintage_date)
selected_horizons <- c(1,2,3)

all_results <- list()



# Loop through each combination
for (fcode in selected_fcodes) {
  for (vintage in selected_vintages) {
    for (horizon in selected_horizons) {
      
      selected_date <- as.Date(vintage, origin = "1970-01-01") %m+% months(horizon)
      
      result <- calculate_risk_scores(
        selected_fcode = fcode,
        selected_date = selected_date,
        selected_horizon = horizon
      )
      
      all_results <- append(all_results, list(result))
    }
  }
}





final_results <- bind_rows(all_results)


# Save results
write.csv(final_results, "./Output/Results_summary/risk_score_and_z_scores_with_prob.csv")
