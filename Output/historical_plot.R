library(ggplot2)
library(dplyr)
library(scales) 

quantile_summary <- read.csv('./Output/Results_summary/final_summary_quantiles_match_fcode_2025.csv')

quantile_summary <- quantile_summary %>%
  mutate(date = as.Date(date))


###Join the quantile summary with actual data to see dengue cases at that date along with the populaiton
obs_case <- readRDS('./Model/Data/Full_data_set_with_covariates_and_lags.rds') %>%
  dplyr::select(fcode,date, obs_dengue_cases, pop_total)

quantiles_and_obs_cases <- left_join(quantile_summary,obs_case,by=c('date','fcode'))

# Ensure date is in Date format
quantiles_and_obs_cases$date <- as.Date(quantiles_and_obs_cases$date)


# # Prepare data
# plot_data <- quantiles_and_obs_cases %>%
#   filter(horizon == 3) %>%
#   group_by(date) %>%
#   summarise(
#     obs_cases = sum(obs_dengue_cases.x, na.rm = TRUE),
#     predicted_mean = sum(mean, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Reshape to long format for legend
# plot_data_long <- plot_data %>%
#   pivot_longer(cols = c(obs_cases, predicted_mean),
#                names_to = "type", values_to = "cases")
# 
# # Clean labels
# plot_data_long$type <- recode(plot_data_long$type,
#                               "obs_cases" = "Observed",
#                               "predicted_mean" = "Predicted")
# 
# # Plot with legend
# ggplot(plot_data_long, aes(x = date, y = cases, color = type)) +
#   geom_line(linewidth = 1) +
#   scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
#   scale_x_date(
#     date_breaks = "1 month",
#     date_labels = "%b\n%Y",
#     expand = c(0.01, 0.01)
#   ) +
#   labs(
#     title = "Observed vs Predicted Dengue Cases (Grouped by Date)",
#     x = "Date", y = "Dengue Cases",
#     color = "Legend"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)
#   )


###############

# Prepare data
plot_data <- quantiles_and_obs_cases %>%
  filter(horizon == 3) %>%
  group_by(date) %>%
  summarise(
    obs_cases = mean(obs_dengue_cases.x, na.rm = TRUE),
    predicted_mean = mean(mean, na.rm = TRUE),
    lower_CI = mean(lower_80CI, na.rm = TRUE),
    upper_CI = mean(upper_80CI, na.rm = TRUE),
    .groups = "drop"
  )

# Reshape observed & predicted to long format
plot_data_long <- plot_data %>%
  pivot_longer(cols = c(obs_cases, predicted_mean),
               names_to = "type", values_to = "cases")

# Clean up labels
plot_data_long$type <- recode(plot_data_long$type,
                              "obs_cases" = "Observed",
                              "predicted_mean" = "Predicted")

# Plot with ribbon and legend
ggplot() +
  # 95% CI ribbon (stays red and doesn't enter the legend)
  geom_ribbon(data = plot_data, aes(x = date, ymin = lower_CI, ymax = upper_CI),
              fill = "red", alpha = 0.3, inherit.aes = FALSE) +
  
  # Lines for observed and predicted
  geom_line(data = plot_data_long, aes(x = date, y = cases, color = type), linewidth = 1) +
  
  # Customize color legend
  scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
  
  # Monthly x-axis
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    expand = c(0.01, 0.01)
  ) +
  
  labs(
    title = "Observed vs Predicted Dengue Cases with 95% CI",
    x = "Date", y = "Dengue Cases",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)
  )



########

# Ensure date columns are in Date format
obs_case$date <- as.Date(obs_case$date)
quantile_summary$date <- as.Date(quantile_summary$date)

# 1. Get forecast start date
forecast_start <- min(quantile_summary$date[quantile_summary$horizon == 3])

# 2. Summarise observed data
obs_summary <- obs_case %>%
  group_by(date) %>%
  summarise(mean_val = mean(obs_dengue_cases, na.rm = TRUE), .groups = "drop") %>%
  filter(date <= forecast_start)

# 3. Summarise forecast data
forecast_summary <- quantile_summary %>%
  filter(horizon == 3) %>%
  group_by(date) %>%
  summarise(
    mean_val = mean(mean, na.rm = TRUE),
    lower_CI = mean(lower_80CI, na.rm = TRUE),
    upper_CI = mean(upper_80CI, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Combine into a single line data frame
line_df <- bind_rows(obs_summary, forecast_summary) %>%
  mutate(
    type = if_else(date < forecast_start, "Observed", "Forecast")
  )

# 5. Plot
ggplot() +
  # Forecast ribbon (CI)
  geom_ribbon(data = forecast_summary,
              aes(x = date, ymin = lower_CI, ymax = upper_CI),
              fill = "red", alpha = 0.3) +
  
  # Connected line: observed + forecast
  geom_line(data = line_df, aes(x = date, y = mean_val), color = "black", size = 1) +
  
  # Overlay forecast portion in red
  geom_line(data = line_df %>% filter(type == "Forecast"),
            aes(x = date, y = mean_val), color = "red", linewidth = 1) +
  
  # Optional: mark the transition point with a black dot
  geom_point(data = obs_summary %>% filter(date == max(date)),
             aes(x = date, y = mean_val), color = "black", size = 2) +
  
  labs(
    title = "Observed vs Forecasted Dengue Cases",
    x = "Date", y = "Mean Dengue Cases"
  ) +
  theme_minimal()

##############

obs_summary <-  quantiles_and_obs_cases %>%
  group_by(date) %>%
  summarise(mean_val = mean(obs_dengue_cases.x, na.rm = TRUE), .groups = "drop")

# 2. Summarise forecast data (for horizon 3)
forecast_summary <- quantiles_and_obs_cases %>%
  filter(horizon == 3) %>%
  group_by(date) %>%
  summarise(
    mean_val = mean(mean, na.rm = TRUE),
    lower_50 = mean(lower_50CI, na.rm = TRUE),
    upper_50 = mean(upper_50CI, na.rm = TRUE),
    lower_80 = mean(lower_80CI, na.rm = TRUE),
    upper_80 = mean(upper_80CI, na.rm = TRUE),
    lower_85 = mean(lower_85CI, na.rm = TRUE),
    upper_85 = mean(upper_85CI, na.rm = TRUE),
    lower_95 = mean(lower_95CI, na.rm = TRUE),
    upper_95 = mean(upper_95CI, na.rm = TRUE),
    lower_99 = mean(lower_99CI, na.rm = TRUE),
    upper_99 = mean(upper_99CI, na.rm = TRUE),
    .groups = "drop"
  )





ggplot(forecast_summary, aes(x = date)) +
  # Confidence bands with legend entries
  geom_ribbon(aes(ymin = lower_99, ymax = upper_99, fill = "99% CI"), alpha = 1) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = "95% CI"), alpha = 1) +
  geom_ribbon(aes(ymin = lower_85, ymax = upper_85, fill = "85% CI"), alpha = 1) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80, fill = "80% CI"), alpha = 1) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 1) +
  
  # Mean forecasted cases
  geom_line(aes(y = mean_val, color = "Forecasted Mean"), size = 1) +
  
  # Observed cases (dark red solid line)
  geom_line(data = obs_summary, aes(x = date, y = mean_val, color = "Observed Cases"), size = 1) +
  
  # Manual fill and color scales
  scale_fill_manual(
    name = "Confidence Interval",
    values = c(
      "99% CI" = "#cce5ff",
      "95% CI" = "#99ccff",
      "85% CI" = "#66b2ff",
      "80% CI" = "#3399ff",
      "50% CI" = "#0073e6"
    )
  )  +
  
  # Show all x-axis dates
  scale_x_date(
    date_breaks = "1 month",       # adjust as needed (e.g., "2 weeks" for dense plots)
    date_labels = "%b %Y",         # format like "Jan 2024"
    expand = c(0.01, 0.01)
  ) +
  
  labs(
    title = "Mean Predicted Dengue Cases (Horizon 3) with Observed Cases (all Dates)",
    x = "Date", y = "Average Dengue Cases"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels if overlapping
  )



ggplot(forecast_summary, aes(x = date)) +
  # Confidence bands with legend entries
  geom_ribbon(aes(ymin = lower_99, ymax = upper_99, fill = "99% CI"), alpha = 1) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = "95% CI"), alpha = 1) +
  geom_ribbon(aes(ymin = lower_85, ymax = upper_85, fill = "85% CI"), alpha = 1) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80, fill = "80% CI"), alpha = 1) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = "50% CI"), alpha = 1) +
  
  # Mean forecasted cases only
  geom_line(aes(y = mean_val, color = "Forecasted Mean"), size = 1) +
  
  # Manual fill and color scales
  scale_fill_manual(
    name = "CI Interval",
    values = c(
      "99% CI" = "#cce5ff",
      "95% CI" = "#99ccff",
      "85% CI" = "#66b2ff",
      "80% CI" = "#3399ff",
      "50% CI" = "#0073e6"
    )
  ) +
  scale_color_manual(
    name = "",  # Removes the title from color legend
    values = c("Forecasted Mean" = "black")  # Black for forecast line
  ) +
  
  # Show all x-axis dates
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  
  labs(
    title = "Mean Forecasted Dengue Cases (Horizon 3)",
    x = "Date", y = "Average Dengue Cases"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


