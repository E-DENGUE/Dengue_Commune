# Load necessary libraries
library(ggplot2)       # For data visualization
library(dplyr)         # For data manipulation
library(lubridate)     # For date manipulation
library(deSolve)       # For solving differential equations
library(minpack.lm)

# Load the data
dengue_data <- read_csv('./Data/aggregate_mdr.csv')
dengue_data$date <- as.Date(dengue_data$date)
lag3_monthly_dtr <- dengue_data$lag3_monthly_dtr

# Plot the data to understand trends
ggplot(data = dengue_data, aes(x = date, y = cases)) +
  geom_line() +
  labs(title = "Monthly Dengue Cases in the Mekong Delta",
       x = "Date", y = "Number of Cases")

# Define the mechanistic model (SIR model with seasonal and climate effects, births, and deaths)
sir_seasonal_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    # Birth rate assumed to be equal to death rate
    birth_rate <- parameters[["birth_rate"]]
    death_rate <- parameters[["death_rate"]]
    
    # Seasonality in the transmission rate
    beta_seasonal <- beta_0 * (1 + beta_1 * cos(2 * pi * (time + 1) / 12)) + beta_2 * lag3_monthly_dtr[time + 1]
    
    # Equations for the SIR model
    dS <- birth_rate * N - beta_seasonal * S * I / N - death_rate * S
    dI <- beta_seasonal * S * I / N - mu * I - death_rate * I
    dR <- mu * I - death_rate * R
    
    list(c(dS, dI, dR))
  })
}

# Initial conditions
initial_state <- c(S = 1e6, I = 523, R = 0)

# Define parameters
parameters <- list(
  beta_0 = 0.5,
  beta_1 = 0.3,
  beta_2 = 0.01,
  mu = 1 / 14,
  gamma = 0.01,
  birth_rate = 1 / (70 * 365),  # Example: birth rate assuming average lifespan of 70 years
  death_rate = 1 / (70 * 365)   # Example: death rate assuming average lifespan of 70 years
)

# Define the burn-in period
burn_in_period <- 12 * 10  # Run for 10 years before fitting

# Run the model for the burn-in period with scaled tolerances
burn_in_times <- seq(0, burn_in_period - 1, by = 1)
burn_in_out <- ode(y = initial_state, times = burn_in_times, func = sir_seasonal_model, parms = parameters, rtol = 1e-2, atol = 1e-2)

# Extract the final state after the burn-in
burn_in_final_state <- tail(as.data.frame(burn_in_out), 1)
burn_in_final_state <- c(S = burn_in_final_state$S, I = burn_in_final_state$I, R = burn_in_final_state$R)

# Optimizing parameters for the fitting period
fit_model <- function(params) {
  fit_parameters <- list(
    beta_0 = params[1],
    beta_1 = params[2],
    beta_2 = params[3],
    mu = params[4],
    gamma = params[5],
    birth_rate = parameters$birth_rate,
    death_rate = parameters$death_rate
  )
  out <- ode(y = burn_in_final_state, times = times, func = sir_seasonal_model, parms = fit_parameters, rtol = 1e-3, atol = 1e-3)
  sim_cases <- out[, "I"]
  return(sim_cases[1:length(dengue_data$cases)])
}

# Poisson likelihood objective function
objective_function <- function(params) {
  sim_cases <- tryCatch(fit_model(params), error = function(e) numeric(length(dengue_data$cases)) + Inf)
  if (any(!is.finite(sim_cases))) return(Inf)
  
  # Calculate Poisson log-likelihood
  log_likelihood <- sum(dengue_data$cases * log(sim_cases + 1e-10) - sim_cases - lgamma(dengue_data$cases + 1))
  
  # Return negative log likelihood because optim minimizes the objective
  return(-log_likelihood)
}

# Parameter estimation using optim
times <- seq(0, nrow(dengue_data) - 1, by = 1)
initial_params <- c(0.5, 0.3, 0.01, 1 / 14, 0.01)
optim_results <- optim(
  par = initial_params,
  fn = objective_function,
  method = "L-BFGS-B"
)

# Optimized parameters
optimized_params <- optim_results$par

# Forecasting
forecast_times <- seq(0, nrow(dengue_data) + 2, by = 1)  # Including 3-month forecast
forecast_params <- list(
  beta_0 = optimized_params[1],
  beta_1 = optimized_params[2],
  beta_2 = optimized_params[3],
  mu = optimized_params[4],
  gamma = optimized_params[5],
  birth_rate = parameters$birth_rate,
  death_rate = parameters$death_rate
)
forecast <- ode(y = burn_in_final_state, times = forecast_times, func = sir_seasonal_model, parms = forecast_params, rtol = 1e-3, atol = 1e-3)

# Convert output to data frame
forecast_df <- as.data.frame(forecast)
forecast_df <- forecast_df %>%
  mutate(date = seq(min(dengue_data$date), by = "1 month", length.out = length(forecast_times)))

# Plot model fit vs actual data
ggplot(data = forecast_df, aes(x = date)) +
  geom_line(aes(y = I, color = "Model")) +
  geom_line(data = dengue_data, aes(y = cases, color = "Actual")) +
  labs(title = "Model vs Actual Dengue Cases with Forecast",
       x = "Date", y = "Number of Cases") +
  scale_color_manual("", values = c("Model" = "blue", "Actual" = "red"))