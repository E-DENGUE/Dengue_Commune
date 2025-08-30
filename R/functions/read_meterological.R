#Function for reading in raw, daily meteorological data by commune and outputing a monthly average by commune

read_meterological <- function(X) {
  data <- read_csv(X, col_names = FALSE,show_col_types = FALSE)
  
  # Remove the third row (which is the blank row)
  data <- data[-3,]
  
  # Extract the first row as column names
  commune_ids <- as.character(data[1,])
  variable_names <- as.character(data[2,])
  col_names <- paste(commune_ids, variable_names, sep = "__")
  
  # Set the column names of the data
  colnames(data) <- col_names
  colnames(data)[1] <- 'date'
  
  # Remove the first two rows which were used for column names
  data <- data[-c(1, 2),]
  
  # Convert the data to long format using pivot_longer
  tidy_data <- pivot_longer(
    data,
    cols = -date,
    names_to = c("commune_id", "variable_name"),
    names_sep = "__"
  ) %>%
    mutate(monthdate = lubridate::floor_date(as.Date(date), 'month'),
           value = as.numeric(value)) %>%
    arrange(commune_id, variable_name, monthdate) %>%
    group_by(monthdate, commune_id, variable_name) %>%
    summarize(mean_value = mean(value, na.rm = T),.groups = "drop")
}