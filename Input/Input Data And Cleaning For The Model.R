library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(spdep)
library(sp)
library(MASS)
library(surveillance)
library(tidyr)
library(splines)
library(tidyverse)
library(tsibble)
library(sf)
library(scales)
library(RColorBrewer)
library(viridis)
library(roll)

# parent_dir <- "./Input/model_input_data"
# 
# folders <- list.dirs(path = parent_dir, recursive = FALSE)
# 
# all_data <- list()
# 
# for (folder in folders) {
#   csv_files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
#   
#   folder_data <- lapply(csv_files, read_csv)
#   
#   if (length(folder_data) > 0) {
#     all_data <- c(all_data, folder_data)
#   }
# }
# d1 <- bind_rows(all_data)

d1<- read.csv("./Input/model_input_data/model_input_data_mdr_ytkv.csv")

d1$date <- as.Date(paste(d1$year, sprintf("%02d", d1$month), "01", sep = "-"))

names(d1)[names(d1) == "t2m_avg"] <- "avg_daily_temp"
names(d1)[names(d1) == "t2m_max"] <- "avg_max_daily_temp"
names(d1)[names(d1) == "t2m_min" ] <- "avg_min_daily_temp"
names(d1)[names(d1) == "ws_avg"] <- "avg_daily_wind"
names(d1)[names(d1) == "ws_max"] <-  "avg_max_daily_wind"
names(d1)[names(d1) ==  "ws_min"] <- "avg_min_daily_wind"
names(d1)[names(d1) == "rh_avg"] <- "avg_daily_humid"
names(d1)[names(d1) == "rh_max" ] <- "avg_max_daily_humid"
names(d1)[names(d1) == "rh_min"  ] <- "avg_min_daily_humid"
names(d1)[names(d1) == "tp_accum"  ] <- "monthly_cum_ppt"
names(d1)[names(d1) == "population"  ] <- "pop_total"
#names(d1)[names(d1) == "District"  ] <- "district"
#names(d1)[names(d1) == "Province"  ] <- "province"
names(d1)[names(d1) == "Month"  ] <- "month"
names(d1)[names(d1) == "Year"  ] <- "year"
##remove the districts without neighborhood 

d2 <- d1 %>%
  mutate(
    year = as_factor(year)) %>%
  filter(fcode != "TTYTKV_KIEN_HAI_KG",
         fcode != "TTYTKV_PHU_QUOC_KG") %>%
  distinct(year, month, fcode, .keep_all = T) %>%
  arrange(month, year)%>%
  ungroup() %>%
  arrange(fcode, year, month) %>%
  group_by(fcode) %>%
  mutate(date= as.Date(paste(year,month, '01',sep='-'), '%Y-%m-%d'),
         obs_dengue_cases =ifelse(!is.na(pop_total) & is.na( obs_dengue_cases),0,  obs_dengue_cases ) ,
         first_date=min(date),
         last_date =max(date),
  ) %>%
  ungroup() %>%
  filter(!is.na(fcode) &first_date==as.Date( first_date) & last_date== last_date )   

##add 3 more rows for each district after 2025 year

d3 <- d2 %>%
  arrange(fcode, year, month)

last_row <- d3 %>%
  arrange(desc(year), desc(month)) %>%
  dplyr::slice(1)

last_year <- as.numeric(as.character(last_row$year))
last_month <- last_row$month
last_pop <- last_row$pop_total

unique_fcodes <- unique(d1$fcode)

# Generate new rows for the next 3 months
new_rows <- bind_rows(lapply(1:3, function(i) {
  next_month <- last_month + i
  next_year <- last_year
  
  # If next month exceeds December (12), roll over to the next year
  if (next_month > 12) {
    next_month <- next_month - 12
    next_year <- last_year + 1
  }
  
  tibble(
    fcode = unique_fcodes,
    year = next_year,
    month = next_month,
    date = as.Date(paste(next_year, next_month, "01", sep = "-")),
    pop_total = last_pop
  )
}))



# 1. For each fcode, get its last observed year, month, and population
last_obs <- d3 %>%
  group_by(fcode) %>%
  arrange(year, month) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    year  = as.integer(as.character(year)),
    month = as.integer(as.character(month))
  ) %>%
  dplyr::select(fcode, year, month, pop_total)

# 2. Define how many months ahead you want
n_months <- 3
offsets  <- tibble(offset = 1:n_months)

# 3. Cross‐join and compute new rows
new_rows <- last_obs %>%
  tidyr::crossing(offsets) %>%
  mutate(
    tmp_month = month + offset,
    year      = year + (tmp_month - 1) %/% 12,
    month     = (tmp_month - 1) %% 12 + 1,
    date      = make_date(year, month, 1)
  ) %>%
  dplyr::select(fcode, year, month, date, pop_total)

#new_rows


# new_rows now has the correct last_pop per fcode for the next 3 months


d3$year <- as.numeric(as.character(d3$year))

d3 <- bind_rows(d3, new_rows) %>%
  arrange(fcode, year, month)

tail(d3)
###Do scaling and lags
d3 <- d3 %>%
  mutate(
    avg_daily_wind_scale = as.vector(scale(avg_daily_wind)),
    avg_daily_humid_scale = as.vector(scale(avg_daily_humid)),
    avg_daily_temp_scale = as.vector(scale(avg_daily_temp)),
    monthly_cum_ppt_scale = as.vector(scale(monthly_cum_ppt)),
    avg_min_daily_temp_scale = as.vector(scale(avg_min_daily_temp)),
    avg_max_daily_temp_scale = as.vector(scale(avg_max_daily_temp)),
    max_temp_c = avg_max_daily_temp -273.15,
    min_temp_c = avg_min_daily_temp - 273.15,
    optimal_temp = if_else(max_temp_c<=32 & min_temp_c>=24,1,0),
    f_min = exp(-((pmax(0, 26 - min_temp_c))^2) / (2 * 2^2)),
    f_max = exp(-((pmax(0, max_temp_c - 30))^2) / (2 * 2^2)),
    thermal_suitability  = f_min * f_max,
    ) %>%
  arrange(fcode, year, month) %>%
  group_by(fcode) %>%
  mutate(
    lag1_avg_daily_wind = dplyr::lag(avg_daily_wind_scale, 1, default = NA),
    lag2_avg_daily_wind = dplyr::lag(avg_daily_wind_scale, 2, default = NA),
    lag3_avg_daily_wind = dplyr::lag(avg_daily_wind_scale, 3, default = NA),
    
    lag1_avg_daily_humid = dplyr::lag(avg_daily_humid_scale, 1, default = NA),
    lag2_avg_daily_humid = dplyr::lag(avg_daily_humid_scale, 2, default = NA),
    lag3_avg_daily_humid = dplyr::lag(avg_daily_humid_scale, 3, default = NA),
    
    lag1_avg_daily_temp = dplyr::lag(avg_daily_temp_scale, 1),
    lag2_avg_daily_temp = dplyr::lag(avg_daily_temp_scale, 2),
    lag3_avg_daily_temp = dplyr::lag(avg_daily_temp_scale, 3),
    
    lag1_monthly_cum_ppt = dplyr::lag(monthly_cum_ppt_scale, 1),
    lag2_monthly_cum_ppt = dplyr::lag(monthly_cum_ppt_scale, 2),
    lag3_monthly_cum_ppt = dplyr::lag(monthly_cum_ppt_scale, 3),
    
    lag1_avg_min_daily_temp = dplyr::lag(avg_min_daily_temp_scale, 1),
    lag2_avg_min_daily_temp = dplyr::lag(avg_min_daily_temp_scale, 2),
    lag3_avg_min_daily_temp = dplyr::lag(avg_min_daily_temp_scale, 3),
    
    lag1_avg_max_daily_temp = dplyr::lag(avg_max_daily_temp_scale, 1),
    lag2_avg_max_daily_temp = dplyr::lag(avg_max_daily_temp_scale, 2),
    lag3_avg_max_daily_temp = dplyr::lag(avg_max_daily_temp_scale, 3),
    thermal_suitability_lag3 = lag(thermal_suitability,3),
    logit_thermal_suitability_lag3 = log(thermal_suitability_lag3/(1-thermal_suitability_lag3)),
  ) %>%
  ungroup() %>% mutate(
    climate_scale_lag3 = as.numeric(scale(thermal_suitability_lag3))
  )%>%
  arrange(fcode, date) %>%
  group_by(fcode) %>%
  mutate(
    cumsum_cases_12m = roll::roll_sum(obs_dengue_cases, 12, min_obs = 1),
    cumsum_pop_12m = roll::roll_sum(pop_total, 12, min_obs = 1),
    cum_inc_12m = (cumsum_cases_12m + 1) / cumsum_pop_12m * 100000,
    cumsum_cases_24m = roll::roll_sum(obs_dengue_cases, 24, min_obs = 1),
    cumsum_pop_24m = roll::roll_sum(pop_total, 24, min_obs = 1),
    cum_inc_24m = (cumsum_cases_24m + 1) / cumsum_pop_24m * 100000,
    cumsum_cases_36m = roll::roll_sum(obs_dengue_cases, 36, min_obs = 1),
    cumsum_pop_36m = roll::roll_sum(pop_total, 36, min_obs = 1),
    cum_inc_36m = (cumsum_cases_36m + 1) / cumsum_pop_36m * 100000
  ) %>%
  ungroup() %>%
  arrange(fcode, date) %>%
  group_by(fcode) %>%
  mutate(
    log_cum_inc_12m = scale(log(cum_inc_12m)),
    log_cum_inc_24m = scale(log(cum_inc_24m)),
    log_cum_inc_36m = scale(log(cum_inc_36m)),
    
    lag2_log_cum_inc_12m = lag(log_cum_inc_12m, 2),
    lag2_log_cum_inc_24m = lag(log_cum_inc_24m, 2),
    lag2_log_cum_inc_36m = lag(log_cum_inc_36m, 2),
    
    lag3_log_cum_inc_12m = lag(log_cum_inc_12m, 3),
    lag3_log_cum_inc_24m = lag(log_cum_inc_24m, 3),
    lag3_log_cum_inc_36m = lag(log_cum_inc_36m, 3)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag3_monthly_cum_ppt) )


names(d3)
dim(d3)
tail(d3$lag3_log_cum_inc_24m)


q <- d3 %>% filter(fcode=="TTYTKV_BEN_LUC_LA") %>% arrange(date)

plot(q$min_temp_c, q$f_min, pch=16,
     xlab="Min temp (°C)", ylab="f_min")
abline(v=26, lty=2)

plot(q$max_temp_c, q$f_max, pch=16,
     xlab="Max temp (°C)", ylab="f_max")
abline(v=30, lty=2)



rename_map <- c(
  "avg_daily_temp"    = "t2m_avg",
  "avg_max_daily_temp"= "t2m_max",
  "avg_min_daily_temp"= "t2m_min",
  "avg_daily_wind"    = "ws_avg",
  "avg_max_daily_wind"= "ws_max",
  "avg_min_daily_wind"= "ws_min",
  "avg_daily_humid"   = "rh_avg",
  "avg_max_daily_humid"= "rh_max",
  "avg_min_daily_humid"= "rh_min",
  "monthly_cum_ppt"   = "tp_accum"
)

names(d3) <- ifelse(names(d3) %in% names(rename_map), rename_map[names(d3)], names(d3))

# Print updated column names
print(names(d3))
d3<- d3[,-c(20:21)]
saveRDS(d3, './Model/Data/Full_data_set_with_covariates_and_lags.rds')
write.csv(d3, './Model/Data/Full_data_set_with_covariates_and_lags.csv')

##################################################################################################################
#SPATIAL MATRIX:
library(stringr)
d3<- read.csv( './Model/Data/Full_data_set_with_covariates_and_lags.csv')
MDR_NEW <- sf::st_read(dsn = "./Input/shapefiles/ttytkv2025.shp") 


unmatched_d3 <- setdiff(d3$fcode, MDR_NEW$fcode)
unmatched_MDR <- setdiff(MDR_NEW$fcode, d3$fcode)


setdiff(d3$fcode,MDR_NEW$fcode)
setdiff(MDR_NEW$fcode,d3$fcode)

MDR_NEW <- MDR_NEW %>%
  dplyr::filter(fcode != "TTYTKV_KIEN_HAI_KG",
                fcode != "TTYTKV_PHU_QUOC_KG")

# Remove districts (no neighbors) from the dataset
spat_IDS <- MDR_NEW %>%
  dplyr::filter(fcode != "TTYTKV_KIEN_HAI_KG",
                fcode != "TTYTKV_PHU_QUOC_KG") %>%
  mutate(fcodeID= row_number(),fcode=(fcode)) %>%
  as.data.frame() %>%
  dplyr::select(fcode,fcodeID)


saveRDS(MDR_NEW, "./Model/Data/MDR_NEW.rds")
saveRDS(spat_IDS, "./Model/Data/spatial_IDS.rds")

