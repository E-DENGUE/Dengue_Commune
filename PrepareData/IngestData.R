library(INLA)
library(dplyr)
library(tidyverse)
library(sf)
library(roll)
library(spdep)

# Read in neighbors
g <- inla.read.graph("../Data/MDR.graph.commune")
nb_list <- g$nbs

#Provinces to include in analysis

province_codes <- c('BL','BT','CM','CT','HG','LA','KG','TG','TV','VL')

# Read in case data
a1 <- lapply(province_codes, function(X) readxl::read_excel('./Data/Dengue_observed_10_province/250905_ED_MONTHLY dengue case_10 provinces_2010-2024.xlsx', sheet=X)) %>%
  bind_rows() %>%
  rename(l2_code = l2_code_commune,
         obs_dengue_cases = dengue ) %>%
  mutate(date = as.Date(paste(year, month, '01', sep='-'))) 

# Which codes are present in dataset?
l2_code_keep <- a1 %>%
  pull(l2_code) %>%
  unique()

 source('./R/IngestMap.R' )

#lag3_avg_min_daily_temp, lag3_monthly_cum_ppt (cum_tp_accum)
temp_data <- vroom::vroom('./Data/meteorological.csv.gz') %>%
  dplyr::select(monthdate, commune_id, mean_t2m_min, cum_tp_accum, mean_dtr) %>%
  mutate(date = as.Date(monthdate)) %>%
  arrange(commune_id, date) %>%
  group_by(commune_id) %>%
  mutate(lag3_avg_min_daily_temp = lag(mean_t2m_min, 3),
         lag3_monthly_cum_ppt = lag(cum_tp_accum,3),
         lag3_monthly_dtr = lag(mean_dtr,3)
  ) %>%  # 3-month lag
  ungroup() %>%
  dplyr::select(date, commune_id, lag3_monthly_cum_ppt,lag3_avg_min_daily_temp,lag3_monthly_dtr) %>%
  mutate(commune_id = as.character(commune_id)) %>%
  rename(l2_code = commune_id) %>%
  full_join(id_mapping_key, by = c('l2_code' = 'l2_code')) %>%
  mutate(l2_code = as.numeric(l2_code)) %>%
  filter(date>='2010-01-01' &!is.na(fcode))

## read in pop file
pop <- st_read("./Data/Staging_shapefiles/mdr_boundary_level2_2025.geojson") %>%
  as.data.frame() %>%
  dplyr::select(l2_code,area, population) %>%
  mutate(l2_code = as.numeric(l2_code),
         pop_density = population/area/1000)

  
#Combine meterological data and case data

a2 <- a1 %>%
  left_join(temp_data, by=c('l2_code','date')
            ) %>%
  left_join(pop, by=c('l2_code')) %>%
  filter(!is.na(fcode) & date>='2010-01-01') %>%
  dplyr::select(date,fcode, l2_code,obs_dengue_cases ,lag3_avg_min_daily_temp,lag3_monthly_cum_ppt,lag3_monthly_dtr, population ) %>%
  arrange(fcode, date) %>%
  group_by(fcode) %>%
  mutate( 
    cumsum_cases_24m =  roll::roll_sum(obs_dengue_cases,24, min_obs=1), #partial backward moving sum
    cumsum_pop_24m =  roll::roll_sum(population,24, min_obs=1), #partial backward moving sum
    cum_inc_24m = (cumsum_cases_24m+1)/cumsum_pop_24m*100000,
    log_cum_inc_24m = log(cum_inc_24m),
    log_cum_inc_24m = (log_cum_inc_24m - mean(log_cum_inc_24m,na.rm=T))/sd(log_cum_inc_24m, na.rm=T),
    lag3_log_cum_inc_24m=lag(log_cum_inc_24m,3),
    
    log_lag12_inc = lag(
          log( 
            ((obs_dengue_cases+1)/population*100000)
      ), 
      12),
    log_lag12_inc = (log_lag12_inc - mean(log_lag12_inc, na.rm=T))/sd(log_lag12_inc, na.rm=T)
    ) %>%
  ungroup()

# Save results
vroom::vroom_write(a2, "../Data/case_data.csv.gz")


