library(INLA)
library(dplyr)
library(tidyverse)
library(sf)
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

#Read in temperature data
temp_data <- vroom::vroom('./Data/meteorological.csv.gz') %>%
  dplyr::select(monthdate, commune_id, t2m_avg) %>%
  mutate(date = as.Date(monthdate)) %>%
  arrange(commune_id, date) %>%
  group_by(commune_id) %>%
  mutate(temp3 = lag(t2m_avg, 3)) %>%  # 3-month lag
  ungroup() %>%
  dplyr::select(date, commune_id, temp3) %>%
  mutate(commune_id = as.character(commune_id)) %>%
  rename(l2_code = commune_id) %>%
  full_join(id_mapping_key, by = c('l2_code' = 'l2_code')) %>%
  mutate(l2_code = as.numeric(l2_code)) 

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
  left_join(pop, by='l2_code') %>%
  filter(!is.na(fcode) & date>='2010-01-01') %>%
  dplyr::select(date,fcode, l2_code,obs_dengue_cases ,temp3, population )

# Save results
vroom::vroom_write(a2, "../Data/case_data.csv.gz")


