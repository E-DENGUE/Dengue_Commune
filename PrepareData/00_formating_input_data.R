library(parallel)
library(stats)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(pbapply)
library(INLA)
#inla.setOption(mkl=TRUE)
library(MASS)
library(scoringutils)
library(sf)
library(spdep)
library(ggmap) # plotting shapefiles 
library(lattice)  # Load the lattice package if you are using lattice graphics
library(stringr)
library(raster)
library(Hmisc)
library(ggdendro)
library(dtwclust)
library(sf)
library(cluster)
library(dplyr)
source('./R/predict.rlm.R')
source('./R/deseasonalize_climate.R')
#source('./R/all_district_fwd2.R')
#source('./R/scoring_func.R')

######## Load data
d1 <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries_all_factors.rds')


names(d1)[names(d1) == "Dengue"] <- "m_DHF_cases"
names(d1)[names(d1) == "total_population"] <- "pop"
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
names(d1)[names(d1) == "District"  ] <- "district"
names(d1)[names(d1) == "Province"  ] <- "province"
names(d1)[names(d1) == "Month"  ] <- "month"
names(d1)[names(d1) == "Year"  ] <- "year"

# Define the renaming condition for all rows
d1 <- d1 %>%
  mutate(district = ifelse(district == 'CHAU THANH' & province == "AN GIANG", 
                           "CHAU THANH AN GIANG", 
                           ifelse(district == 'CHAU THANH' & province == "BEN TRE", 
                                  "CHAU THANH BEN TRE", 
                                  ifelse(district == 'CHAU THANH' & province == "CA MAU", 
                                         "CHAU THANH CA MAU",
                                         ifelse(district == 'CHAU THANH' & province == "DONG THAP", 
                                                "CHAU THANH DONG THAP",
                                                ifelse(district == 'CHAU THANH' & province == "HAU GIANG", 
                                                       "CHAU THANH HAU GIANG",
                                                       ifelse(district == 'CHAU THANH' & province == "LONG AN", 
                                                              "CHAU THANH LONG AN",
                                                              ifelse(district == 'CHAU THANH' & province == "TIEN GIANG", 
                                                                     "CHAU THANH TIEN GIANG",
                                                                     ifelse(district == 'CHAU THANH' & province == "TRA VINH", 
                                                                            "CHAU THANH TRA VINH",
                                                                            ifelse(district == 'PHU TAN' & province == "CA MAU", 
                                                                                   "PHU TAN CA MAU",
                                                                                   ifelse(district == 'PHU TAN' & province == "AN GIANG", 
                                                                                          "PHU TAN AN GIANG",
                                                                                          as.character(district)
                                                                                   )
                                                                            )
                                                                     )))))))))

d1 <- d1 %>%
  mutate(date = paste(year, month, '01', sept='-'),
         year = as_factor(year)) %>%
  filter(district != "KIEN HAI", 
         district != "PHU QUOC") %>%
  distinct(year, month, province, district, .keep_all = T) 


d2 <- d1 %>%  arrange(month, year)%>%
  ungroup() %>%
  dplyr::select(year, month,province, district,m_DHF_cases,pop,avg_daily_temp,avg_max_daily_temp,avg_min_daily_temp,avg_daily_wind,avg_max_daily_wind,
                avg_min_daily_wind,avg_daily_humid,avg_max_daily_humid,avg_min_daily_humid,monthly_cum_ppt,
                Population_density,Outmigration_Rate, No..DEN1,No..DEN2,No..DEN3,No..DEN4,              
                Inmigration_Rate,NetImmigration_Rate,BI_larvae,CI_larvae ,HI_larvae,DI, prov_dis,         
                Poverty_Rate,   Hygienic_Water_Access, breeding_site_elimination_campaign,          
                Monthly_Average_Income_Percapita,Total_Passenger, communication_or_training,number_of_outbreak_response,               
                Hygienic_Toilet_Access, Urbanization_Rate,large_scale_spraying_for_epidemic_response  , land.scan.population ,number_of_outbreak_detection,active_spraying
  ) %>%
  ungroup() %>%
  arrange(province,district, year, month) %>%
  group_by(district) %>%
  mutate(date= as.Date(paste(year,month, '01',sep='-'), '%Y-%m-%d'),
         m_DHF_cases =ifelse(!is.na(pop) & is.na(m_DHF_cases),0, m_DHF_cases ) ,
         first_date=min(date),
         last_date =max(date),
  ) %>%
  ungroup() %>%
  filter(!is.na(district) &first_date==as.Date('2004-01-01') & last_date=='2022-12-01')   #filter out regions with partial time series

rain1 <- deseasonalize_climate("monthly_cum_ppt") %>% rename(total_rainfall_ab = climate_aberration)

temp1 <- deseasonalize_climate("avg_daily_temp" )  %>% rename( ave_temp_ab = climate_aberration)
temp2 <- deseasonalize_climate("avg_min_daily_temp")  %>% rename( max_temp_ab = climate_aberration)
temp3 <- deseasonalize_climate("avg_max_daily_temp" )  %>% rename( min_ave_temp_ab = climate_aberration)

humid1 <- deseasonalize_climate("avg_daily_humid")  %>% rename(ave_humid_ab = climate_aberration)
humid2 <- deseasonalize_climate("avg_min_daily_humid")  %>% rename(min_humid_abb = climate_aberration)
humid3 <- deseasonalize_climate("avg_max_daily_humid")  %>% rename(max_humid_abb = climate_aberration)

wind1 <- deseasonalize_climate("avg_daily_wind")  %>% rename( ave_wind_ab = climate_aberration)
wind2 <- deseasonalize_climate("avg_min_daily_wind")  %>% rename( min_wind_ab = climate_aberration)
wind3 <- deseasonalize_climate("avg_max_daily_wind")  %>% rename( max_wind_ab = climate_aberration)

d3 <- d2 %>%
  left_join(rain1, by=c('district', 'date')) %>%
  left_join(temp1, by=c('district', 'date')) %>%
  left_join(temp2, by=c('district', 'date')) %>%
  left_join(temp3, by=c('district', 'date')) %>%
  left_join(humid1, by=c('district', 'date')) %>%
  left_join(humid2, by=c('district', 'date')) %>%
  left_join(humid3, by=c('district', 'date')) %>%
  left_join(wind1, by=c('district', 'date')) %>%
  left_join(wind2, by=c('district', 'date')) %>%
  left_join(wind3, by=c('district', 'date')) %>%   
  mutate(
    avg_daily_wind_scale = as.vector(scale(avg_daily_wind)),
    avg_daily_humid_scale = as.vector(scale(avg_daily_humid)),
    avg_daily_temp_scale = as.vector(scale(avg_daily_temp)),  
    monthly_cum_ppt_scale = as.vector(scale(monthly_cum_ppt)),
    avg_min_daily_temp_scale = as.vector(scale(avg_min_daily_temp)), 
    avg_max_daily_temp_scale = as.vector(scale(avg_max_daily_temp)),
    total_rainfall_ab_scale = as.vector(scale(total_rainfall_ab)),
    breeding_site_elimination_campaign_scale = as.vector(scale(breeding_site_elimination_campaign)),
    active_spraying_scale = as.vector(scale(active_spraying)),
    large_scale_spraying_for_epidemic_response_scale = as.vector(scale(large_scale_spraying_for_epidemic_response)),
    communication_or_training_scale = as.vector(scale(communication_or_training)),
    number_of_outbreak_detection_scale = as.vector(scale(number_of_outbreak_detection)),
    number_of_outbreak_response_scale = as.vector(scale(number_of_outbreak_response))
  ) %>%
  
  arrange(province, district, year, month) %>%
  
  # Group by district before applying lags (no scaling inside the group_by)
  group_by(district) %>%
  mutate(
    lag1_avg_daily_wind = dplyr::lag(avg_daily_wind_scale, 1),
    lag2_avg_daily_wind = dplyr::lag(avg_daily_wind_scale, 2),
    lag3_avg_daily_wind = dplyr::lag(avg_daily_wind_scale, 3),
    
    lag1_avg_daily_humid = dplyr::lag(avg_daily_humid_scale, 1),
    lag2_avg_daily_humid = dplyr::lag(avg_daily_humid_scale, 2),
    lag3_avg_daily_humid = dplyr::lag(avg_daily_humid_scale, 3),
    
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
    
    lag1_total_rainfall_ab = dplyr::lag(total_rainfall_ab_scale, 1),
    lag2_total_rainfall_ab = dplyr::lag(total_rainfall_ab_scale, 2),
    lag3_total_rainfall_ab = dplyr::lag(total_rainfall_ab_scale, 3),
    
    lag2_breeding_site_elimination_campaign = dplyr::lag(breeding_site_elimination_campaign_scale, 2),	
    lag2_active_spraying = dplyr::lag(active_spraying_scale, 2),
    lag2_large_scale_spraying_for_epidemic_response = dplyr::lag(large_scale_spraying_for_epidemic_response_scale, 2),
    lag2_communication_or_training = dplyr::lag(communication_or_training_scale, 2),
    lag2_number_of_outbreak_detection = dplyr::lag(number_of_outbreak_detection_scale, 2),
    lag2_number_of_outbreak_response = dplyr::lag(number_of_outbreak_response_scale, 2),
    
    lag3_breeding_site_elimination_campaign = dplyr::lag(breeding_site_elimination_campaign_scale, 3),	
    lag3_active_spraying = dplyr::lag(active_spraying_scale, 3),
    lag3_large_scale_spraying_for_epidemic_response = dplyr::lag(large_scale_spraying_for_epidemic_response_scale, 3),
    lag3_communication_or_training = dplyr::lag(communication_or_training_scale, 3),
    lag3_number_of_outbreak_detection = dplyr::lag(number_of_outbreak_detection_scale, 3),
    lag3_number_of_outbreak_response = dplyr::lag(number_of_outbreak_response_scale, 3)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag3_monthly_cum_ppt) & first_date==as.Date('2004-01-01') & last_date=='2022-12-01') %>%
  arrange(district, province, year, month) %>^
  mutate(pop = as.numeric(pop),
         log_df_rate = log((m_DHF_cases +1 ) / d3$pop * 100000)
  )

saveRDS(d3, 'Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3.rds') 


###############################
#Create SPATIAL MATRIX:


MDR_NEW <- st_read(dsn = "./Data/shapefiles/MDR_NEW_Boundaries_Final.shp") 

# Create a new variable 'District_province' by concatenating 'VARNAME' and 'NAME_En' with an underscore
MDR_NEW <- MDR_NEW %>%
  dplyr::mutate(District_province = paste( VARNAME,NAME_En, sep = " "))

MDR_NEW$VARNAME<- toupper(MDR_NEW$VARNAME)
MDR_NEW$NAME_En<- toupper(MDR_NEW$NAME_En)

MDR_NEW  <- MDR_NEW  %>%
  mutate(VARNAME = ifelse(VARNAME == 'CHAU THANH' & NAME_En == "AN GIANG", 
                          "CHAU THANH AN GIANG", 
                          ifelse(VARNAME == 'CHAU THANH' & NAME_En == "BEN TRE", 
                                 "CHAU THANH BEN TRE", 
                                 ifelse(VARNAME == 'CHAU THANH' & NAME_En == "CA MAU", 
                                        "CHAU THANH CA MAU",
                                        ifelse(VARNAME == 'CHAU THANH' & NAME_En == "DONG THAP", 
                                               "CHAU THANH DONG THAP",
                                               ifelse(VARNAME == 'CHAU THANH' & NAME_En == "HAU GIANG", 
                                                      "CHAU THANH HAU GIANG",
                                                      ifelse(VARNAME == 'CHAU THANH' & NAME_En == "LONG AN", 
                                                             "CHAU THANH LONG AN",
                                                             ifelse(VARNAME == 'CHAU THANH' & NAME_En == "TIEN GIANG", 
                                                                    "CHAU THANH TIEN GIANG",
                                                                    ifelse(VARNAME == 'CHAU THANH' & NAME_En == "TRA VINH", 
                                                                           "CHAU THANH TRA VINH",
                                                                           ifelse(VARNAME == 'PHU TAN' & NAME_En == "CAM MAU", 
                                                                                  "PHU TAN CA MAU",
                                                                                  ifelse(VARNAME == 'PHU TAN' & NAME_En == "AN GIANG", 
                                                                                         "PHU TAN AN GIANG",
                                                                                         as.character(VARNAME)
                                                                                  )
                                                                           )
                                                                    )))))))))
# Remove island districts (no neighbors) from the dataset
spat_IDS <- MDR_NEW %>%
  dplyr::filter(VARNAME != "KIEN HAI",
                VARNAME != "PHU QUOC") %>%
  rename(district=VARNAME) %>%
  mutate(districtID= row_number(), district=(district)) %>%
  as.data.frame() %>%
  dplyr::select(district,districtID)

setdiff(toupper(d3$district),toupper(spat_IDS$district))
setdiff(toupper(spat_IDS$district),toupper(d3$district))
sort(spat_IDS$district) ==sort(unique(d3$district))

MDR_NEW<- MDR_NEW %>%
  dplyr::filter(VARNAME != "KIEN HAI",
                VARNAME != "PHU QUOC")

saveRDS(MDR_NEW, "./Data/MDR_NEW.rds")
saveRDS(spat_IDS, "./Data/spatial_IDS.rds")
#
