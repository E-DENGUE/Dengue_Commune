inla_spacetime_mod <- function(vintage_date, modN, formula1='y ~ -1 +  X +   f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'){
  
  set.seed(8123)  # Global R seed for reproducibility
  
  
  # c1 <- d2 %>%
  #   filter( date>='2010-01-01')%>%
  #   left_join(spat_IDS, by='fcode') %>%
  #   arrange(fcode, date) %>%
  #   mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
  #   group_by(fcode) %>%
  #   mutate(fcode2=fcode,
  #          Dengue_fever_rates = obs_dengue_cases / pop_total * 100000,
  #          log_df_rate = log((obs_dengue_cases + 1) / pop_total * 100000),
  #          log_pop_total = log(pop_total / 100000),
  #          year = lubridate::year(date),
  #          obs_dengue_cases_hold = ifelse(date > vintage_date, NA_real_, obs_dengue_cases),
  #          lag_y = lag(log_df_rate, 1),
  #          lag2_y = lag(log_df_rate, 2),
  #          lag3_y = lag(log_df_rate, 3),
  #          max_allowed_lag = ifelse(any(grepl('lag_y', formula1) | grepl('lag1', formula1)), 1, 3),
  #          horizon = ifelse(date == (vintage_date %m+% months(1)), 1,
  #                           ifelse(date == (vintage_date %m+% months(2)), 2,
  #                           ifelse(date == (vintage_date %m+% months(3)), 3, 0)
  #          )),
  #          sin12 = sin(2*pi*t/12),
  #          cos12 = cos(2*pi*t/12),
  #          month=as.factor(month(date)),
  #          monthN=month(date),
  #          offset1 = pop_total/100000,
  #          #log_offset=log(pop_total/100000)
  #   ) %>%
  #   filter(date<= (vintage_date %m+% months(3) ) & !is.na(lag3_y) & horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
  #   ungroup() %>%
  #   mutate(
  #     fcodeID2 = fcodeID,
  #     fcodeID3 = fcodeID,
  #     fcodeID4 = fcodeID,
  #     t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
  #     
  #     time_id1= t , 
  #     time_id2=t,
  #     time_id3= t,
  #     time_id4= t) %>%
  #   arrange(date,fcodeID) %>% #SORT FOR SPACE_TIME
  #   mutate(fcodeIDpad=str_pad(fcodeID, 3, pad = "0", side='left'),
  #          timeIDpad=str_pad(time_id1, 5, pad = "0", side='left'),
  #         # Population_density= scale(pop_density),
  #   )
  # 
  
  c1 <- d2 %>%
    filter(date >= as.Date("2010-01-01")) %>%
    left_join(spat_IDS, by = "fcode") %>%
    arrange(fcode, date) %>%
    mutate(t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
    group_by(fcode) %>%
    mutate(
      fcode2 = fcode,
      
      Dengue_fever_rates = obs_dengue_cases / pop_total * 100000,
      log_df_rate = log((obs_dengue_cases + 1) / pop_total * 100000),
      log_pop_total = log(pop_total / 100000),
      
      year = lubridate::year(date),
      yearN = year - min(year, na.rm = TRUE) + 1,
      
      obs_dengue_cases_hold = ifelse(date > vintage_date, NA_real_, obs_dengue_cases),
      
      lag_y  = lag(log_df_rate, 1),
      lag2_y = lag(log_df_rate, 2),
      lag3_y = lag(log_df_rate, 3),
      
      # harmonics
      sin12 = sin(2*pi*t/12),
      cos12 = cos(2*pi*t/12),
      sin6  = sin(2*pi*t/6),
      cos6  = cos(2*pi*t/6),
      sin4  = sin(2*pi*t/4),
      cos4  = cos(2*pi*t/4),
      
      # thermal suitability (NO clipping)
     # max_temp_c = t2m_max - 273.15,
    #  min_temp_c = t2m_min - 273.15,
     # optimal_temp = if_else(max_temp_c <= 32 & min_temp_c >= 24, 1, 0),
      
      #f_min = exp(-((pmax(0, 26 - min_temp_c))^2) / (2 * 2^2)),
      #f_max = exp(-((pmax(0, max_temp_c - 30))^2) / (2 * 2^2)),
      #thermal_suitability = f_min * f_max,
      #thermal_suitability_lag3 = lag(thermal_suitability, 3),
      #logit_thermal_suitability_lag3 =
       # log(thermal_suitability_lag3 / (1 - thermal_suitability_lag3)),
      
      #lag3_log_cases = lag(log(obs_dengue_cases + 1), 3),
      
      # horizon logic (your existing)
      max_allowed_lag = ifelse(any(grepl("lag_y", formula1) | grepl("lag1", formula1)), 1, 3),
      horizon = ifelse(date == (vintage_date %m+% months(1)), 1,
                       ifelse(date == (vintage_date %m+% months(2)), 2,
                              ifelse(date == (vintage_date %m+% months(3)), 3, 0))),
      
      month  = as.factor(month(date)),
      monthN = month(date),
      offset1 = pop_total / 100000
    ) %>%
    filter(date <= (vintage_date %m+% months(3)) &
             !is.na(lag3_y) &
             horizon <= max_allowed_lag) %>%
    ungroup() %>%
    mutate(
      
      #climate_scale_lag3 = scale(thermal_suitability_lag3),
      
      
      fcodeID2 = fcodeID,
      fcodeID3 = fcodeID,
      fcodeID4 = fcodeID,
      
      
      fcode0 = as.numeric(as.factor(fcode)),
      fcode1 = fcode0,
      fcode2 = fcode0,
      fcode3 = fcode0,
      fcode4 = fcode0,
      fcode5 = fcode0,
      fcode6 = fcode0,
      fcode7 = fcode0,
      
      yearID1 = yearN,
      yearID2 = yearN,
      yearID3 = yearN,
      yearID4 = yearN,
      yearID5 = yearN,
      yearID6 = yearN,
      
      year_fcode_ID1 = as.numeric(as.factor(paste(fcode0, yearN))),
      year_fcode_ID2 = year_fcode_ID1,
      year_fcode_ID3 = year_fcode_ID1,
      year_fcode_ID4 = year_fcode_ID1,
      year_fcode_ID5 = year_fcode_ID1,
      year_fcode_ID6 = year_fcode_ID1,
      
      
      t = t - min(t, na.rm = TRUE) + 1,
      time_id1 = t,
      time_id2 = t,
      time_id3 = t,
      time_id4 = t
    ) %>%
    arrange(date, fcodeID) %>%
    mutate(
      fcodeIDpad = str_pad(fcodeID, 3, pad = "0", side = "left"),
      timeIDpad  = str_pad(time_id1, 5, pad = "0", side = "left")
    )
  
  c1$fcodeID <- as.numeric(c1$fcodeID)

  
  form2 <- as.formula (formula1)
  
  # form2 <- as.formula(y ~ f(t, group = fcodeID2, model = "ar1", 
  # hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
  #    2)))))    
  
  
  #nbinomial or poisson
  offset1 <- c1$offset1
  
  mod1 <- inla(form2, data = c1,  family = "poisson",E=offset1,
               control.compute = list(dic = FALSE, 
                                      waic = FALSE, 
                                      config = T,
                                      return.marginals=F
               ),
               # save predicted values on response scale
               control.predictor = list(compute=TRUE, link=1),
               control.inla = list(strategy='adaptive', # adaptive gaussian
                                   cmin=0),
               control.fixed = list(mean.intercept=0, 
                                    prec.intercept=0.04, # precision 1
                                    mean=0, 
                                    prec=1), # weakly regularising on fixed effects (sd of 1)
               inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
               num.threads=8
  )    
  
  mod.family <- mod1$.args$family
  
  
  c1 <- c1 %>%
    ungroup() %>%
    mutate(forecast= as.factor(if_else(is.na(obs_dengue_cases_hold),1,0)),
           horizon = if_else(date== (vintage_date %m+% months(1)),1,
                             if_else(date== (vintage_date %m+% months(2)),2,
                             if_else(date== (vintage_date %m+% months(3)),3, 0
                             )
           )),
           max_allowed_lag = if_else(grepl('lag_y',formula1 ),1,3)
    )%>%
    filter(horizon <= max_allowed_lag)
  #View(c1 %>% dplyr::select(fcode, date,obs_dengue_cases_hold,Dengue_fever_rates,log_df_rate,lag_y,lag2_y, forecast, horizon)  %>% filter(date>=as.Date('2012-01-01')))
  
  
  score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)
  
  scores <- scoring_func(score.list)
  

  
  c1.out <- c1 %>%
    dplyr::select(date, fcode, obs_dengue_cases, forecast,horizon ) 
  
  out.list =  list ('ds'=c1.out, 'scores'=scores$crps3,'log.samps.inc'=scores$log.samps.inc,  'fixed.eff'=mod1$summary.fixed, 'waic'=mod1$waic,'form'=formula1)
  saveRDS(out.list,paste0('./Output/Results/Results_spacetime/', 'mod',modN,'_',vintage_date  ,'.rds' )   )
  return(out.list)
}
