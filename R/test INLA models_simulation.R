source("./R/99_load.R")


set.seed(8123)  # Global R seed for reproducibility

vintage_date <- as.Date('2023-10-01')

c1 <- d2 %>%
  filter( date>='2004-09-01')%>%
  arrange(fcode, date) %>%
  mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
  group_by(fcode) %>%
  mutate(fcode2=fcode,
         fcodeID = fcode,
         Dengue_fever_rates = obs_dengue_cases / pop_total * 100000,
         log_df_rate = log((obs_dengue_cases + 1) / pop_total * 100000),
         log_pop_total = log(pop_total / 100000),
         year = lubridate::year(date),
         obs_dengue_cases_hold = ifelse(date > vintage_date, NA_real_, obs_dengue_cases),
         lag_y = lag(log_df_rate, 1),
         lag2_y = lag(log_df_rate, 2),
         lag3_y = lag(log_df_rate, 3),
         t2m_avg
         max_allowed_lag = 3,
         horizon = ifelse(date == (vintage_date %m+% months(1)), 1,
                          ifelse(date == (vintage_date %m+% months(2)), 2,
                                 ifelse(date == (vintage_date %m+% months(3)), 3, 0)
                          )),
         sin12 = sin(2*pi*t/12),
         cos12 = cos(2*pi*t/12),
         month=as.factor(month(date)),
         monthN=month(date),
         offset1 = pop_total/100000,
         #log_offset=log(pop_total/100000)
  ) %>%
  filter(date<= (vintage_date %m+% months(3) ) & !is.na(lag3_y) & horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
  ungroup() %>%
  mutate(
    fcodeID2 = fcodeID,
    fcodeID3 = fcodeID,
    fcodeID4 = fcodeID,
    t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
    
    time_id1= t , 
    time_id2=t,
    time_id3= t,
    time_id4= t) %>%
  arrange(date,fcodeID) %>% #SORT FOR SPACE_TIME
  mutate(fcodeIDpad=str_pad(fcodeID, 3, pad = "0", side='left'),
         timeIDpad=str_pad(time_id1, 5, pad = "0", side='left')
  )

c1$fcodeID<- as.numeric(c1$fcodeID)


form2 <- as.formula(
                'obs_dengue_cases_hold~   lag3_y + 
                       #  f(fcodeID, model="iid") +
                 f(fcodeID,
                        model="bym2",
                        constr= TRUE,
                        graph=MDR.adj,
                         hyper = hyper.besag ,
                        scale.model = TRUE) +
                     lag3_avg_min_daily_temp + 
                     lag3_monthly_cum_ppt +
                    f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes

                    #  f(t2, replicate=fcodeID2, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
                     sin12 + cos12
                '
)

c1_subset <- c1 %>%
  mutate(t2=t
         ) %>%
  filter(date >= '2012-01-01')
  
offset1 <- c1_subset$offset1

mod1 <- inla(form2, data = c1_subset,  family = "zeroinflatedpoisson0",E=offset1,
             control.compute = list(dic = FALSE, 
                                    waic = FALSE, 
                                    config = T,
                                    return.marginals=F
             ),
             # save predicted values on response scale
             control.predictor = list(compute=TRUE, link=1),
             control.inla = list(strategy = 'simplified.laplace', cmin = 0.01),
             control.fixed = list(mean.intercept=0, 
                                  prec.intercept=1, # precision 1
                                  mean=0, 
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=4
)    

summary(mod1)

#no random effects:0.442 second
#+spatial intercept 3 seconds
#++AR1 Random effect (non varying) :9 (8.8 with simplified laplace)--22 seconds if use all data
#++AR1 random effect  spatially varying ): 76 minutes; kills the temp effect totally
#+BYM2 intercept but no AR1 27 seconds
#++same but scale covariates: 29 seconds
#+
#+
#+


c1_agg <- c1 %>%
  mutate(t2=t
  ) %>%
  group_by(t) %>%
  summarize(lag3_avg_min_daily_temp  = mean(lag3_avg_min_daily_temp),
            lag3_monthly_cum_ppt = mean(lag3_monthly_cum_ppt ),
            obs_dengue_cases_hold  = sum(obs_dengue_cases_hold ),
            sin12 = mean(sin12),
            cos12=mean(cos12),
            offset1 = sum(offset1)
            )%>%
  ungroup() %>%
  arrange(t) %>%
  mutate(log_lag3_cases = log(lag(obs_dengue_cases_hold,3) +1))
            

offset1 <- c1_agg$offset1

form2 <- as.formula('obs_dengue_cases_hold~
                        log_lag3_cases+
                lag3_avg_min_daily_temp +
                      lag3_monthly_cum_ppt + 
                      sin12 + cos12 '  #shared AR(1) across fcodes
)
mod1 <- inla( form2  , data = c1_agg,  family = "poisson",E=offset1,
             control.compute = list(dic = FALSE, 
                                    waic = FALSE, 
                                    config = T,
                                    return.marginals=F
             ),
             # save predicted values on response scale
             control.predictor = list(compute=TRUE, link=1),
             control.inla = list(strategy = 'simplified.laplace', cmin = 0.01),
             control.fixed = list(mean.intercept=0, 
                                  prec.intercept=1, # precision 1
                                  mean=0, 
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=4
)    
summary(mod1)
