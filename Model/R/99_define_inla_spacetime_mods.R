# 
# 
# mod1 <- 'obs_dengue_cases_hold ~ lag3_y+
#          f(t, model="ar1") + f(fcodeID, model="iid") +
#          f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'
# 
# 
# 
# 
# mod2<- 'obs_dengue_cases_hold~   lag3_y + 
#                             f(fcodeID,
#                                    model="besag",
#                                    constr= TRUE,
#                                    graph=MDR.adj,
#                                     hyper = hyper.besag ,
#                                    scale.model = TRUE) +
#                      lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
#                         f(t, replicate=fcodeID3, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
#                       f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'
# 
# mod3 <- 'obs_dengue_cases_hold~   lag3_y + log_cum_inc_12m +log_cum_inc_24m +log_cum_inc_36m +
#                             f(fcodeID,
#                                    model="besag",
#                                    constr= TRUE,
#                                    graph=MDR.adj,
#                                     hyper = hyper.besag ,
#                                    scale.model = TRUE) +
#                      lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
#                         f(t, replicate=fcodeID3, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
#                       f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'


############Make the seasonality simpler 
mod4 <- 'obs_dengue_cases_hold ~ lag3_y+
         f(t, model="ar1") + f(fcodeID, model="iid") +
         f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'




mod5 <- 'obs_dengue_cases_hold~   lag3_y + 
                            f(fcodeID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                    hyper = hyper.besag ,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=fcodeID3, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

mod6 <- 'obs_dengue_cases_hold~   lag3_y + log_cum_inc_12m +log_cum_inc_24m +log_cum_inc_36m +
                            f(fcodeID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                    hyper = hyper.besag ,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=fcodeID3, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'


##Reference model 
mod0 <- 'obs_dengue_cases_hold ~  f(fcodeID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'




# -----------------------------------------
# BYM2 spatial models (replace Besag with BYM2) and and thermal 
# -----------------------------------------

mod7 <- 'obs_dengue_cases_hold ~ lag3_y +
f(fcodeID, model="bym2", constr=TRUE, graph=MDR.adj, hyper=hyper.bym2, scale.model=TRUE) +
   climate_scale_lag3  + lag3_monthly_cum_ppt +
  f(t, replicate=fcodeID3, model="ar1", hyper=hyper.ar1, constr=TRUE) +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'
  
#Keep min-temp and add thermal suitability
mod8 <- 'obs_dengue_cases_hold ~ lag3_y +
  f(fcodeID, model="bym2", constr=TRUE, graph=MDR.adj, hyper=hyper.bym2, scale.model=TRUE) +
  lag3_avg_min_daily_temp +  climate_scale_lag3  + lag3_monthly_cum_ppt +
  f(t, replicate=fcodeID3, model="ar1", hyper=hyper.ar1, constr=TRUE) +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'


#Thermal + precipitation(similar to mod6)
mod9 <- 'obs_dengue_cases_hold ~ lag3_y + log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
  f(fcodeID, model="bym2", constr=TRUE, graph=MDR.adj, hyper=hyper.bym2, scale.model=TRUE) +
   climate_scale_lag3  + lag3_monthly_cum_ppt +
  f(t, replicate=fcodeID3, model="ar1", hyper=hyper.ar1, constr=TRUE) +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'


mod10 <- 'obs_dengue_cases_hold ~ lag3_y +
  f(fcodeID, model="bym2", constr=TRUE, graph=MDR.adj, hyper=hyper.bym2, scale.model=TRUE) +
  climate_scale_lag3  + lag3_monthly_cum_ppt +
  f(fcodeID2,  climate_scale_lag3 , model="iid") +
  f(t, replicate=fcodeID3, model="ar1", hyper=hyper.ar1, constr=TRUE) +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'


# ---------------------------------------------------------
#“flexible harmonics” family 
# ---------------------------------------------------------

mod11 <- '
  obs_dengue_cases_hold ~ 1 +
  lag3_y +
  sin12 + cos12 +
  f(fcode0, model="iid") +
  # District seasonality (annual + semi-annual)
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(fcode3, sin6,  model="iid") +
  f(fcode4, cos6,  model="iid") +
  f(fcode5, lag3_y, model="iid") +
  # Year seasonality (annual + semi-annual)
  f(yearID1, sin12, model="iid") +
  f(yearID2, cos12, model="iid") +
  f(yearID3, sin6,  model="iid") +
  f(yearID4, cos6,  model="iid") +
  # Year×district
  f(year_fcode_ID1, sin12, model="iid") +
  f(year_fcode_ID2, cos12, model="iid") +
  f(year_fcode_ID3, sin6,  model="iid") +
  f(year_fcode_ID4, cos6,  model="iid")
'

#Add fixed semi-annual terms too 
mod12 <- '
  obs_dengue_cases_hold ~ 1 +
  lag3_y +
  sin12 + cos12 + sin6 + cos6 +
  f(fcode0, model="iid") +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(fcode3, sin6,  model="iid") +
  f(fcode4, cos6,  model="iid") +
  f(fcode5, lag3_y, model="iid") +
  f(yearID1, sin12, model="iid") +
  f(yearID2, cos12, model="iid") +
  f(yearID3, sin6,  model="iid") +
  f(yearID4, cos6,  model="iid") +
  f(year_fcode_ID1, sin12, model="iid") +
  f(year_fcode_ID2, cos12, model="iid") +
  f(year_fcode_ID3, sin6,  model="iid") +
  f(year_fcode_ID4, cos6,  model="iid")
'

# Add 4-month harmonic
mod13 <- '
  obs_dengue_cases_hold ~ 1 +
  lag3_y +
  sin12 + cos12 + sin6 + cos6 + sin4 + cos4 +
  f(fcode0, model="iid") +
  # District harmonics
  f(fcode1, sin12, model="iid") + f(fcode2, cos12, model="iid") +
  f(fcode3, sin6,  model="iid") + f(fcode4, cos6,  model="iid") +
  f(fcode6, sin4,  model="iid") + f(fcode7, cos4,  model="iid") +
  f(fcode5, lag3_y, model="iid") +
  # Year harmonics
  f(yearID1, sin12, model="iid") + f(yearID2, cos12, model="iid") +
  f(yearID3, sin6,  model="iid") + f(yearID4, cos6,  model="iid") +
  f(yearID5, sin4,  model="iid") + f(yearID6, cos4,  model="iid") +
  # Year×district harmonics
  f(year_fcode_ID1, sin12, model="iid") + f(year_fcode_ID2, cos12, model="iid") +
  f(year_fcode_ID3, sin6,  model="iid") + f(year_fcode_ID4, cos6,  model="iid") +
  f(year_fcode_ID5, sin4,  model="iid") + f(year_fcode_ID6, cos4,  model="iid")
'

#  harmonics + BYM2 spatial baseline instead of iid intercept
mod14 <- '
  obs_dengue_cases_hold ~ 1 +
  lag3_y +
  sin12 + cos12 + sin6 + cos6 +
  f(fcodeID, model="bym2", constr=TRUE, graph=MDR.adj, hyper=hyper.bym2, scale.model=TRUE) +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(fcode3, sin6,  model="iid") +
  f(fcode4, cos6,  model="iid") +
  f(fcode5, lag3_y, model="iid") +
  f(yearID1, sin12, model="iid") +
  f(yearID2, cos12, model="iid") +
  f(yearID3, sin6,  model="iid") +
  f(yearID4, cos6,  model="iid") +
  f(year_fcode_ID1, sin12, model="iid") +
  f(year_fcode_ID2, cos12, model="iid") +
  f(year_fcode_ID3, sin6,  model="iid") +
  f(year_fcode_ID4, cos6,  model="iid")
'

#  Smooth the year-level harmonic coefficients across years (RW1 instead of iid)
mod15 <- '
  obs_dengue_cases_hold ~ 1 +
  lag3_y +
  sin12 + cos12 + sin6 + cos6 +
  f(fcode0, model="iid") +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(fcode3, sin6,  model="iid") +
  f(fcode4, cos6,  model="iid") +
  f(fcode5, lag3_y, model="iid") +
  f(yearID1, sin12, model="rw1") +
  f(yearID2, cos12, model="rw1") +
  f(yearID3, sin6,  model="rw1") +
  f(yearID4, cos6,  model="rw1") +
  f(year_fcode_ID1, sin12, model="iid") +
  f(year_fcode_ID2, cos12, model="iid") +
  f(year_fcode_ID3, sin6,  model="iid") +
  f(year_fcode_ID4, cos6,  model="iid")
'

#  harmonics + thermal + precip 
mod16 <- '
  obs_dengue_cases_hold ~ 1 +
  lag3_y +
  climate_scale_lag3 + lag3_monthly_cum_ppt +
  sin12 + cos12 + sin6 + cos6 +
  f(fcode0, model="iid") +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(fcode3, sin6,  model="iid") +
  f(fcode4, cos6,  model="iid") +
  f(fcode5, lag3_y, model="iid") +
  f(yearID1, sin12, model="iid") +
  f(yearID2, cos12, model="iid") +
  f(yearID3, sin6,  model="iid") +
  f(yearID4, cos6,  model="iid") +
  f(year_fcode_ID1, sin12, model="iid") +
  f(year_fcode_ID2, cos12, model="iid") +
  f(year_fcode_ID3, sin6,  model="iid") +
  f(year_fcode_ID4, cos6,  model="iid")
'


#########Dan's Proposed models 

form1 <-  '
     obs_dengue_cases_hold  ~ 1 +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    f(fcode3, sin6, model = "iid") +
    f(fcode4, cos6, model = "iid") +
    f(fcode5,  lag3_y, model = "iid") +
    
    # Year seasonality - keep both annual AND semi-annual
    f(yearID1, sin12, model = "iid") +
    f(yearID2, cos12, model = "iid") +
   # f(yearID3, sin6, model = "iid") +
  #  f(yearID4, cos6, model = "iid") +
  
    # Year×district
    f(year_fcode_ID1, sin12, model = "iid") +
    f(year_fcode_ID2, cos12, model = "iid") 
    
    '


form2 <-  '
     obs_dengue_cases_hold  ~ 1 +
    
    thermal_suitability_lag3 +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    
    # Year seasonality - keep both annual AND semi-annual
    f(yearID1, sin12, model = "iid") +
    f(yearID2, cos12, model = "iid") +
  
     f(t, model = "ar1", constr = TRUE, replicate = fcode7,
   hyper = list(
      prec = list(prior = "pc.prec", param = c(0.5, 0.01)),
       rho  = list(prior = "pc.cor1", param = c(0, 0.9))
   ))
    
    '


form3 <-  '
     obs_dengue_cases_hold  ~ 1 +
    thermal_suitability_lag3 +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    f(fcode5,  lag3_y, model = "iid") +
    
    # Year seasonality - keep both annual AND semi-annual
    f(yearID1, sin12, model = "iid") +
    f(yearID2, cos12, model = "iid") 
    '


form4 <-  '
     obs_dengue_cases_hold  ~ 1 +
    thermal_suitability_lag3 +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") 

    '


form5 <-  '
     obs_dengue_cases_hold  ~ 1 +
    thermal_suitability_lag3 +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") 
    
    '


#same as 4 but take away thermal index
form6 <-  '
     obs_dengue_cases_hold  ~ 1 +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") 

    '


#same as 4 but take away lag log cases
form7 <- '
     obs_dengue_cases_hold  ~ 1 +
    thermal_suitability_lag3 +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") 

    '


#form 6 +rw1 RE
form8 <-  '
     obs_dengue_cases_hold  ~ 1 +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    
     f(t, model = "rw1", constr = TRUE, replicate = fcode7,
   hyper = list(
      prec = list(prior = "pc.prec", param = c(0.5, 0.01))
   ))

    '


#same as mod8 but with AR1 instead of RW1; similar to mod 2 but without harmonics varying by year
form9 <- '
     obs_dengue_cases_hold  ~ 1 +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    
     f(t, model = "ar1", constr = TRUE, replicate = fcode7,
   hyper = list(
      prec = list(prior = "pc.prec", param = c(0.5, 0.01)),
       rho  = list(prior = "pc.cor1", param = c(0, 0.9))
   ))

    '


###Focus on Models 1, 6, 7, and 9 and add covariate 
form10 <-  '
     obs_dengue_cases_hold  ~ 1 +log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    f(fcode3, sin6, model = "iid") +
    f(fcode4, cos6, model = "iid") +
    f(fcode5,  lag3_y, model = "iid") +
    
    # Year seasonality - keep both annual AND semi-annual
    f(yearID1, sin12, model = "iid") +
    f(yearID2, cos12, model = "iid") +
   # f(yearID3, sin6, model = "iid") +
  #  f(yearID4, cos6, model = "iid") +
  
    # Year×district
    f(year_fcode_ID1, sin12, model = "iid") +
    f(year_fcode_ID2, cos12, model = "iid") +
    f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)
    
    '


form11<-  '
     obs_dengue_cases_hold  ~ 1 +log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)

    '


#same as 4 but take away lag log cases
form12 <- '
     obs_dengue_cases_hold  ~ 1 +
    thermal_suitability_lag3 +log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)

    '


form13 <- '
     obs_dengue_cases_hold  ~ 1 +log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
     lag3_y +
    sin12 + cos12 +
    f(fcode0, model = "iid") +
    
    # District seasonality - annual
    f(fcode1, sin12, model = "iid") +
    f(fcode2, cos12, model = "iid") +
    
     f(t, model = "ar1", constr = TRUE, replicate = fcode7,
   hyper = list(
      prec = list(prior = "pc.prec", param = c(0.5, 0.01)),
       rho  = list(prior = "pc.cor1", param = c(0, 0.9))
   ))+
   f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)

    '

form14 <- '
  obs_dengue_cases_hold ~ 1 +
  climate_scale_lag3 +
  log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
  lag3_y +
  sin12 + cos12 +
  f(fcode0, model="iid") +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)
'


form15 <- '
  obs_dengue_cases_hold ~ 1 +
  climate_scale_lag3 +
  log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
  lag3_y +
  sin12 + cos12 +
  f(fcode0, model="iid") +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(fcode3, climate_scale_lag3, model="iid") +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)
'

form16 <- '
  obs_dengue_cases_hold ~ 1 +
  climate_scale_lag3 +
  log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
  lag3_y +
  sin12 + cos12 +
  f(fcode0, model="iid") +
  f(yearID1, climate_scale_lag3, model="iid") +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)
'

form17 <- '
  obs_dengue_cases_hold ~ 1 +
  climate_scale_lag3 +
  log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
  lag3_y +
  sin12 + cos12 +
  f(fcode0, model="iid") +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(fcode3, sin12*climate_scale_lag3, model="iid") +
  f(fcode4, cos12*climate_scale_lag3, model="iid") +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)
'


form18 <- '
  obs_dengue_cases_hold ~ 1 +
  climate_scale_lag3 +
  log_cum_inc_12m + log_cum_inc_24m + log_cum_inc_36m +
  lag3_y +
  sin12 + cos12 +
  f(fcode0, model="iid") +
  f(fcode1, sin12, model="iid") +
  f(fcode2, cos12, model="iid") +
  f(t, model="ar1", constr=TRUE, replicate=fcode7,
    hyper=list(
      prec=list(prior="pc.prec", param=c(0.5, 0.01)),
      rho =list(prior="pc.cor1",  param=c(0, 0.9))
    )) +
  f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)
'
