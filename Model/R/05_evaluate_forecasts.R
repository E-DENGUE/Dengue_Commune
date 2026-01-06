##In console:
# salloc
# module load R/4.2.0-foss-2020b
# R

#setwd("~/project/dengue_test/Cluster_DW")


library(dplyr)
library(parallel)
library(ggplot2)
library(tidyverse)
library(broom)
library(plotly)
library(viridis)
library(lubridate)
library(gganimate)
library(pbapply)
library(scoringutils)
options(dplyr.summarise.inform = FALSE)
library(dplyr)


N_cores = detectCores()


obs_epidemics <- readRDS( './Model/Data/observed_alarms_2025.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  rename(case_vintage=obs_dengue_cases) %>%
  dplyr::select(date, fcode,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))


##cleaned data set 
obs_case <- readRDS('./Model/Data/Full_data_set_with_covariates_and_lags.rds') %>%
  dplyr::select(date, fcode, obs_dengue_cases, pop_total)

out <- readRDS( "./Output/Results_summary/crps_add3.rds")  #CRPS score from model





miss.mod <- out %>%
  group_by(modN) %>%
  dplyr::summarize(N=n()) %>%
  mutate(exclude_miss_mod = N<max(N))

miss.dates <- out %>% 
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod==F) %>%
  group_by(date, horizon) %>%   
  filter(  horizon %in% c(1,2,3)) %>%
  dplyr::summarize(N_mods=n(), N_cases=mean(obs_dengue_cases)) %>%
  ungroup() %>%
  group_by(horizon) %>%
  mutate(miss_date = if_else(N_mods< max(N_mods),1,0 )) %>%
  ungroup()



#note this is not a proper time series--we are double counting cases across models.
ggplot(miss.dates, aes(x=date, y=N_cases)) +
  theme_classic()+
  geom_line()+
  facet_wrap(~horizon) +
  geom_point(aes(x=date, y=N_cases, color=miss_date))

#FILTER OUT months when an epidemic has been recognized by the time forecast is made in a specific fcode (using fixed epidemic threshold)
out_1a <- out %>%
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod!=1) %>%
  dplyr::select(-pop_total,-obs_dengue_cases) %>%
  left_join(obs_epidemics, by=c('fcode'='fcode','vintage_date'='date'))   #%>%
#filter(epidemic_flag==0) #ONLY EVALUATE MONTHS WHERE EPIDEMIC HAS NOT YET BEEN OBSERVED IN THE fcode

#View(out_1a %>% group_by(fcode,date, horizon) %>% dplyr::summarize(N=n()))

 out_1a <- out_1a %>% 
 filter(!modN %in% c("mod1_", "mod2_","mod3_"),
          date <= as.Date("2025-06-01"))

 # out_1a <- out_1a %>% 
 #   filter(     date <= as.Date("2025-06-01"))

#Overall
out2 <- out_1a %>%
  filter(epidemic_flag==0) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  group_by(horizon, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1,rm.na=TRUE),crps2 = mean(crps2,rm.na=TRUE) ,N=n() ) %>%
  ungroup() %>%
  arrange(horizon, crps2) %>%
  group_by(horizon) %>%
  dplyr::mutate( w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2) ) %>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(fcodeID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=fcodeID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  )
View(out2)


w_summary <- out2 %>%
  select(horizon, modN, w_i2) %>%
  mutate(w_i2 = round(w_i2, 3))

w_summary_sum <- w_summary %>%
  group_by(horizon) %>%
  summarise(sum_w_i2 = sum(w_i2, na.rm = TRUE))

w_summary_sum

#what model factors are associate with a higher weight?
mod1 <- lm(w_i1 ~ rw_season + harm_season+lag2_y + lag2_monthly_cum_ppt + rw_time_spatial + type4_spatial_bym, data=out2)
summary(mod1)

#By calendar month
out3 <- out_1a %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  filter(epidemic_flag==0) %>%
  filter( !is.na(crps2)) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, month, modN, form) %>%
  dplyr:: summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,month, crps2)%>%
  ungroup() %>%
  group_by(horizon,month) %>%
  mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),
         w_i2 = (1/crps2^2)/sum(1/crps2^2) )%>%
  filter(horizon==3)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(fcodeID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=fcodeID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  )
View(out3)

library(dplyr)
## How does best model differ by fcode?
out4 <- out_1a %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0 &!is.na(crps2))  %>%
  filter(epidemic_flag==0) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, fcode, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,fcode, crps2)%>%
  ungroup() %>%
  group_by(horizon,fcode) %>%
  mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2), rel_wgt2= w_i2/max(w_i2) )%>%
  filter(horizon==2)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(fcodeID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=fcodeID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  ) %>%
  dplyr::select(-form) %>%
  ungroup() %>%
  arrange(fcode, crps2) %>%
  group_by(fcode) %>%
  mutate(mod_rank = row_number()) %>%
  ungroup()

out4_ranks <- out4 %>% group_by(modN) %>% dplyr::summarize(ave_rank=mean(mod_rank), min_rank=min(mod_rank), max=max(mod_rank))


View(out4)

out4%>%
  ggplot(aes(x=modN,y=rel_wgt2, group=fcode )) +
  geom_line()+
  theme_classic()

ggplot(out4, aes(x = fcode, y = modN, fill = rel_wgt2)) +
  geom_tile() +
  scale_fill_viridis(discrete = FALSE)+
  labs(title = "CRPS Heatmap",
       x = "fcode",
       y = "ModelN")

## cluster of models, base don performance across fcodes
out4.c <- reshape2::dcast(out4, modN~fcode, value.var= 'rel_wgt2' ) 

out4.c.m <- out4.c %>% dplyr::select(-modN) %>% as.matrix()

row.names(out4.c.m) <- out4.c$modN
dist_mat <- dist(as.matrix(out4.c.m), method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 4)

cluster_mods <- cbind.data.frame(modN=names(cut_avg), clustN=cut_avg) %>%
  left_join(out2, by='modN') %>%
  arrange(clustN, crps2) %>%
  group_by(clustN) %>%
  mutate(group_order=row_number()) %>%
  ungroup()

##MODELS TO SELECT
#Select mod1 in cluster 1; 
#mod18 in cluster 2
#mod49 in cluster 3
#PC1 in cluster 3
#modhhh4_power_precip_temp_ from cluster 4

ensemble_mods <- c ('mod1_','mod2_','mod3_','mod4_','mod5_','mod6_','PC_lags','modhhh4_power_precip_temp_',"modhhh4_power_cum_lag24_")

## reverse now: clusterof fcodes, based on how different models perform
out4.c.map <- reshape2::dcast(out4, fcode~modN, value.var= 'rel_wgt2' ) 

out4.c.map.m <- out4.c.map %>% dplyr::select(-fcode) %>% as.matrix()

row.names(out4.c.map.m) <- out4.c.map$fcode
dist_mat_map <- dist(as.matrix(out4.c.map.m), method = 'euclidean')

hclust_avg_map <- hclust(dist_mat_map, method = 'average')
plot(hclust_avg_map)
cut_avg_map <- cutree(hclust_avg_map, k = 4)
cluster.map <- cbind.data.frame(fcode=names(cut_avg_map), clustN=cut_avg_map)

#MAP THESE CLUSTER ASSIGNMENTS


out4 %>%
  reshape2::dcast(fcode~modN, value.var='rel_wgt2') %>%
  dplyr::select(-fcode) %>%
  as.matrix() %>%
  cor() 

#mean of relative weights across all fcodes--this basically agrees with what is seen in out2
out4 %>% group_by(modN) %>% dplyr::summarize(rel_wgt2=mean(rel_wgt2)) %>% arrange(-rel_wgt2)

#how much does inclusion of different components affect model weight?
mods <- out3 %>%
  ungroup() %>% 
  nest_by(month) %>%
  mutate(mod = list(lm(w_i2 ~ rw_season + harm_season+lag2_y + lag2_monthly_cum_ppt + rw_time_spatial + type4_spatial_bym, data=data))) %>%
  dplyr::reframe(broom::tidy(mod)) %>%
  mutate(p.value=round(p.value,3))
#View(mods)

#################################################
#Observed vs expected

mod.weights_overall <- out2 %>%
  ungroup() %>%
  dplyr::select( w_i2 ,modN) %>%
  filter(modN %in% ensemble_mods) %>%
  mutate(w_i2 = w_i2/sum(w_i2))

#ensemble, weight based on overall performance
p0.ds <- out_1a %>%
  filter(modN %in% ensemble_mods) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  left_join(obs_case, by=c('date','fcode')) %>%
  filter( horizon==3) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date) %>%
  dplyr::summarize(obs_dengue_cases=sum(obs_dengue_cases),pop_total=sum(pop_total), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_overall, by=c('modN')) %>% #weights determined by month-specific  predictions
  ungroup() %>%
  group_by(date, vintage_date) %>%
  mutate(sum_wgts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_overall = sum(w_i2/sum_wgts *pred_count) ) %>%
  dplyr::select(date,vintage_date,ensemble_overall)

#ensemble; varying weights by calendar month
mod.weights_t <- out3 %>%
  ungroup() %>%
  filter(modN %in% ensemble_mods) %>%
  filter(horizon==3) %>%
  dplyr::select(w_i2, modN,  month)

p1.ds <- out_1a %>%
  filter(modN %in% ensemble_mods) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  left_join(obs_case, by=c('date','fcode')) %>%
  filter( horizon==2 ) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date) %>%
  dplyr::summarize(obs_dengue_cases=sum(obs_dengue_cases,rm.na=TRUE),pop_total=sum(pop_total), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_t, by=c('modN','month')) %>% #weights determined by month-specific  predictions
  ungroup() %>%
  group_by(date, vintage_date) %>%
  mutate(sum_wgts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_month = sum(w_i2/sum_wgts *pred_count),
                   obs_dengue_cases=mean(obs_dengue_cases),pop_total=mean(pop_total)) %>%
  ungroup() %>%
  arrange( date)

# mod.weights_dist<- out4 %>%
#   ungroup() %>%
#   filter(horizon==2) %>%
#   dplyr::select(w_i2, modN,   fcode) %>%
#   arrange(fcode, -w_i2)
# 
# p3.ds<- out_1a %>%
#   filter(modN %in% ensemble_mods) %>%
#   left_join(miss.dates, by=c('date','horizon')) %>%
#   filter(miss_date==0 & exclude_miss_mod==0)  %>%
#   left_join(obs_case, by=c('date','fcode')) %>%
#   filter( horizon==2 ) %>%
#   dplyr::select(-form) %>%
#   group_by(modN,fcode, date,vintage_date) %>%
#   dplyr::summarize(obs_dengue_cases=sum(obs_dengue_cases),pop_total=sum(pop_total), pred_count=sum(pred_mean),threshold_poisson=threshold_poisson,threshold_nb=threshold_nb,threshold=threshold,threshold_quant=threshold_quant) %>%
#   mutate(month=month(date)) %>%
#   left_join(mod.weights_dist, by=c('modN','fcode')) %>% #weights determined by month-specific  predictions
#   #filter(w_i2>=0.05) %>%
#   ungroup() %>%
#   group_by(date,vintage_date, fcode) %>%
#   mutate(sum_wts=sum(w_i2)) %>%
#   dplyr::summarize(ensemble_dist_wgt = sum(w_i2/sum_wts *pred_count) #summarize across the different models to get date and fcode-specific estimate
#   ) %>%
#   ungroup() %>%
#   group_by(date,vintage_date,fcode) %>%
#   dplyr::summarize( ensemble_dist_wgt=sum(ensemble_dist_wgt)) %>% #sum across the fcodes
#   dplyr::right_join(p1.ds, by='date') %>%
#   left_join(p0.ds, by=c('date'))




p1a <- p1.ds %>%
  ggplot(aes(x=date, y=obs_dengue_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  #geom_line(aes(x=date, y=pred_count,group=modN, color=modN), lwd=0.5, alpha=0.5) +
  geom_line(aes(x=date, y=ensemble_month), alpha=0.5 ,lwd=1, col='red')+
  ggtitle("Prediction Accuracy")



p1b <- p1.ds %>%
  ggplot(aes(x=date, y=obs_dengue_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  #geom_line(aes(x=date, y=pred_count,group=modN, color=modN), lwd=0.5, alpha=0.5) +
  geom_line(aes(x=vintage_date, y=ensemble_month), alpha=0.5 ,lwd=1, col='red')+
  ggtitle("Predictions by Vintage Date")

p1a
p1b
ggplotly(p1b)  


#########################
#same but weights vary by fcode
mod.weights_dist<- out4 %>%
  ungroup() %>%
  filter(horizon==2) %>%
  dplyr::select(w_i2, modN,   fcode) %>%
  arrange(fcode, -w_i2)


library(dplyr)

pp.ds <- out_1a %>%
  left_join(obs_case, by=c('date','fcode')) %>%
  filter( horizon==2 & modN %in% ensemble_mods ) %>% #RESTRICTS TO THE SELECTED ENSEMBLE
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,fcode) %>%
  dplyr::summarize(obs_dengue_cases=sum(obs_dengue_cases),pop_total=sum(pop_total), pred_count=sum(pred_mean),threshold_quant=threshold_quant) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','fcode')) %>% #weights determined by month-specific  predictions
  #filter(w_i2>=0.05) %>%
  ungroup() %>%
  group_by(date,vintage_date, fcode) %>%
  mutate(sum_wts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_dist_wgt = sum(w_i2/sum_wts*pred_count/pop_total*100000)) #summarize across the different models to get date and fcode-specific estimate




p2.ds <- out_1a %>%
  left_join(obs_case, by=c('date','fcode')) %>%
  filter( horizon==3 & modN %in% ensemble_mods ) %>% #RESTRICTS TO THE SELECTED ENSEMBLE
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,fcode) %>%
  summarize(obs_dengue_cases=sum(obs_dengue_cases,rm.na=TRUE),pop_total=sum(pop_total), pred_count=sum(pred_mean)) %>%
  mutate(month=lubridate::month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','fcode')) %>% #weights determined by month-specific  predictions
  #filter(w_i2>=0.05) %>%
  ungroup() %>%
  group_by(date,vintage_date, fcode) %>%
  mutate(sum_wts=sum(w_i2)) %>%
  summarize(ensemble_dist_wgt = sum(w_i2/sum_wts *pred_count) #summarize across the different models to get date and fcode-specific estimate
  ) %>%
  ungroup() %>%
  group_by(date,vintage_date) %>%
  summarize( ensemble_dist_wgt=sum(ensemble_dist_wgt)) %>% #sum across the fcodes
  right_join(p1.ds, by='date') %>%
  left_join(p0.ds, by=c('date'))


p2.ensembles <- p2.ds %>%
  ggplot(aes(x=date, y=obs_dengue_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA) +
  # geom_line(aes(x=date, y=pred_count,group=modN, color=modN), lwd=0.5, alpha=0.5) +
  geom_line(aes(x=date, y=ensemble_dist_wgt,), alpha=0.5 ,lwd=1, col='red')+ #weight by fcode
  geom_line(aes(x=date, y=ensemble_month,), alpha=0.5 ,lwd=1, col='blue')+ #weight by calendar month 
  geom_line(aes(x=date, y=ensemble_overall), alpha=0.5 ,lwd=1, col='orange')+
  ggtitle("Differential weighting of ensemble has little effect")

p2.ensembles #the first 2 ensembles looks almost identical; weighting by fcode slightly different


p2.ensembles <- p2.ds %>%
  ggplot(aes(x = date, y = obs_dengue_cases)) +
  geom_line(color = "black", lwd = 0.7) +
  geom_line(aes(y = ensemble_dist_wgt, color = "Weight by fcode"), alpha = 0.7, lwd = 1) +
  geom_line(aes(y = ensemble_month, color = "Weight by month"), alpha = 0.7, lwd = 1) +
  geom_line(aes(y = ensemble_overall, color = "Overall weight"), alpha = 0.7, lwd = 1) +
  theme_classic() +
  ylim(0, NA) +
  labs(
    title = "Differential weighting of ensemble has little effect",
    color = "Ensemble Type"
  ) +
  scale_color_manual(values = c(
    "Weight by fcode" = "red",
    "Weight by month" = "blue",
    "Overall weight" = "orange"
  ))

p2.ensembles


obs_vs_expected_fcode <- out_1a %>%
  left_join(obs_case, by=c('date','fcode')) %>%
  filter( horizon==3 &  modN %in% ensemble_mods ) %>%
  group_by(fcode, modN) %>%
  dplyr::summarize(obs=sum(obs_dengue_cases), pred=sum(pred_mean)) %>%
  mutate(diff= obs - pred, rr=obs/pred)

p2.ds <- out_1a %>%
  left_join(obs_case, by=c('date','fcode')) %>%
  dplyr::select(-form) 

p2a <- p2.ds %>%
  filter( horizon==3 &  modN %in% ensemble_mods  & fcode %in% c("ED_VINH_LONG_BINH_MINH_TOWN"  )) %>%
  ggplot(aes(x=date, y=obs_dengue_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~fcode,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN),alpha=0.2)+
  ggtitle('ED_VINH_LONG_BINH_MINH_TOWN')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2a

#Ben luc in cluster 2
p2b <- p2.ds %>%
  filter( horizon==2 &  modN %in% ensemble_mods  & fcode %in% c('BEN LUC')) %>%
  ggplot(aes(x=date, y=obs_dengue_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~fcode,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN),alpha=0.2)+
  ggtitle('Ben Luc')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2b

#BAC LIEU in cluster 3
p2c <- p2.ds %>%
  filter( horizon==2 &  modN %in% ensemble_mods  & fcode %in% c('BAC LIEU')) %>%
  ggplot(aes(x=date, y=obs_dengue_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~fcode,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN),alpha=0.2)+
  ggtitle('Bac Lieu')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2c

#CAI LAY in cluster 3
p2d <- p2.ds %>%
  filter( horizon==2 &  modN %in% ensemble_mods  & fcode %in% c('CAI LAY')) %>%
  ggplot(aes(x=date, y=obs_dengue_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~fcode,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN),alpha=0.2)+
  ggtitle('Cai Lay')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2d

library(ggpubr)

ggarrange(
  p2a, p2b, p2c, p2d, 
  ncol = 2, nrow = 2,    # This arranges the plots in a 2x2 grid
  common.legend = TRUE,  # Indicates a common legend for all plots
  legend = 'bottom'      # Places the common legend at the bottom
)
#### Ensemble performance by fcode
fcode_select <- c("ED_TIEN_GIANG_CAI_BE_DISTRICT","ED_SOC_TRANG_LONG_PHU_DISTRICT"  ,"ED_TIEN_GIANG_CHO_GAO_DISTRICT" ,  "ED_TRA_VINH_CHAU_THANH_DISTRICT"    )

p2.ds_fcode <- out_1a %>%
  left_join(obs_case, by=c('date','fcode')) %>%
  filter( horizon==3 ) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,fcode) %>%
  dplyr::summarize(obs_dengue_cases=sum(obs_dengue_cases),pop_total=sum(pop_total), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','fcode')) %>% #weights determined by month-specific  predictions
  filter(  modN %in% ensemble_mods ) %>%
  ungroup() %>%
  group_by(date,vintage_date, fcode) %>%
  mutate(sum_wts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_dist_wgt = sum(w_i2/sum_wts *pred_count), obs_dengue_cases=mean(obs_dengue_cases) #summarize across the different models to get date and fcode-specific estimate
  ) %>%
  filter(fcode %in% fcode_select )

p2.ds_fcode %>%
  ggplot(aes(x=date, y=obs_dengue_cases)) +
  geom_line()+
  geom_line(aes(x=date, y=ensemble_dist_wgt), color='red')+
  facet_wrap(~fcode)+
  theme_minimal()

#b vintage_date--would want red line to be ahead of black line
p2.ds_fcode %>%
  ggplot(aes(x=date, y=obs_dengue_cases)) +
  geom_line()+
  geom_line(aes(x=vintage_date, y=ensemble_dist_wgt), color='red')+
  facet_wrap(~fcode)+
  theme_minimal() +
  ggtitle('Vintage date of forecats vs obs date')

###The outbreak region from the prediction using the ensemble model

################################################

################################################
##BRIER SCORE
################################################
#Shows poor performance during time of year with little dengue; good performance
#when  in dengue season
b1 <- readRDS('./Output/Results_summary/brier_add3.rds')


b2_brier <- inner_join(b1, obs_case, by = c("fcode", "date")) 
b2_brier <- b2_brier %>% filter (modN== c("mod1_"  ,                                                                  
                                                "mod2_"  ,                                                                  
                                                "mod3_"   ,                                                                 
                                                "mod4_"    ,                                                                
                                                "mod5_"     ,                                                               
                                                "mod6_" ))




monthly_brier <- b2_brier %>%
  group_by(monthN,modN,horizon) %>%
  summarise(
    mean_brier_nb = mean(brier_nb, na.rm = TRUE),
    mean_brier_2sd = mean(brier_2sd, na.rm = TRUE)
  )


library(dplyr)
library(ggplot2)

# Summarize data
monthly_brier <- b2_brier %>%
  group_by(monthN, modN, horizon) %>%
  summarise(
    mean_brier_nb = mean(brier_nb, na.rm = TRUE),
    mean_brier_2sd = mean(brier_2sd, na.rm = TRUE),
    .groups = "drop"
  )

monthly_brier %>%
  filter(horizon == 3) %>%
  ggplot(aes(x = factor(monthN), y = mean_brier_2sd, color = modN, group = modN)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  theme_classic() +
  labs(
    title = "Monthly Mean 2SD Brier Score for Horizon = 3",
    x = "Month",
    y = "Mean Brier (2SD Method)",
    color = "Model"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


library(ggplot2)
library(dplyr)

monthly_brier %>%
  group_by(horizon, modN) %>%
  summarise(mean_brier_2sd = mean(mean_brier_2sd, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(horizon), y = mean_brier_2sd, color = modN, group = modN)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_classic() +
  labs(
    title = "Brier Score Across Lead Times",
    x = "Lead time (months)",
    y = "Brier score (mean, 2SD method)",
    color = "Model"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

monthly_brier  %>%
  dplyr::group_by(horizon, modN) %>%
  dplyr::summarize(brier_2sd = mean(brier_2sd,na.rm = TRUE)) %>%
  ggplot(aes(x = horizon, y = brier_2sd, group = modN, color = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 1, alpha = 0.7) +   geom_point(aes(shape = modN), size = 3) +
  ylim(0, 0.20) +  # Set y-axis limits
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "Brier score",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  )


library(dplyr)
library(lubridate)

b2_brier <- b2_brier %>%
  mutate(year = lubridate::year(date))

brier_year <- b2_brier %>% filter(year==2025)%>%  group_by(year, modN, horizon) %>%
  summarise(mean_brier_2sd = mean(brier_2sd, na.rm = TRUE), .groups = "drop")

library(ggplot2)
ggplot(brier_year, aes(x = factor(horizon), 
                       y = mean_brier_2sd, 
                       color = modN, 
                       group = modN)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_classic() +
  labs(
    title = "Model Performance (Brier Mean+2SD)",
    x = "Forecast Horizon (Months)",
    y = "Mean Brier Score (2SD)",
    color = "Model"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
