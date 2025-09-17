library(tidyverse)
library(sf)
library(leaflet)
library(cowplot)
library(INLA)

a1 <- vroom::vroom("../Data/case_data.csv.gz") %>%
  arrange(fcode, date) %>%
  mutate(t = row_number())


ave_cases <- a1 %>%
  group_by(l2_code) %>%
  mutate(total_cases = sum(obs_dengue_cases),
         ave_month_cases = mean(obs_dengue_cases),
        N= n())
 hist(ave_cases$ave_month_cases)
 range(ave_cases$N) #all communess have 180 months
 
 max_dtr <- a1 %>%
   mutate(year= lubridate::year(date),
          month = lubridate::month(date)) %>%
   group_by(l2_code, year) %>%
   mutate(max_dtr_ind = if_else(lag3_monthly_dtr==max(lag3_monthly_dtr),1,-999),
          max_dtr_month = month*max_dtr_ind - 3, #what is the actual month
          max_dtr_month = if_else(max_dtr_month<1,max_dtr_month+12, max_dtr_month)
          )%>%
   summarize(max_dtr= max(lag3_monthly_dtr),
             max_dtr_month = max(max_dtr_month)
   )
   
 gravity <- a1 %>%
   mutate(year= lubridate::year(date),
          month = lubridate::month(date)) %>%
   group_by(l2_code, year) %>%
   mutate(proportion_month = obs_dengue_cases/sum(obs_dengue_cases),
          gravity_part = proportion_month * month
          ) %>%
   summarize( gravity = sum(gravity_part),
              check_prop = sum(proportion_month),
              tot_case = sum(obs_dengue_cases)
              ) %>%
   ungroup() %>%
   mutate(commune_x = as.numeric(as.factor(l2_code))) %>%
   group_by(year) %>%
   mutate(relative_gravity = gravity - mean(gravity, na.rm=T)) %>%
   ungroup() %>%
   arrange(l2_code, year) %>%
   group_by(l2_code) %>%
   mutate(rel_grav_lag1 = lag(relative_gravity,1)
          ) %>%
   ungroup() %>%
   left_join(max_dtr, by=c('year','l2_code')
   )
 
 l2_codes <- unique(gravity$l2_code)
 
 gravity %>%
   filter(l2_code %in% l2_codes[1:10]) %>%
ggplot( aes(x=rel_grav_lag1, y=relative_gravity, color=as.factor(l2_code)))+
   geom_point()
 
ggplot(gravity, aes(x=tot_case, y=gravity))+
  geom_point()

ggplot(gravity, aes(x=max_dtr, y=gravity))+
  geom_point()
ggplot(gravity, aes(x=max_dtr_month, y=gravity))+
  geom_point()

ggplot(gravity, aes(x=max_dtr_month, y=max_dtr))+
  geom_point()

# ggplot(gravity, aes(x=commune_index, y=gravity, size=(tot_case)))+
#   geom_point()

ggplot(gravity, (aes(x=year, y=gravity, group=l2_code)))+
  geom_line()

ggplot(gravity, (aes(x=year, y=relative_gravity, group=l2_code)))+
  geom_line()

#relative_gravity

ave_rel_gravity <- gravity %>%
  group_by(l2_code) %>%
  summarize(mean_relative_gravity = mean(relative_gravity, na.rm=T),
            var_relative_gravity = var(relative_gravity, na.rm=T),
            min_rel_gravity = quantile(relative_gravity, na.rm=T, probs=0.1),
            max_rel_gravity = quantile(relative_gravity, na.rm=T, probs=0.9),
            tot_case = mean(tot_case, na.rm=T),) %>%
  ungroup() %>%
  arrange(mean_relative_gravity) %>%
  mutate(orderN = row_number(),
         l2_code = as.character(l2_code))

ggplot(ave_rel_gravity)+
  geom_point(aes(x=orderN, y=mean_relative_gravity)) +
    geom_errorbar(aes(x=orderN, ymin=mean_relative_gravity - 2*sqrt(var_relative_gravity), ymax=mean_relative_gravity + 2*sqrt(var_relative_gravity)), width=0.1) 

geo <- st_read("./Data/Staging_shapefiles/mdr_boundary_level2_2025.geojson")

geo_joined <- geo %>%
  right_join(ave_rel_gravity, by='l2_code')  %>%
  mutate(
    pop_density = population/area/100,
    incidence = tot_case/population*100000,
    gravity_cat = ntile(mean_relative_gravity, 2),
    other_cat   = ntile(tot_case, 2),
    bi_class    = paste0(gravity_cat, "-", other_cat)
  )

# ggplot(geo_joined) +
#   geom_point(aes(x=sqrt(pop_density), y=sqrt(incidence) ) )

#average timing
ggplot(geo_joined) +
  geom_sf(aes(fill = mean_relative_gravity), color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()

bivar_pal <- c(
   "2-1" = "#b0d5df", "1-1" = "#64acbe",
   "2-2" = "#c85a5a", "1-2" = "#574249"
  )

legend <- ggplot() +
  geom_tile(
    data = expand.grid(gravity_cat = 1:2, case_cat = 1:2),
    aes(x = gravity_cat, y = case_cat, fill = paste0(gravity_cat, "-", case_cat))
  ) +
  scale_fill_manual(values = bivar_pal) +
  labs(x = "Later →", y = "Total Cases ↑") +
  theme_void() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 8),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1)
  )

geo_joined %>%
  ggplot() +
  geom_point(aes(x=factor(bi_class), y=log(pop_density)))+
  geom_violin(aes(x=factor(bi_class), y=log(pop_density)))

geo_joined %>%
  ggplot() +
  geom_point(aes(y=mean_relative_gravity, x=log(pop_density), size=1/var_relative_gravity), alpha=0.2)+
  theme_classic()

geo_joined %>%
  ggplot() +
  geom_point(aes(y=mean_relative_gravity, x=log(pop_density)))


map_density <- ggplot(geo_joined) +
  geom_sf(aes(fill = log(pop_density)), color = "white", size = 0.2)+
  theme_minimal() 
  
map_density

map <- ggplot(geo_joined) +
  geom_sf(aes(fill = bi_class), color = "white", size = 0.2) +
  scale_fill_manual(values = bivar_pal, na.value = "grey80") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Bivariate Choropleth",
    subtitle = "Mean Relative Gravity × Total Cases"
  )

final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +          # map fills canvas
  draw_plot(legend, 0.7, 0.1, 0.25, 0.25) # place legend in bottom-right

final_plot

###############################################
##time seriespredictors of gravity
#############################################
id_mapping_key <- vroom::vroom('../Data/inla_id_key.csv')

mod_ds <- gravity %>%
  left_join(id_mapping_key, by='l2_code') %>%
  mutate(t = year - min(year, na.rm=T) + 1 ,
         fcodeID1 = fcode,
         fcodeID2 = fcode
         
                  )  %>%
  filter(!is.na(fcodeID1)) 
  
  


form2 <- as.formula(
  'relative_gravity~   lag3_monthly_dtr + lag3_avg_min_daily_temp + lag3_monthly_cum_ppt+
                            f(fcodeID1,
                        model="bym2",
                        constr= TRUE,
                        graph=MDR.adj,
                         hyper = hyper.bym2 ,
                        scale.model = TRUE) +
                    f(t,  replicate=fcodeID2, model="ar1", hyper = hyper.ar1,constr=TRUE) 
                '
)

form3 <- as.formula(
  'relative_gravity~   f(fcodeID1,
                        model="iid") +
                    f(t,  replicate=fcodeID2, model="ar1", hyper = hyper.ar1,constr=TRUE) 
                '
)
hyper.bym2 = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                  theta2 = list(prior="pc", param=c(0.5, 0.5)))

hyper.ar1 = list(theta1 = list(prior='pc.prec', param=c(0.5, 0.01)),
                 rho = list(prior='pc.cor0', param = c(0.5, 0.75)))

MDR.adj <- "../Data/MDR.graph.commune"

mod1 <- inla(form3, data = mod_ds[mod_ds$year<2020,],  family = "gaussian",
             control.compute = list(dic = FALSE, 
                                    waic = FALSE, 
                                    config = T,
                                    return.marginals=F
             ),
             # save predicted values on response scale
             control.inla = list(strategy = 'simplified.laplace', cmin = 0.01),
             control.fixed = list(mean.intercept=0, 
                                  prec.intercept=1, # precision 1
                                  mean=0, 
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=4
)    

summary(mod1)

####
#PLOTS OF DTR BY AREA
#####
#"at higher average temperatures (above 18°C), large daily temperature fluctuations decrease mosquito survival and viral amplification, leading to lower transmission rates"
a1 %>%
  ggplot(aes(x=date, y=lag3_monthly_dtr, group=fcode))+
  geom_line( alpha=0.1)+
  ylab('Diurnal temperature range')

a1 %>%
  ggplot(aes(x=date, y=(lag3_avg_min_daily_temp-273.15), group=fcode))+
  geom_line()

a1 %>%
  ggplot(aes(x=date, y=lag3_monthly_cum_ppt, group=fcode))+
  geom_line()

a1 %>%
  ggplot(aes(x=lag3_monthly_dtr, y=lag3_avg_min_daily_temp))+
  geom_point(alpha=0.1)


ave_climate <- a1 %>%
  group_by(l2_code) %>%
  summarize(mean_dtr= mean(lag3_monthly_dtr,na.rm=T),
            mean_min_temp = mean(lag3_avg_min_daily_temp, na.rm=T),
            mean_ppt = mean(lag3_monthly_cum_ppt, na.rm=T)
            )

map <- ggplot(geo_joined) +
  geom_sf(aes(fill = bi_class), color = "white", size = 0.2)


form4 <- as.formula(
  'obs_dengue_cases~   lag3_avg_min_daily_temp +
                          lag3_monthly_dtr +
                          lag3_monthly_cum_ppt+
                          lag3_monthly_dtr*lag3_avg_min_daily_temp +
                          lag3_monthly_dtr *lag3_monthly_cum_ppt+
                            f(fcode,
                        model="iid") +
                    f(t,   model="ar1", hyper = hyper.ar1,constr=TRUE) 
                '
)

mod_ds <- a1 %>%
  ungroup() %>%
  mutate( lag3_avg_min_daily_temp= scale(lag3_avg_min_daily_temp),
          lag3_monthly_dtr= scale(lag3_monthly_dtr),
          lag3_monthly_cum_ppt = scale(lag3_monthly_cum_ppt)
          )

pop1 <- mod_ds$population/100000

mod1 <- inla(form4, data = mod_ds,  family = "poisson", offset=pop1,
             control.compute = list(dic = FALSE, 
                                    waic = FALSE, 
                                    config = T,
                                    return.marginals=F
             ),
             # save predicted values on response scale
             control.inla = list(strategy = 'simplified.laplace', cmin = 0.01),
             control.fixed = list(mean.intercept=0, 
                                  prec.intercept=1, # precision 1
                                  mean=0, 
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=4
)    
summary(mod1)

