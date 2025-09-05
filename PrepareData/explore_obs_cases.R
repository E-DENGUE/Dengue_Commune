library(tidyverse)
library(sf)
library(leaflet)
library(cowplot)

province_codes <- c('BL','BT','CM','CT','HG','LA','KG','TG','TV','VL')

a1 <- lapply(province_codes, function(X) readxl::read_excel('./Data/Dengue_observed_10_province/250905_ED_MONTHLY dengue case_10 provinces_2010-2024.xlsx', sheet=X)) %>%
  bind_rows() %>%
   rename(l2_code = l2_code_commune,
         obs_dengue_cases = dengue ) 


ave_cases <- a1 %>%
  group_by(l2_code) %>%
  mutate(total_cases = sum(obs_dengue_cases),
         ave_month_cases = mean(obs_dengue_cases),
        N= n())
 hist(ave_cases$ave_month_cases)
 range(ave_cases$N) #all communess have 180 months
 
 gravity <- a1 %>%
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
   ungroup()
 
 
ggplot(gravity, aes(x=tot_case, y=gravity))+
  geom_point()

ggplot(gravity, aes(x=commune_index, y=gravity, size=(tot_case)))+
  geom_point()

ggplot(gravity, (aes(x=year, y=gravity, group=l2_code)))+
  geom_line()

ggplot(gravity, (aes(x=year, y=relative_gravity, group=l2_code)))+
  geom_line()

#relative_gravity

ave_rel_gravity <- gravity %>%
  group_by(l2_code) %>%
  summarize(mean_relative_gravity = mean(relative_gravity, na.rm=T),
            min_rel_gravity = quantile(relative_gravity, na.rm=T, probs=0.1),
            max_rel_gravity = quantile(relative_gravity, na.rm=T, probs=0.9),
            tot_case = mean(tot_case, na.rm=T),) %>%
  ungroup() %>%
  arrange(mean_relative_gravity) %>%
  mutate(orderN = row_number(),
         l2_code = as.character(l2_code))

ggplot(ave_rel_gravity)+
  geom_point(aes(x=orderN, y=mean_relative_gravity, size=(tot_case))) +
    geom_errorbar(aes(x=orderN, ymin=min_rel_gravity, ymax=max_rel_gravity), width=0.1) 

geo <- st_read("./Data/Staging_shapefiles/svn_admin_boundary_level2_2025.geojson")

geo_joined <- geo %>%
  right_join(ave_rel_gravity, by='l2_code')  %>%
  mutate(
    gravity_cat = ntile(mean_relative_gravity, 3),
    other_cat   = ntile(tot_case, 3),
    bi_class    = paste0(gravity_cat, "-", other_cat)
  )

#average timing
ggplot(geo_joined) +
  geom_sf(aes(fill = mean_relative_gravity), color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()

  
bivar_pal <- c(
  "3-1" = "#e8e8e8", "2-1" = "#b0d5df", "1-1" = "#64acbe",
  "3-2" = "#e4acac", "2-2" = "#ad9ea5", "1-2" = "#627f8c",
  "3-3" = "#c85a5a", "2-3" = "#985356", "1-3" = "#574249"
)
legend <- ggplot() +
  geom_tile(
    data = expand.grid(gravity_cat = 1:3, case_cat = 1:3),
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


