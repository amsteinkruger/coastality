# Check out real estate transactions for the Oregon coast. 

#  Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(haven)
library(viridis)

#  Parameters

list_counties = 
  list("Clatsop" = 4,
       "Tillamook" = 29,
       "Lincoln" = 21,
       "Lane" = 20,
       "Douglas" = 10,
       "Coos" = 6,
       "Curry" = 8)

#  Data

#   Flat Data

dat_flat_lincoln = 
  "data/dta files (stata)/Lincoln_res_clean_v2026.dta" %>% 
  read_dta

dat_flat_lincoln %>% names %>% as_tibble %>% View

dat_flat_lincoln_less = 
  dat_flat_lincoln %>% 
  select(year_sold, 
         sale_amount, 
         buyer_loc, 
         parcel_longitude, 
         parcel_latitude) %>% 
  mutate(buyer_loc = ifelse(buyer_loc %in% 1:2, 0, ifelse(buyer_loc == 99, NA, 1))) %>% # Is the buyer new to coastal OR? 
  filter(year_sold %in% 2001:2025)

#    Note that buyer location is only as good as mailing cities (per Stata code). 

#   Spatial Data

dat_spat_lincoln = 
  "data/OR_parcelmaps/Oregon/2020_shapefile/OR_parcels_2020.shp" %>% 
  vect %>% 
  filter(County %in% list_counties) %>% 
  filter(County == 21)

dat_spat_lincoln_less = dat_spat_lincoln %>% select(OBJECTID)

#  Processing

#   Spatial Join

#   Euclidean Distance to Coast

#   Road Distance to Coast Access

#   Viewsheds

#  Descriptive Visualization

#   What's the distribution of sale amounts over space?

dat_lincoln_points = 
  dat_flat_lincoln_less %>% 
  vect(geom = c("parcel_longitude", "parcel_latitude"))

dat_lincoln_points_bounds = dat_lincoln_points %>% bbox

#   Are sale amounts increasing with in-migration from a buyer mailing address outside coastal Oregon?

library(ggridges)

vis_inmigrate = 
  dat_flat_lincoln_less %>% 
  ggplot(aes(x = sale_amount %>% log, 
             y = buyer_loc %>% factor,
             fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 0.95,
                               quantile_lines = TRUE, 
                               quantiles = 2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis(option = "C") +
  labs(x = "Sale Amount (Log.) (Nominal?)",
       y = "Densities by Buyer Residency",
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("out_small/vis_inmigrate.png",
       vis_inmigrate,
       dpi = 300,
       width = 4.5,
       height = 3)

#   Are sale amounts increasing in time?

vis_time = 
  dat_flat_lincoln_less %>% 
  ggplot() +
  geom_boxplot(aes(x = year_sold %>% factor,
                   y = sale_amount %>% log)) + 
  labs(x = "Sale Year",
       y = "Sale Amount (Log.) (Nominal?)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("out_small/vis_time.png",
       vis_time,
       dpi = 300,
       width = 4.5,
       height = 3)

#   Are sale amounts decreasing in longitude (increasing in proximity to the coast)?

vis_longitude =
  dat_flat_lincoln_less %>% 
  mutate(parcel_longitude_cut = parcel_longitude %>% cut(breaks = 100),
         sale_amount_cut = sale_amount %>% log %>% cut(breaks = 100)) %>% 
  group_by(parcel_longitude_cut, sale_amount_cut) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  complete(parcel_longitude_cut, sale_amount_cut) %>% 
  mutate(count = count %>% replace_na(0)) %>% 
  ggplot() + 
  geom_raster(aes(x = parcel_longitude_cut,
                  y = sale_amount_cut,
                  fill = count)) +
  scale_fill_viridis(option = "E") +
  coord_fixed() +
  labs(x = "Longitude",
       y = "Sale Amount (Log.) (Nominal?)",
       fill = "Transactions") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.ticks = element_blank())

ggsave("out_small/vis_longitude.png",
       vis_longitude,
       dpi = 300,
       width = 4.5,
       height = 3)

#  Are sale amounts decreasing in the product of longitude and years?  

vis_both = 
  dat_flat_lincoln_less %>% 
  mutate(interaction = year_sold * parcel_longitude,
         interaction_log = interaction %>% abs %>% log,
         interaction_log_cut = interaction_log %>% cut(breaks = 100),
         sale_amount_cut = sale_amount %>% log %>% cut(breaks = 100)) %>% 
  group_by(interaction_log_cut, sale_amount_cut) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  complete(interaction_log_cut, sale_amount_cut) %>% 
  mutate(count = count %>% replace_na(0)) %>% 
  ggplot() + 
  geom_raster(aes(x = interaction_log_cut,
                  y = sale_amount_cut,
                  fill = count)) +
  scale_fill_viridis(option = "E") +
  coord_fixed() +
  labs(x = "Interaction of Year and Longitude (Abs. Value, Log.)",
       y = "Sale Amount (Log.) (Nominal?)",
       fill = "Transactions") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.ticks = element_blank())

ggsave("out_small/vis_both.png",
       vis_both,
       dpi = 300,
       width = 4.5,
       height = 3)

#  Descriptive Models

mod_0 = 
  dat_flat_lincoln_less %>% 
  lm(sale_amount ~ buyer_loc,
     data = .)

mod_1 = 
  dat_flat_lincoln_less %>% 
  lm(sale_amount ~ year_sold,
     data = .)

mod_2 = 
  dat_flat_lincoln_less %>% 
  lm(sale_amount ~ parcel_longitude,
     data = .)

mod_3 = 
  dat_flat_lincoln_less %>% 
  lm(sale_amount ~ year_sold * parcel_longitude,
     data = .)

mod_4 = 
  dat_flat_lincoln_less %>% 
  lm(sale_amount ~ buyer_loc * year_sold * parcel_longitude,
     data = .)

library(modelsummary)
library(kableExtra)

models <- list()
models[["1"]] <- mod_0
models[["2"]] <- mod_1
models[["3"]] <- mod_2
models[["4"]] <- mod_3
models[["5"]] <- mod_4

modelsummary(models,
             stars = c('*' = .05, '**' = .01, '***' = .001),
             statistic = "std.error",
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "out_small/table.png",
             title = "Linear Regression Results")
