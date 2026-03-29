# Check out real estate transactions for the Oregon coast. 

#  Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(haven)

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

dat_flat_lincoln_less = dat_flat_lincoln %>% select(sale_amount, year_sold, parcel_longitude, parcel_latitude)

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

#   Are sale amounts increasing in time?

vis_time = 
  dat_flat_lincoln_less %>% 
  filter(year_sold %in% 2001:2025) %>% 
  ggplot() +
  geom_boxplot(aes(x = year_sold %>% factor,
                   y = sale_amount %>% log)) + 
  labs(x = "Sale Year",
       y = "Sale Amount (Log.) (Nominal?)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#   Are sale amounts decreasing in longitude (increasing in proximity to the coast)?

#  Switch to a geom_raster.

vis_longitude =
  dat_flat_lincoln_less %>% 
  filter(year_sold %in% 2001:2025) %>% 
  ggplot() + 
  geom_raster(aes(x = parcel_longitude,
                 y = sale_amount %>% log),
             alpha = 0.05) +
  labs(x = "Longitude",
       y = "Sale Amount (Log.) (Nominal?)") +
  theme_minimal()

#  Are sale amounts decreasing in the product of longitude and years?  


#  Descriptive Model


