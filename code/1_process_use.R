# Set up a panel of transactions with relevant covariates. 

#  covariates for hedonics from transfers
#  viewsheds
#  tsunami hazard
#  beach access
#  trailhead?
#  school
#  hospital
#  census: demographics, density, income, employment, . . .

library(tidyverse)
library(haven)
library(terra)
library(tidyterra)

# Transfers

dat_transfers = 
  "data/Transfers/Lincoln_Res_clean_v2026.dta" |> 
  read_dta() %>% 
  filter(year_sold %in% 1995:2024)

dat_transfers_less =
  dat_transfers |> 
  select(
    clip, 
    starts_with("parcel_"), 
    buyer_loc,
    year_sold,
    city,
    bedrooms,
    bathrooms,
    living_square_feet,
    universal_building_square_feet,
    acres,
    age,
    stories_number,
    multifamily) %>% 
  mutate(buyer_loc = ifelse(buyer_loc %in% 1:2, 0, ifelse(buyer_loc == 99, NA, 1))) # Is the buyer new to coastal OR? 

dat_transfers_spatial =
  dat_transfers_less |>
  # select(clip, starts_with("parcel_")) |>
  vect(geom = c("parcel_longitude", "parcel_latitude")) |>
  project("EPSG:3857")
  
# dat_owners_transactions_extract = 
#   dat_owners_parcels_join |> 
#   select(clip) |> 
#   terra::extract(dat_transactions_spatial) |> 
#   rename(clip_owner = clip)

# joins

# spatial joins

# minimal hedonic model

# better hedonic model

# segments by clustering

# test for substitution across segments
