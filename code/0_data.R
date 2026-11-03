# Data

#  Coastal Zone (OR)

dat_zone = 
  "data/0_Coastal_Zone_Boundary" %>% 
  vect %>% 
  mutate(Zone = 1, .keep = "none")

#  Elevation

dat_dem_30 = 
  "data/0_Elevation_30/Elevation.tif" %>% 
  rast %>% 
  project("EPSG:2992") %>% 
  crop(dat_zone %>% project("EPSG:2992"))

# dat_dem_10 = 

#  Coast (OR)

dat_coast = 
  "data/0_Coast.gdb" %>% 
  vect %>% 
  mutate(Coast = 1, .keep = "none") %>% 
  project("EPSG:2992") %>% 
  as.points

#   Sample vertices. This is useful for quick computation of distances, but wrong. 

dat_coast_less = dat_coast %>% slice_sample(prop = 0.01)

#  Beach Access

#  Tsunami Zone

dat_tsunami = 
  "data/0_Tsunami_Zone.gdb" %>% 
  vect %>% 
  filter(str_sub(Type, 1, 5) == "Local") %>% 
  mutate(Tsunami = 1, .keep = "none") %>% 
  project("EPSG:2992")

#  101
#  Cities (Portland, Salem, Corvallis, Eugene, Medford)

#  Cotality

#   Property Basic (OR)

#    Get all variable names.

vec_pb_names_all = 
  "data/0_Cotality/2_PB/OR_PB_SELECT.csv" %>% 
  read_csv %>% 
  pull(VARIABLE)

#    Get useful variable names.

vec_pb_names_use = 
  "data/0_Cotality/2_PB/OR_PB_SELECT.csv" %>% 
  read_csv %>% 
  filter(SELECT == 1) %>% 
  pull(VARIABLE)

#    Get data. 

dat_pb = 
  "data/0_Cotality/2_PB/OR_PB_08052026.csv" %>% 
  read_csv(col_names = FALSE) %>% 
  slice(-1:-2) %>% 
  set_names(vec_pb_names_all) %>% 
  select(all_of(vec_pb_names_use)) %>% 
  rename_with(~ str_replace_all(.x, " ", "_"))

#    Pick counties. 

#     Clatsop, Tillamook, Lincoln, Lane, Douglas, Coos, Curry

vec_counties_fips = 
  # c(7, 57, 41, 39, 19, 11, 15) %>% 
  c(41) %>% 
  as.character %>% 
  str_pad(3, "left", "0") %>% 
  paste0("41", .)

#    Explicate spatial data. Note that some observations are missing coordinates. 

dat_pb_spatial = 
  dat_pb %>% 
  filter(FIPS_CODE %in% vec_counties_fips) %>% 
  select(CLIP, starts_with("PARCEL_LEVEL")) %>% 
  drop_na(starts_with("PARCEL_LEVEL")) %>% 
  mutate(across(starts_with("PARCEL_LEVEL"), as.numeric)) %>% 
  vect(
    geom = c("PARCEL_LEVEL_LONGITUDE", "PARCEL_LEVEL_LATITUDE"), 
    crs = "+proj=longlat +datum=WGS84"
  ) %>% 
  project("EPSG:3857") %>% 
  intersect(dat_zone) %>% 
  select(-Zone)

#   Owner Transfer (OR)

#    Get all variable names.

vec_ot_names_all = 
  "data/0_Cotality/3_OT/OR_OT_SELECT.csv" %>% 
  read_csv %>% 
  mutate(VARIABLE = VARIABLE %>% str_replace_all("�", " ")) %>% 
  pull(VARIABLE)

#    Get useful variable names.

vec_ot_names_use = 
  "data/0_Cotality/3_OT/OR_OT_SELECT.csv" %>% 
  read_csv %>% 
  mutate(VARIABLE = VARIABLE %>% str_replace_all("�", " ")) %>% 
  filter(SELECT == 1) %>% 
  pull(VARIABLE)

#    Get data.

dat_ot = 
  "data/0_Cotality/3_OT/OR_OT_08052026.csv" %>% 
  read_csv(col_names = FALSE) %>% 
  slice(-1:-2) %>% 
  # Handle names.
  set_names(vec_ot_names_all) %>% 
  select(all_of(vec_ot_names_use)) %>% 
  rename_with(~ str_replace_all(.x, " ", "_")) %>% 
  # Reduce data.
  filter((SALE_DERIVED_DATE %>% str_sub(1, 4) %>% as.numeric) > 1999) %>% # Filter to transactions after 1999.
  filter(PRIMARY_CATEGORY_CODE == "A") %>% # Filter to arms-length transactions. 
  filter(INVESTOR_PURCHASE_INDICATOR == 0) %>% # Filter to transactions without noted investment intent.
  filter(DEED_CATEGORY_TYPE_CODE == "G") %>% # Filter to transactions with deeds.
  filter(is.na(MULTI_OR_SPLIT_PARCEL_CODE)) %>% # Filter to single-parcel transactions without splits.
  filter(RESIDENTIAL_INDICATOR == "Y") %>% # Filter to transactions for residential properties.
  filter(`PROPERTY_INDICATOR_CODE_-_STATIC` == "10") %>% # Filter to single-family residences.
  filter(INTERFAMILY_RELATED_INDICATOR == 0) %>% # Filter to transactions without family relationships.
  filter(SHORT_SALE_INDICATOR == 0) # Filter to transactions without family relationships.

#    Join to PB for spatial data. 

dat_ot_spatial = 
  dat_ot %>% 
  filter(FIPS_CODE %in% vec_counties_fips) %>% 
  select(CLIP, SALE_DERIVED_DATE) %>% 
  left_join(dat_pb_spatial, .) %>% 
  drop_na(SALE_DERIVED_DATE) %>% 
  project("EPSG:2992")

# Compute spatial covariates for transfers. 

# Elevation

dat_ot_elevation = 
  dat_ot_spatial %>% 
  extract(dat_dem_30)


# Viewsheds

# Distance to Coast (High Mean Tide via CUSP)

dat_ot_coast = 
  dat_ot_spatial %>% 
  nearest(dat_coast_less) %>% 
  select(Distance_Coast = distance)

# Tsunami Exposure

dat_ot_tsunami = dat_ot_spatial %>% intersect(dat_tsunami)




