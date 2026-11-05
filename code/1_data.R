# Data

#  Contents:
#   Covariates
#   Cotality
#   Spatial Operations
#   Joins
#   Export

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

#  Land Cover

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
  select(CLIP, OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID, SALE_DERIVED_DATE) %>% 
  left_join(dat_pb_spatial, .) %>% 
  drop_na(SALE_DERIVED_DATE) %>% 
  project("EPSG:2992")

# Compute spatial covariates for transfers. 

#  Elevation

dat_ot_elevation = 
  dat_ot_spatial %>% 
  terra::extract(dat_dem_30, .) %>% 
  select(Elevation) %>% 
  mutate(Row = row_number()) %>% 
  left_join(
    dat_ot_spatial %>% mutate(Row = row_number()),
    .
  ) %>% 
  select(-Row)

#  Viewsheds

#   Viewsheds are computationally intensive; parallelization helps. 

#   Write data to disk for reading into parallel environments. 

writeRaster(dat_dem_30, "intermediate/dat_dem_30.tif", overwrite = TRUE)

dat_dem_30_area = 
  dat_dem_30 %>% 
  cellSize %T>% 
  writeRaster("intermediate/dat_dem_30_area.tif", overwrite = TRUE) # This returns feet?

dat_dem_30_area_ocean = 
  (dat_dem_30_area * (dat_dem_30 <= 1)) %T>% 
  writeRaster("intermediate/dat_dem_30_area_ocean.tif", overwrite = TRUE)

dat_ot_spatial %>% 
  crds %>% 
  as_tibble %>% 
  distinct %T>% 
  write_csv("intermediate/dat_ot_coordinates.csv")

#   Define functions to parallelize. 
  
fun_views_outer = 
  function(dat_point, path_dem, path_area, path_area_ocean){
    
    dat_dem = path_dem %>% rast
    dat_area = path_area %>% rast
    dat_area_ocean = path_area_ocean %>% rast
    
    dat = 
      dat_point %>% 
      # slice_head(n = 3) %>% # Testing. 
      mutate(Points = map2(x, y, c)) %>% 
      mutate(
        Viewsheds = 
          Points %>% 
          map(
            fun_views_inner, 
            dat_dem = dat_dem,
            dat_area = dat_area,
            dat_area_ocean = dat_area_ocean
          )
      ) %>% 
      select(-Points) %>% 
      unnest(Viewsheds)
    
  }

fun_views_inner = 
  function(vec_point, dat_dem, dat_area, dat_area_ocean){
    
    dat_view = viewshed(dat_dem, vec_point)
    
    dat_view_area = dat_view * dat_area
    
    dat_view_ocean = dat_view * dat_area_ocean
    
    val_total = as.numeric(global(dat_view_area, "sum", na.rm = TRUE) / 27878400)
    val_ocean = as.numeric(global(dat_view_ocean,  "sum", na.rm = TRUE) / 27878400)
    
    dat_out = tibble(Viewshed_Total = val_total, Viewshed_Ocean = val_ocean)
    
    return(dat_out)
    
  }

#   Set up parallelization. 

par_cores = 16

plan(multisession, workers = par_cores)

set.seed(0112358)

#   Compute viewsheds in parallel. 

Sys.time()

dat_ot_views = 
  tibble(Group = 1:par_cores) %>% 
  mutate(
    Data = 
      Group %>% 
      future_map(
        ~ "intermediate/dat_ot_coordinates.csv" %>% 
          read_csv %>% 
          mutate(Group = (row_number() * par_cores / nrow(.)) %>% ceiling) %>% 
          filter(Group == .x) %>% 
          select(-Group) %>% 
          fun_views_outer(
            path_dem = "intermediate/dat_dem_30.tif",
            path_area = "intermediate/dat_dem_30_area.tif",
            path_area_ocean = "intermediate/dat_dem_30_area_ocean.tif"
          ), 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  select(Data) %>% 
  unnest(Data) # %>% 
  # left_join(
  #   dat_ot_spatial %>% 
  #     crds %>% 
  #     as_tibble %>% 
  #     mutate(Row = row_number()), 
  #   .
  # )

Sys.time()

# ~6 hours for all observations?

#  Distance to Coast (High Mean Tide via CUSP)

dat_ot_coast = 
  dat_ot_spatial %>% 
  nearest(dat_coast_less) %>% 
  select(Distance_Coast = distance)

#  Tsunami Exposure

dat_ot_tsunami = dat_ot_spatial %>% intersect(dat_tsunami)

# Join covariates to transfers.

#  Remember to reduce OT to some smaller subset that might actually support estimation. 
#  Remember to add some subset of PB covariates to OT, noting the snapshot/panel problem. 

dat_ot_augment = 
  dat_ot_spatial %>%
  as_tibble %>% 
  # left_join(
  #   dat_ot,
  #   by = c("CLIP", "OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID", "SALE_DERIVED_DATE")
  # ) %>% 
  left_join(
    dat_ot_elevation %>% as_tibble,
    by = c("CLIP", "OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID", "SALE_DERIVED_DATE")
  ) %>% 
  bind_cols(dat_ot_views %>% as_tibble) %>%
  bind_cols(dat_ot_coast %>% as_tibble) %>% 
  left_join(
    dat_ot_tsunami %>% as_tibble,
    by = c("CLIP", "OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID", "SALE_DERIVED_DATE")
  ) %>% 
  mutate(Tsunami = replace_na(Tsunami, 0)) %T>% 
  write_csv("intermediate/dat_panel.csv")
