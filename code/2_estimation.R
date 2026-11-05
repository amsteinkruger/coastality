# Estimation

#  Data

dat = 
  "intermediate/dat_panel.csv" %>% 
  read_csv %>% 
  mutate(
    SALE_AMOUNT_LOG = SALE_AMOUNT %>% log,
    SALE_DERIVED_DATE_YEAR = SALE_DERIVED_DATE %>% str_sub(1, 4) %>% as.numeric,
    SALE_DERIVED_DATE_MONTH = SALE_DERIVED_DATE %>% str_sub(5, 6) %>% as.numeric,
    SALE_DERIVED_DATE_DAY = SALE_DERIVED_DATE %>% str_sub(7, 8) %>% as.numeric,
    BUYER_MAILING_STATE_BINARY = (BUYER_MAILING_STATE == "OR"),
    BUYER_OCCUPANCY_CODE_BINARY = (BUYER_OCCUPANCY_CODE %in% c("M", "O", "S"))
  )

#  Models

#   OLS, Minimal

mod_1 = feols(SALE_AMOUNT_LOG ~ BUYER_MAILING_STATE_BINARY, dat)

#   OLS, Controls

mod_2 = 
  feols(
    SALE_AMOUNT_LOG ~ 
      BUYER_MAILING_STATE_BINARY + 
      `ACTUAL_YEAR_BUILT_-_STATIC` +
      CASH_PURCHASE_INDICATOR +
      BUYER_OCCUPANCY_CODE_BINARY +
      LAND_SQUARE_FOOTAGE +
      `LIVING_SQUARE_FEET_-_ALL_BUILDINGS` +
      `BEDROOMS_-_ALL_BUILDINGS` +
      `TOTAL_BATHROOMS_-_ALL_BUILDINGS` +
      Elevation + 
      Viewshed_Total +
      Viewshed_Ocean + 
      Distance_Coast + 
      Tsunami,
    dat
  )

#   OLS, Controls, FE

mod_3 = 
  feols(
    SALE_AMOUNT_LOG ~ 
      BUYER_MAILING_STATE_BINARY + 
      `ACTUAL_YEAR_BUILT_-_STATIC` +
      CASH_PURCHASE_INDICATOR +
      BUYER_OCCUPANCY_CODE_BINARY +
      LAND_SQUARE_FOOTAGE +
      `LIVING_SQUARE_FEET_-_ALL_BUILDINGS` +
      `BEDROOMS_-_ALL_BUILDINGS` +
      `TOTAL_BATHROOMS_-_ALL_BUILDINGS` +
      Elevation + 
      Viewshed_Total +
      Viewshed_Ocean + 
      Distance_Coast + 
      Tsunami |
      SALE_DERIVED_DATE_YEAR,
    dat
  )

# Tables

modelsummary(list(mod_1, mod_2, mod_3), stars = TRUE, output = "results/table.docx")
