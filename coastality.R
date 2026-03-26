# Check out real estate transactions for the Oregon coast. 

#  Packages

library(tidyverse)
library(haven)

#  Data

dat_lincoln = 
  "data/dta files (stata)/Lincoln_res_clean_v2026.dta" %>% 
  read_dta

dat_lincoln %>% names %>% as_tibble %>% View

# dat_lincoln$sale_amount %>% log %>% hist
# dat_lincoln$year_sold %>% hist

#  What's driving value increase on the coast? Value ~ Construction_Age + Coastal + Whatever
