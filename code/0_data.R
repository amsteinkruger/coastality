# Data

#  Cotality, Owner Transfer

#   Get all variable names.

vec_ot_names_all = 
  "data/0_Cotality/3_OT/OR_OT_SELECT.csv" %>% 
  read_csv %>% 
  mutate(VARIABLE = VARIABLE %>% str_replace_all("�", " ")) %>% 
  pull(VARIABLE)

#   Get useful variable names.

vec_ot_names_use = 
  "data/0_Cotality/3_OT/OR_OT_SELECT.csv" %>% 
  read_csv %>% 
  mutate(VARIABLE = VARIABLE %>% str_replace_all("�", " ")) %>% 
  filter(SELECT == 1) %>% 
  pull(VARIABLE)

#   Get data.

dat_ot = 
  "data/0_Cotality/3_OT/OR_OT_08052026.csv" %>% 
  read_csv(col_names = FALSE) %>% 
  slice(-1:-2) %>% 
  set_names(vec_ot_names_all) %>% 
  select(all_of(vec_ot_names_use)) %>% 
  rename_with(~ str_replace_all(.x, " ", "_"))
