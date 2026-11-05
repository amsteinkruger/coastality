# Visualization

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

#  Buyer States by Year

vis = 
  dat %>% 
  drop_na(SALE_DERIVED_DATE_YEAR, BUYER_MAILING_STATE_BINARY) %>% 
  mutate(
    Year = SALE_DERIVED_DATE_YEAR, 
    Origin = ifelse(BUYER_MAILING_STATE_BINARY, "Other", "OR")
  ) %>% 
  group_by(Year, Origin) %>% 
  summarize(Count = n()) %>% 
  group_by(Year) %>% 
  mutate(Proportion = Count / sum(Count)) %>% 
  ungroup %>% 
  pivot_longer(c(Count, Proportion)) %>% 
  ggplot() + 
  geom_col(aes(x = Year, y = value, fill = Origin), width = 0.75) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = NULL, fill = "Buyer Origin") +
  facet_wrap(~ name, scales = "free_y") +
  theme_pubr()

ggsave(
  "results/vis.png",
  vis,
  dpi = 300,
  width = 7,
  height = 4
)
