pm_data = 
  read_csv("data/county_pm25.csv") %>%
  select(
    county_code = fips,
    PM2.5_concentration = pm25
  ) %>%
  group_by(county_code) %>% 
  summarise(
    PM2.5_concentration = mean(PM2.5_concentration)
  ) %>%
  # Keep track of whether PM data exists for counties.
  mutate(in_pm_data = TRUE)