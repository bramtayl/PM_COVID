temperature_data = 
  read_csv(
    "data/temp_seasonal_county.csv",
    col_types = list(
      fips = col_double()
    )
  ) %>%
  select(
    county_code = fips,
    winter_mean_maximum_temperature = winter_tmmx,
    summer_mean_maximum_temperature = summer_tmmx,
    winter_mean_relative_humidity = winter_rmax,
    summer_mean_relative_humidity = summer_rmax
  ) %>%
  group_by(county_code) %>% 
  summarise(
    winter_mean_maximum_temperature = mean(winter_mean_maximum_temperature),
    summer_mean_maximum_temperature = mean(summer_mean_maximum_temperature),
    winter_mean_relative_humidity = mean(winter_mean_relative_humidity),
    summer_mean_relative_humidity = mean(summer_mean_relative_humidity)
  )