hospitals_data = 
  read_csv(
    "data/hospitals.csv",
    na = c("NA", "NOT AVAILABLE"),
    col_types = list(
      COUNTYFIPS = col_double()
    ),
  ) %>%
  select(
    county_code = COUNTYFIPS,
    hospital_beds = BEDS
  ) %>%
  mutate(
    # Negative numebrs are placeholders for missing in this data.
    hospital_beds = ifelse(hospital_beds < 0, NA, hospital_beds)
  ) %>%
  group_by(county_code) %>%
  summarise(
    # Ignore missing values.
    hospital_beds = sum(hospital_beds, na.rm = TRUE)
  )