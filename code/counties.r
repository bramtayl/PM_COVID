options(tigris_use_cache = TRUE)

state_data = 
  states() %>%
  as_tibble() %>%
  select(
    state_code = GEOID,
    state_name = NAME
  )

# JHU reports aggregates for NYC on some dates
# even though all the boroughs are separate counties.
# To aggregate NYC, make a new unit of observation called a "metro".
# A metro is usually the same as the county, but for NYC, the metro includes
# all the boroughs
NYC_counties = 
  tibble(
    state_name = "New York",
    county_name = c("New York", "Bronx", "Kings", "Queens", "Richmond"),
    # Manhattan is the primary county that subsumes the others
    primary = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    # The same as the fips code for manhattan
    metro_code = 36061
  )

county_data = 
  # Use an earlier year because the PM data references a county that doesn't
  # exist anymore.
  counties(year = 2014) %>%
  as_tibble() %>%
  select(
    county_code = GEOID,
    county_name = NAME,
    state_code = STATEFP
  ) %>%
  as_tibble() %>%
  mutate(county_code = as.numeric(county_code)) %>%
  # Replace the state codes with names.
  left_join(state_data) %>%
  select(-state_code) %>%
  # Add in metro data.
  left_join(NYC_counties) %>%
  mutate(
    # All counties are primary except in NYC
    primary = coalesce(primary, TRUE),
    # The metro code equals the county code everywhere but NYC
    metro_code = coalesce(metro_code, county_code),
  )