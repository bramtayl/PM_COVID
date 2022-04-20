census_data = 
  read_csv(
    "data/census_county_interpolated.csv",
    col_types = list(
      fips = col_double()
    )
  ) %>%
  select(
    county_code = fips,
    year,
    population,
    proportion_poor = poverty,
    population_density = popdensity,
    median_house_value = medianhousevalue,
    proportion_black = pct_blk,
    median_household_income = medhouseholdincome,
    proportion_owner_occupied = pct_owner_occ,
    proportion_hispanic = hispanic,
    proportion_no_high_school = education
  ) %>%
  filter(year == 2016) %>%
  mutate(
    population_density_quantile = cut(
      population_density,
      quantile(
          population_density,
          probs = seq(0, 1, 0.2)
      ),
      # so the lowest doesn't get left out
      include.lowest = TRUE
    ),
    # keep track of whether census data exists for counties
    in_census_data = TRUE
  )