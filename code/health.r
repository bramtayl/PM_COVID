health_data = 
  read_csv(
    "data/analytic_data2020.csv",
    col_types = list(
      fipscode = col_double()
    ),
    # The first row just has descriptions.
    # Might be nice to read them in and use them for selection instead.
    skip = 1,
    # Parse the whole thing first to verify column types
    guess_max = 3194
  ) %>%
  select(
    county_code = fipscode,
    proportion_obsese = v011_rawvalue,
    proportion_smoke = v009_rawvalue
  )