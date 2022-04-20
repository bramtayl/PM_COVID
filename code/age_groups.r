age_groups = 
  tibble(
    file = c(
      "data/county_old_mortality.txt",
      "data/county_014_mortality.txt",
      "data/county_1544_mortality.txt",
      "data/county_4564_mortality.txt"
    ),
    variable = c(
      "proportion_65_and_over",
      "proportion_14_and_under",
      "proportion_15_to_44",
      "proportion_45_to_64"
    )
  ) %>%
  group_by(variable) %>%
  summarize(
    # use read.table instead of read_delim
    # handles misformatted files better
    read.table(file, sep = "", header = TRUE) %>%
    select(
      county_code = County.Code,
      age_group_population = Population
    )
  )

age_group_data = 
  read.table("data/county_base_mortality.txt", sep = "", header = TRUE) %>%
  select(
    county_code = County.Code,
    whole_population = Population
  ) %>%
  left_join(age_groups) %>%
  mutate(
    # fill 0 for missing
    proportion = coalesce(age_group_population / whole_population, 0),
    # for some counties, no age group data is available
    variable = coalesce(variable, "missing_age_groups")
  ) %>%
  select(variable, county_code, proportion) %>%
  # fill 0 for missing
  pivot_wider(
    names_from = variable, 
    values_from = proportion, 
    values_fill = 0
  ) %>%
  select(-missing_age_groups)