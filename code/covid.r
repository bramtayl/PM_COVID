get_covid_data <- function(
  a_date,
  folder = "data",
  commit = CSSE_commit
) {
  read.csv(file.path(
    folder,
    commit,
    paste0(format(a_date, format = "%m-%d-%Y"), ".csv")
  )) %>%
  select(
    county_code = FIPS, 
    county_name = Admin2, 
    cumulative_covid_deaths = Deaths,
    confirmed_covid_cases = Confirmed,
    country_abbreviation = Country_Region
  ) %>%
  filter(country_abbreviation == "US" & !is.na(county_code)) %>%
  select(-country_abbreviation)
}

daily_covid_data =
  dates_data %>%
  group_by(date) %>%
  summarize(get_covid_data(date)) %>%
  ungroup() %>%
  mutate(
    # Keep track of whether covid data exists for counties.
    in_original_covid_data = TRUE
  )

# JHU uses New York City for the aggregated 5 boroughs
# Figure out which dates JHU aggregated the boroughs on
covid_aggregation_data = 
  daily_covid_data %>%
  group_by(date) %>%
  summarize(
    aggregated_by_metro = "New York City" %in% county_name
  )

# Use data from when JHU aggregated to metro as is.
only_metro_covid_data =
  daily_covid_data %>%
  select(-county_name) %>%
  inner_join(
    covid_aggregation_data %>%
    filter(aggregated_by_metro)
  ) %>%
  select(-aggregated_by_metro) %>%
  rename(metro_code = county_code)

metro_covid_data =
  # Aggregate the NYC data after JHU stopped aggregating it.
  daily_covid_data %>%
  select(-county_name) %>%
  inner_join(
    covid_aggregation_data %>%
    filter(!aggregated_by_metro)
  ) %>%
  select(-aggregated_by_metro) %>%
  left_join(
    county_data %>%
    select(metro_code, county_code)
  ) %>%
  group_by(date, metro_code) %>%
  summarize(
    cumulative_covid_deaths = sum(cumulative_covid_deaths),
    confirmed_covid_cases = sum(confirmed_covid_cases),
    in_original_covid_data = any(in_original_covid_data)
  ) %>%
  ungroup() %>%
  bind_rows(only_metro_covid_data)

# Wu found counties with no reported covid, that might not be mentioned on later
# dates.
no_early_covid = 
  metro_covid_data %>%
  filter(
    date == reference_date & 
    confirmed_covid_cases == 0 &
    cumulative_covid_deaths == 0
  ) %>%
  select(metro_code) %>%
  mutate(in_no_early_covid = TRUE)

first_covid_data <-
  metro_covid_data %>%
  filter(confirmed_covid_cases > 0) %>%
  group_by(metro_code) %>%
  arrange(date) %>%
  # Find date of first case in each county
  slice(1) %>%
  ungroup %>%
  select(
    metro_code,
    first_confirmed_date = date
  )

expanded_covid_data = 
  full_join(
    metro_covid_data,
    # Replicate for all dates
    # Otherwise, some dates will be missing after the full join
    full_join(
      no_early_covid,
      dates_data,
      by = character()
    )
  ) %>%
  left_join(first_covid_data) %>%
  mutate(
    cumulative_covid_deaths =
      # Fill in zero if the county was mentioned early but not later
      ifelse(
        is.na(in_original_covid_data) & in_no_early_covid,
        0,
        cumulative_covid_deaths
      )
  )