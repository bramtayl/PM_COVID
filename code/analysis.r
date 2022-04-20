library(dplyr)
library(glmmTMB)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)
library(tigris)

# the first date we will analyze
first_analysis_date = as.Date("2020-03-22")

# a guess of the date they generated their figure
run_date_guess = as.Date("2020-09-07")

# the dates we will consider
dates_data = 
  tibble(date = seq(first_analysis_date, run_date_guess, by = "day"))

# an early date that Wu used to figure out which counties JHU is keeping track
# of
reference_date = as.Date("2020-03-30")

# commit to use in the CSSE data
# this one is from September 9, 2020
CSSE_commit = "968f9e627140a318cc5876b79fb959298da9a348"

source("code/counties.r")

source("code/age_groups.r")
source("code/census.r")
source("code/covid.r")
source("code/health.r")
source("code/hospitals.r")
source("code/pm.r")
source("code/policy.r")
source("code/temperature.r")

county_controls =
  county_data %>%
  left_join(pm_data) %>%
  left_join(temperature_data) %>%
  left_join(census_data) %>%
  left_join(health_data) %>%
  left_join(age_group_data) %>%
  left_join(hospitals_data) %>%
  mutate(
    # Fill 0 for missing
    hospital_beds = coalesce(hospital_beds, 0)
  ) %>%
  # Only use counties mentioned in both the PM data and the census data
  filter(in_pm_data & in_census_data) %>% 
  group_by(metro_code) %>%
  mutate(
    # There is a clear bug in Wu et al's code where they accidentally aggregate
    # population early
    buggy_population = 
      ifelse(
        primary,
        sum(population),
        population
      )
  ) %>%
  ungroup()

metro_controls = 
  county_controls %>%
  group_by(state_name, metro_code) %>%
  summarize(
    # Weight most things by population.
    PM2.5_concentration = weighted.mean(PM2.5_concentration, buggy_population),
    proportion_poor = weighted.mean(proportion_poor, buggy_population),
    median_house_value = weighted.mean(median_house_value, buggy_population),
    median_household_income = weighted.mean(median_household_income, buggy_population),
    proportion_owner_occupied = weighted.mean(proportion_owner_occupied, buggy_population),
    proportion_no_high_school = weighted.mean(proportion_no_high_school, buggy_population),
    proportion_black = weighted.mean(proportion_black, buggy_population),
    proportion_hispanic = weighted.mean(proportion_hispanic, buggy_population),
    proportion_65_and_over = weighted.mean(proportion_65_and_over, buggy_population),
    proportion_15_to_44 = weighted.mean(proportion_15_to_44, buggy_population),
    proportion_45_to_64 = weighted.mean(proportion_45_to_64, buggy_population),
    proportion_obsese = weighted.mean(proportion_obsese, buggy_population),
    proportion_smoke = weighted.mean(proportion_smoke, buggy_population),
    summer_mean_maximum_temperature = weighted.mean(summer_mean_maximum_temperature, buggy_population),
    summer_mean_relative_humidity = weighted.mean(summer_mean_relative_humidity, buggy_population),
    winter_mean_maximum_temperature = weighted.mean(winter_mean_maximum_temperature, buggy_population),
    winter_mean_relative_humidity = weighted.mean(winter_mean_relative_humidity, buggy_population),
    # Sum these last 2.
    hospital_beds = sum(hospital_beds),
    population = sum(population)
  ) %>%
  left_join(
    county_controls %>%
    # Maybe another bug in Wu et al's code
    # They used the quantile of the primary county to represent the whole metro.
    # This probably doesn't make a difference though, because all NYC boroughs 
    # are likely in the highest quantile
    filter(primary) %>%
    select(
      metro_code,
      population_density_quantile
    )
  )

all_data = 
  full_join(
    # Replicate the control data for all dates.
    metro_controls,
    dates_data,
    by = character()
  ) %>%
  left_join(expanded_covid_data) %>%
  filter(
    # Wu only used counties mentioned by in the covid data
    in_original_covid_data | in_no_early_covid
  ) %>%
  mutate(
    # Maybe another bug in Wu et al's code.
    # If the first covid data is after the date of analysis, set to 0.
    days_since_covid =
      ifelse(
        first_confirmed_date <= date,
        date - first_confirmed_date + 1,
        NA
      ) %>%
      coalesce(0)
  ) %>%
  left_join(policy_data) %>%
  mutate(
    # Maybe another bug in Wu et al's code
    # If the first social distancing mandate is after the date of analysis,
    # set to 0
    days_since_social_distancing = 
      as.numeric(run_date_guess - social_distancing_date) %>%
      coalesce(0)
  ) %>%
  # Only use states which we have policy information for
  filter(in_policy_data)
  
analyze_one_date = function(
  one_day_data,
  # Z-score for a 95% confidence interval
  confidence_interval_z_score = 1.96
) {
  print(first(one_day_data$date))
  glmmTMB.off.main = glmmTMB(
    cumulative_covid_deaths ~ 
      PM2.5_concentration + 
      factor(population_density_quantile) +
      scale(proportion_poor) +
      scale(log(median_house_value)) +
      scale(log(median_household_income)) +
      scale(proportion_owner_occupied) +
      scale(proportion_no_high_school) +
      scale(proportion_black) +
      scale(proportion_hispanic) +
      scale(proportion_65_and_over) +
      scale(proportion_15_to_44) +
      scale(proportion_45_to_64) +
      scale(days_since_social_distancing) +
      scale(days_since_covid) + 
      scale(hospital_beds/population) +
      scale(proportion_obsese) +
      scale(proportion_smoke) +
      scale(summer_mean_maximum_temperature) +
      scale(winter_mean_maximum_temperature) +
      scale(summer_mean_relative_humidity) +
      scale(winter_mean_relative_humidity) +
      offset(log(population)) +
      (1 | state_name),
    data = one_day_data,
    family = nbinom2,
    ziformula = ~ 1
  )
  coefficients = summary(glmmTMB.off.main)["coefficients"]$coefficients$cond
  log_estimate = coefficients["PM2.5_concentration", "Estimate"]
  standard_error = coefficients["PM2.5_concentration", "Std. Error"]
  tibble(
    # Exponentiate
    lower_bound = exp(log_estimate - confidence_interval_z_score * standard_error),
    estimate = exp(log_estimate),
    upper_bound = exp(log_estimate + confidence_interval_z_score * standard_error)
  )
}

all_data %>%
  group_by(date) %>%
  summarize(analyze_one_date(cur_data_all())) %>%
  write_csv("data/daily_estimates.csv")
