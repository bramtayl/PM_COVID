policy_data = 
  read_csv("data/state_policy0410.csv", n_max = 51) %>%
  rename(social_distancing_date = `Stay at home/ shelter in place`) %>%
  select(
    state_name = State,
    social_distancing_date
  ) %>%
  mutate(
    # "0" means missing in this data
    social_distancing_date =
      mdy(ifelse(social_distancing_date == "0", NA, social_distancing_date)),
    in_policy_data = TRUE
  )
