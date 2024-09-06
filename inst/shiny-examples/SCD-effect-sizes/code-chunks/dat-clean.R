# clean data
library(dplyr)

dat <-
  dat %>%
  group_by({user_grouping}) %>%
  mutate(
    phase_pair_calculated = calc_phase_pairs({user_condition}, session = {user_session_number})
  ) %>%
  ungroup()

