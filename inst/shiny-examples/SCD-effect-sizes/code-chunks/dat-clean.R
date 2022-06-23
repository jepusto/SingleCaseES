# clean data
library(dplyr)

dat <-
  dat %>%
  group_by({user_grouping}) %>%
  arrange({user_grouping}, {user_session_number}) %>%
  mutate(phase_pair_calculated = calc_phase_pairs({user_condition})) %>%
  ungroup()

