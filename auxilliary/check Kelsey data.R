library(tidyverse)

Kelsey <- read_csv("auxilliary/Kelsey.csv")

Kelsey_positive_SD <- 
  Kelsey %>%
  group_by(Identifier) %>%
  mutate(SD = sd(Y_values[Condition == 0])) %>%
  filter(SD > 0)

EDES_ES <- batch_calc_ES(dat = Kelsey, 
                         grouping = Identifier,
                         condition = Condition, 
                         outcome = Y_values, 
                         session_number = NULL,
                         baseline_phase = 0, 
                         intervention_phase = 1,
                         scale = "other", 
                         intervals = NA, 
                         observation_length = NA, 
                         improvement = "increase",
                         ES = "SMD", std_dev = "baseline")
