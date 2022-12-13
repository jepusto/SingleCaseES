library(dplyr)
library(tidyr)

## Test Crozier 2005 Calculations
load("~/SingleCaseES/data/Crozier2005.Rdata")

Crozier2005_res <- as.data.frame(cbind(c("A1 to B1", "B1 to A2", "A2 to B2"), 
                                       c(79.1, 64.2, 97.92))) %>%
  rename(Phase_Shift = V1,
         Article_ES = V2)

shift1 <- batch_calc_ES(dat = Crozier2005,
                        condition = phase,
                        baseline_phase = "A1",
                        intervention_phase = "B1",
                        outcome = score,
                        ES = c("PoGO"),
                        goal = 0)

shift2 <- batch_calc_ES(dat = Crozier2005,
                        condition = phase,
                        baseline_phase = "B1",
                        intervention_phase = "A2",
                        outcome = score,
                        ES = c("PoGO"),
                        goal = 11.17)

shift3 <- batch_calc_ES(dat = Crozier2005,
                        condition = phase,
                        baseline_phase = "A2",
                        intervention_phase = "B2",
                        outcome = score,
                        ES = c("PoGO"),
                        goal = 0)

Crozier2005_PoGO <- rbind(shift1, shift2, shift3) 

Crozier_Compare <- cbind(Crozier2005_res, Crozier2005_PoGO)

## Test English 1997 Calculations
load("~/SingleCaseES/data/English1997.Rdata")

English1997_res <- as.data.frame(cbind(c("Sue", "Don", "Jake", "Pete"),
                                       c(79.1, 69, 35.8, 108.2))) %>%
  rename(case = V1,
         Article_ES = V2)

English1997_PoGO <- batch_calc_ES(dat = English1997,
                                  grouping = c(case),
                                  condition = phase,
                                  outcome = score,
                                  ES = c("PoGO"),
                                  goal = 21.25)

English_Compare <- left_join(English1997_res, English1997_PoGO, by = "case")


## Test Facon 2008 Calculations
load("~/SingleCaseES/data/Facon2008.Rdata")

Facon2008_res <- as.data.frame(cbind(c("A to B", "B to C", "C to D", "D to E", "E to F", "F to G", "G to H", "H to I"),
                                     c(4.6, 12.4, 23.3, 43.5, 62.1, 77.9, 87.4, 106.2))) %>%
  rename(Phase_Shift = V1,
         Article_ES = V2)

shift1_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "A",
                        intervention_phase = "B",
                        ES = c("PoGO"),
                        goal = 70)

shift2_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "B",
                        intervention_phase = "C",
                        ES = c("PoGO"),
                        goal = 70)

shift3_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "C",
                        intervention_phase = "D",
                        ES = c("PoGO"),
                        goal = 70)

shift4_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "D",
                        intervention_phase = "E",
                        ES = c("PoGO"),
                        goal = 70)

shift5_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "E",
                        intervention_phase = "F",
                        ES = c("PoGO"),
                        goal = 70)

shift6_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "F",
                        intervention_phase = "G",
                        ES = c("PoGO"),
                        goal = 70)

shift7_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "G",
                        intervention_phase = "H",
                        ES = c("PoGO"),
                        goal = 70)

shift8_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "H",
                        intervention_phase = "I",
                        ES = c("PoGO"),
                        goal = 70)

Facon2008_PoGO <- rbind(shift1_f, shift2_f, shift3_f, shift4_f, shift5_f, shift6_f, shift7_f, shift8_f)

Facon_Compare <- cbind(Facon2008_res, Facon2008_PoGO)

## Test Olszewski 2017 Calculations
load("~/SingleCaseES/data/Olszewski2017.RData")

Olszewski2017_res <- as.data.frame(cbind(c("Blends", "Segmenting", "First Part ID", "First Sound ID"),
                                         c(23.0, 85.3, 100, 26))) %>%
  rename(behavior = V1,
         Article_ES = V2)

Olszewski2017_PoGO <- batch_calc_ES(dat = Olszewski2017,
                                    grouping = c(behavior),
                                    condition = phase,
                                    outcome = score,
                                    ES = c("PoGO"),
                                    goal = 20)

Segmenting <- Olszewski2017 %>%
  filter((session %in% 1:8 | session %in% 14:18) & behavior == "Segmenting")

Segmenting_PoGO <- batch_calc_ES(dat = Segmenting,
                                 condition = phase,
                                 outcome = score,
                                 ES = c("PoGO"), 
                                 goal = 20) %>%
  mutate(behavior = "Segmenting") %>%
  select(behavior, ES, Est, SE, CI_lower, CI_upper)

FirstPart <- Olszewski2017 %>%
  filter((session %in% 1:11 | session %in% 14:18) & behavior == "First Part ID")

FirstPart_PoGO <- batch_calc_ES(dat = FirstPart,
                                condition = phase,
                                outcome = score,
                                ES = c("PoGO"),
                                goal = 20) %>%
  mutate(behavior = "First Part ID") %>%
  select(behavior, ES, Est, SE, CI_lower, CI_upper)

Olszewski_Compare <- Olszewski2017_PoGO %>%
  filter(behavior == "Blends" | behavior == "First Sound ID") %>%
  rbind(Segmenting_PoGO, FirstPart_PoGO) %>%
  left_join(Olszewski2017_res, by = "behavior")
