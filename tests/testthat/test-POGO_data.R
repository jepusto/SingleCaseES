library(dplyr)
library(tidyr)
library(testthat)

# Test Crozier 2005 Calculations

test_that("POGO calculation agrees with article for Crozier 2005", {
  
  # We should replace the load() with the following
  # data("Crozier2005", package = "SingleCaseES")
  load("data/Crozier2005.Rdata")
  
  Crozier2005_res <- data.frame(Phase_Shift = c("A1 to B1", "B1 to A2", "A2 to B2"), 
                                Article_ES = c(79.1, 64.2, 97.92))
  
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
  
  expect_equal(round(Crozier_Compare$Article_ES,0), round(Crozier_Compare$Est, 0))
  
})

test_that("POGO calculation agrees with article for English1997", {
## Test English 1997 Calculations
load("data/English1997.Rdata")

English1997_res <- data.frame(case = c("Sue", "Don", "Jake", "Pete"),
                              Article_ES = c(79.1, 69, 35.8, 108.2))

English1997_PoGO <- batch_calc_ES(dat = English1997,
                                  grouping = c(case),
                                  condition = phase,
                                  outcome = score,
                                  ES = c("PoGO"),
                                  goal = 21.25)

English_Compare <- left_join(English1997_res, English1997_PoGO, by = "case")

expect_equal(round(English_Compare$Article_ES,0), round(English_Compare$Est, 0))

})

test_that("POGO calculation agrees with article for Facon 2008", {
## Test Facon 2008 Calculations
load("data/Facon2008.Rdata")

Facon2008_res <- data.frame(Phase_Shift = c("A to B", "B to C", "C to D", "D to E", "E to F", "F to G", "G to H", "H to I"),
                            Article_ES = c(4.6, 12.4, 23.3, 43.5, 62.1, 77.9, 87.4, 106.2))

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
                        baseline_phase = "A",
                        intervention_phase = "C",
                        ES = c("PoGO"),
                        goal = 70)

shift3_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "A",
                        intervention_phase = "D",
                        ES = c("PoGO"),
                        goal = 70)

shift4_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "A",
                        intervention_phase = "E",
                        ES = c("PoGO"),
                        goal = 70)

shift5_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "A",
                        intervention_phase = "F",
                        ES = c("PoGO"),
                        goal = 70)

shift6_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "A",
                        intervention_phase = "G",
                        ES = c("PoGO"),
                        goal = 70)

shift7_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "A",
                        intervention_phase = "H",
                        ES = c("PoGO"),
                        goal = 70)

shift8_f <- batch_calc_ES(dat = Facon2008,
                        condition = phase,
                        outcome = score,
                        baseline_phase = "A",
                        intervention_phase = "I",
                        ES = c("PoGO"),
                        goal = 70)

Facon2008_PoGO <- rbind(shift1_f, shift2_f, shift3_f, shift4_f, shift5_f, shift6_f, shift7_f, shift8_f)

Facon_Compare <- cbind(Facon2008_res, Facon2008_PoGO)

expect_equal(round(Facon_Compare$Article_ES,1), round(Facon_Compare$Est, 1))
})

test_that("POGO calculation agrees with article for Olszewski 2017", {
## Test Olszewski 2017 Calculations
load("data/Olszewski2017.RData")

Olszewski2017_res <- data.frame(behavior = c("Blends", "Segmenting", "First Part ID", "First Sound ID"),
                                Article_ES = c(23.0, 85.3, 100, 26))

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

expect_equal(round(Olszewski_Compare$Article_ES,1), round(Olszewski_Compare$Est, 1))
})

test_that("POGO calculation agrees with article for Spencer 2012", {
  load("data/Spencer2012.Rdata")
  
  Spencer2012_res <- data.frame(Observation = c("Child A1", "Child A2", "Child A3", "Child B1", "Child B2", "Child B3", "Child C1", "Child C2", "Child C3"),
                                    Article_ES = c(33.333, 50, 46.875, 58.826, 44.444, 14.711, 72.222, 36.111, 27.778))
  
  spencer2012_long <- Spencer2012 %>%
    group_by(Observation) %>%
    pivot_longer(cols = c(Pre, Post), names_to = "Phase", values_to = "Score")
  
  Spencer2012_PoGO <- batch_calc_ES(dat = spencer2012_long,
                grouping = Observation,
                condition = Phase,
                outcome = Score,
                baseline_phase = "Pre",
                intervention_phase = "Post",
                ES = c("PoGO"),
                goal = 4)
  
  Spencer2012_Compare <- left_join(Spencer2012_res, Spencer2012_PoGO, by = "Observation")
  
  expect_equal(round(Spencer2012_Compare$Article_ES,1), round(Spencer2012_Compare$Est, 1))
  
})

test_that("POGO calculation agrees with article for Kelley 2015", {
  load("data/Kelley2015.Rdata")
  
  kelley_dat <- Kelley2015 %>%
    pivot_longer(cols = c(pre, post), names_to = "Phase", values_to = "Score") %>%
    filter(condition == "treatment")
})
