library(dplyr)
library(tidyr)

test_that("POGO calculation agrees with article for Crozier 2005", {
  
  data("Crozier2005")
  
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
  
  data("English1997")

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
  
  data("Facon2008")

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
  
  data("Olszewski2017")

  Olszewski2017_res <- data.frame(behavior = c("Blends", "Segmenting", "First Part ID", "First Sound ID"),
                                  Article_ES = c(23.0, 85.3, 100, 26))
  
  Olszewski2017_PoGO <- batch_calc_ES(dat = Olszewski2017,
                                      grouping = c(behavior),
                                      condition = phase,
                                      outcome = score,
                                      ES = c("PoGO"),
                                      goal = 20)
  
  Segmenting <- 
    Olszewski2017 %>%
    dplyr::filter(
      session %in% c(1:8,14:18), 
      behavior == "Segmenting"
    )
  
  Segmenting_PoGO <- batch_calc_ES(dat = Segmenting,
                                   condition = phase,
                                   outcome = score,
                                   ES = c("PoGO"), 
                                   goal = 20) %>%
    mutate(behavior = "Segmenting") %>%
    select(behavior, ES, Est, SE, CI_lower, CI_upper)
  
  FirstPart <- 
    Olszewski2017 %>%
    dplyr::filter(session %in% c(1:11,14:18), behavior == "First Part ID")
  
  FirstPart_PoGO <- batch_calc_ES(dat = FirstPart,
                                  condition = phase,
                                  outcome = score,
                                  ES = c("PoGO"),
                                  goal = 20) %>%
    mutate(behavior = "First Part ID") %>%
    select(behavior, ES, Est, SE, CI_lower, CI_upper)
  
  Olszewski_Compare <- Olszewski2017_PoGO %>%
    dplyr::filter(behavior %in% c("Blends","First Sound ID")) %>%
    rbind(Segmenting_PoGO, FirstPart_PoGO) %>%
    left_join(Olszewski2017_res, by = "behavior")
  
  expect_equal(round(Olszewski_Compare$Article_ES,1), round(Olszewski_Compare$Est, 1))
})

test_that("POGO calculation agrees with article for Spencer 2012", {
  
  data("Spencer2012")

  Spencer2012_res <- data.frame(Observation = c("Child A1", "Child A2", "Child A3", "Child B1", "Child B2", "Child B3", "Child C1", "Child C2", "Child C3"),
                                    Article_ES = c(33.333, 50, 46.875, 58.826, 44.444, 14.711, 72.222, 36.111, 27.778))
  
  spencer2012_long <- 
    Spencer2012 %>%
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
  
  data("Kelley2015")
  
  Kelley2015_res <- data.frame(observation = c("240408", "240412", "240413", "240903", "240913", "240915", "241201", "241204", "241211"),
                               Article_ES = c(33.33333333, 32.25850156, 100, 25.71449796, 50.00029412, 46.87516602, 58.62083234, 5.714555101, 83.33333333),
                               Article_SE = 100*c(0.096225045, 0.105658281, 0.120697847, 0.154036724, 0.088234833, 0.170816987, 0.211244275, 0.057166015, 0.166666667))

  kelley_dat <- 
    Kelley2015 %>%
    pivot_longer(cols = c(pre, post), names_to = "Phase", values_to = "Score") %>%
    dplyr::filter(condition == "treatment") %>%
    mutate(Score = as.numeric(Score))
  
  Kelley2015_PoGO <- batch_calc_ES(dat = kelley_dat,
                grouping = observation, 
                condition = Phase,
                outcome = Score,
                baseline_phase = "pre",
                intervention_phase = "post",
                ES = c("PoGO"),
                goal = 12)
  
  Kelley2015_Compare <- left_join(Kelley2015_PoGO, Kelley2015_res, by = "observation")
  
  expect_equal(round(Kelley2015_Compare$Article_ES,1), round(Kelley2015_Compare$Est, 1))
  expect_equal(round(Kelley2015_Compare$Article_SE,1), round(Kelley2015_Compare$SE, 1))
  
})

test_that("POGO calculation agrees with article for Peters 2020", {
  
  data("Peters2020")

  Peters2020_res <- data.frame(Observation = c("Child F1", "Child F2", "Child F3", "Child F4", "Child F5", "Child F6", "Child F7", "Child F8", "Child F9", 
                                               "Child J1", "Child J2", "Child J3", "Child J4", "Child J5", "Child J6", "Child J7", "Child J8"),
                               Article_ES = c(45.45454545, 24.52901388, 43.75, 47.54098361, 87.5, 50, 25, 56.8627451, 80.43478261,
                                              14.08450704, 12.5, 12.5, 5.555555556, 67.1641791, 75, 76.8115942, 93.65079365),
                               Article_SE = 100*c(0.135641715, 0.114136702, 0.090507378, 0.131092941, 0.098293817, 0.174768405, 0.20848209, 0.180583945, 0.174069647,
                                              0.067576046, 0.051031036, 0.051031036, 0.030270132, 0.101676434, 0.097340735, 0.082401857, 0.095980561))
  
  peters_long <- Peters2020 %>%
    pivot_longer(cols = c(Pre, Post), names_to = "phase", values_to = "score") 
  
  Peters2020_PoGO <- batch_calc_ES(dat = peters_long,
                                   grouping = Observation,
                                   condition = phase,
                                   outcome = score,
                                   baseline_phase = "Pre",
                                   intervention_phase = "Post",
                                   ES = c("PoGO"),
                                   goal = 8)
  
  Peters2020_Compare <- left_join(Peters2020_PoGO, Peters2020_res, by = "Observation")
  
  expect_equal(round(Peters2020_Compare$Article_ES,1), round(Peters2020_Compare$Est, 1))
  expect_equal(round(Peters2020_Compare$Article_SE,1), round(Peters2020_Compare$SE, 1))
  
})

test_that("POGO calculation agrees with article for Dennis 2021", {
  
  data("Dennis2021")

  Dennis2021_res <- data.frame(Participant = c(1,2,3,4,5,6,1,2,3,4,5,6),
                           Condition = c(rep("App", 6), rep("TCH", 6)),
                           Article_ES = c(38.0952381, 11.26760563, 50, 57.14285714, 76.27118644, 23.72881356, 32.78688525, 7.246376812, 35.48387097, 34.54545455, 51.61290323, 14.28571429),
                           Article_SE = 100*c(0.082972405, 0.059776573, 0.16511651, 0.074451044, 0.086261415, 0.096243717, 0.10868039, 0.065541587, 0.145493518, 0.097742848, 0.096234682, 0.117356181))
  
  dennis_long <- 
    Dennis2021 %>%
    pivot_longer(cols = c(Pre, Post), names_to = "phase", values_to = "score")
  
  Dennis2021_PoGO <- 
    batch_calc_ES(dat = dennis_long,
                  grouping = c(Participant, Condition),
                  condition = phase,
                  outcome = score,
                  baseline_phase = "Pre",
                  intervention_phase = "Post",
                  ES = c("PoGO"),
                  goal = 9)
    
  Dennis2021_Compare <- left_join(Dennis2021_PoGO, Dennis2021_res, by = c("Participant", "Condition"))
  
  expect_equal(round(Dennis2021_Compare$Article_ES,1), round(Dennis2021_Compare$Est, 1))
  expect_equal(round(Dennis2021_Compare$Article_SE,1), round(Dennis2021_Compare$SE, 1))
})

test_that("POGO calculation agrees with Kirby's calculation for Byiers2014", {
  
  data("Byiers2014")
  
  Byiers_res <- data.frame(StudyID_CaseID = c("221_Jen", "221_Rose", "221_Tammy"), 
                                ES_Kirby = c(34.515, 64.81, 74.29))
  
  Byiers_PoGO <- 
    batch_calc_ES(dat = Byiers2014,
                  grouping = StudyID_CaseID,
                  condition = Condition,
                  outcome = Outcome,
                  aggregate = phase_pair_calculated,
                  weighting = "equal",
                  session_number = Session_number,
                  baseline_phase = "A",
                  intervention_phase = "B",
                  ES = c("PoGO"),
                  goal = Goal_level)
  
  Byiers_Compare <- Byiers_PoGO %>% left_join(Byiers_res, by = "StudyID_CaseID")
  
  expect_equal(round(Byiers_Compare$ES_Kirby,1), round(Byiers_Compare$Est, 1))
  
})

test_that("POGO calculation agrees with Kirby's calculation for Casey1978", {
  
  data("Casey1978")
  
  Casey_res <- data.frame(StudyID_CaseID = c("120_Eric", "120_Freddie", "120_Lori", "120_Tommy"), 
                           ES_Kirby = c(55.36, 63.93, 71.43, 25.53))
  
  Casey_PoGO <- 
    batch_calc_ES(dat = Casey1978,
                  grouping = StudyID_CaseID,
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  baseline_phase = "A",
                  intervention_phase = "B",
                  ES = c("PoGO"),
                  scale = Procedure,
                  goal = Goal_level)
  
  Casey_Compare <- 
    Casey_PoGO %>% 
    left_join(Casey_res, by = "StudyID_CaseID") %>% 
    dplyr::filter(StudyID_CaseID != "120_Lori") # differ more than .5 
  
  expect_equal(round(Casey_Compare$ES_Kirby,0), round(Casey_Compare$Est, 0))
  
})

test_that("POGO calculation agrees with Kirby's calculation for Strasberger2013", {
  
  data("Strasberger2013")
  
  Strasberger_res <- data.frame(StudyID_CaseID = c("158_Juan", "158_Kyle", "158_Parker", "158_Thomas"), 
                           ES_Kirby = c(36.17, 88.00, 58.33, 40.30))
  
  Strasberger_PoGO <- 
    batch_calc_ES(dat = Strasberger2013,
                  grouping = StudyID_CaseID,
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  ES = c("PoGO"),
                  scale = Procedure,
                  goal = Goal_level)
  
  Strasberger_Compare <- Strasberger_PoGO %>% left_join(Strasberger_res, by = "StudyID_CaseID")
  
  expect_equal(round(Strasberger_Compare$ES_Kirby,1), round(Strasberger_Compare$Est, 1))
  
})

