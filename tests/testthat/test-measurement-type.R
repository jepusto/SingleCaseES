context("The outcome measurement scale.")

suppressWarnings(library(dplyr))

test_that("Scale in uppercase are accepted for the measurement scale.", {
  
  data("Byiers2014")
  
  data("Casey1978")
  Casey1978 <- Casey1978 %>% mutate(phase_pair_calculated = 1)
  
  data("Strasberger2014")
  Strasberger2014 <- Strasberger2014 %>% mutate(phase_pair_calculated = 1)
  
  dat <- rbind(Byiers2014, Casey1978, Strasberger2014)
  
  N_case <- length(unique(dat$StudyID_CaseID))
  ES_ests <- 
    batch_calc_ES(dat = dat,
                  grouping = c(StudyID, StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  aggregate = phase_pair_calculated,
                  session_number = Session_number,
                  ES = c("LRRi","LRRd", "PoGO"),
                  improvement = "increase",
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide") 
  expect_equal(nrow(ES_ests), N_case)
  expect_s3_class(ES_ests, "data.frame")
  
  dat_A <- 
    dat %>% 
    mutate(Procedure = toupper(Procedure))
  
  ES_ests_A <- 
    batch_calc_ES(dat = dat_A,
                  grouping = c(StudyID, StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  aggregate = phase_pair_calculated,
                  session_number = Session_number,
                  ES = c("LRRi","LRRd", "PoGO"),
                  improvement = "increase",
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide") 
  
  expect_equal(ES_ests_A, ES_ests)
  
  dat_B <- 
    dat %>% 
    mutate(Procedure = if_else(StudyID_CaseID == "221_Jen", "Prcntg", Procedure))
  expect_error(batch_calc_ES(dat = dat_B,
                             grouping = c(StudyID, StudyID_CaseID),
                             condition = Condition,
                             outcome = Outcome,
                             aggregate = phase_pair_calculated,
                             session_number = Session_number,
                             ES = c("LRRi","LRRd", "PoGO"),
                             improvement = "increase",
                             scale = Procedure,
                             goal = Goal_level,
                             format = "wide"))
  
})