context("Warning messages for measurement scale and improvement direction.")

test_that("Scale variations are accepted for the measurement scale.", {
  
  data("Byiers2014")
  
  data("Casey1978")
  Casey1978$phase_pair_calculated <- 1
  
  data("Strasberger2014")
  Strasberger2014$phase_pair_calculated <- 1
  
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
  
  dat_C <- dat_B <- dat_A <- dat
  dat_A$Procedure <- toupper(dat$Procedure)

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
  
  dat_B$Procedure <- ifelse(dat_B$StudyID_CaseID == "221_Jen", "Prcntg", dat_B$Procedure)
  
  expect_error(
    batch_calc_ES(
      dat = dat_B,
      grouping = c(StudyID, StudyID_CaseID),
      condition = Condition,
      outcome = Outcome,
      aggregate = phase_pair_calculated,
      session_number = Session_number,
      ES = c("LRRi","LRRd", "PoGO"),
      improvement = "increase",
      scale = Procedure,
      goal = Goal_level,
      format = "wide"
    )
  )
  
  dat_C$Procedure <- substr(dat_A$Procedure, 1, 5)
  
  ES_ests_C <- 
    batch_calc_ES(dat = dat_C,
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
  
  expect_equal(ES_ests_C, ES_ests)
  
})


test_that("Improvement direction variations are accepted for the outcome valence.", {
  
  data("Strasberger2014")
  dat <- dat_A <- dat_B <- dat_C <- Strasberger2014
  N_sessions <- as.numeric(table(dat$StudyID_CaseID))
  
  ES_ests <- 
    batch_calc_ES(dat = dat,
                  grouping = c(StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  ES = c("LRRi","LRRd", "PoGO"),
                  improvement = "Increase",
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide")
  expect_equal(nrow(ES_ests), length(unique(dat$StudyID_CaseID)))
  expect_s3_class(ES_ests, "data.frame")
  
  dat_A$Direction <- "increase"
  ES_ests_A <- 
    batch_calc_ES(dat = dat_A,
                  grouping = c(StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  ES = c("LRRi","LRRd", "PoGO"),
                  improvement = Direction,
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide")
  
  expect_equal(ES_ests, ES_ests_A)
  
  dat_B$Direction <- c(rep("INCREASE", N_sessions[1]), 
                       rep("incre", N_sessions[2]), 
                       rep("Incre", N_sessions[3]),
                       rep("Increase", N_sessions[4]))
  ES_ests_B <- 
    batch_calc_ES(dat = dat_B,
                  grouping = c(StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  ES = c("LRRi","LRRd", "PoGO"),
                  improvement = Direction,
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide")
  
  expect_equal(ES_ests, ES_ests_B)
  
  dat_C$Direction <- c(rep("INCREASE", N_sessions[1]), 
                     rep("incrase", N_sessions[2]), 
                     rep("direction", N_sessions[3]),
                     rep("IncR", N_sessions[4]))
  
  expect_error(
    batch_calc_ES(
      dat = dat_C,
      grouping = c(StudyID_CaseID),
      condition = Condition,
      outcome = Outcome,
      session_number = Session_number,
      ES = c("LRRi","LRRd", "PoGO"),
      improvement = Direction,
      scale = Procedure,
      goal = Goal_level,
      format = "wide"
    )
  )
  
})