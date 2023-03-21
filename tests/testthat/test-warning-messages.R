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
  dat <- dat_A <- dat_B <- dat_C <- dat_D <- Strasberger2014
  N_sessions <- as.numeric(table(dat$StudyID_CaseID))
  
  ES_ests <- 
    batch_calc_ES(dat = dat,
                  grouping = c(StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  ES = c("Tau","PND","LRRi","LRRd", "PoGO"),
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
                  ES = c("Tau","PND","LRRi","LRRd", "PoGO"),
                  improvement = Direction,
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide")
  
  expect_equal(ES_ests, ES_ests_A)
  
  dat_B$Direction <- rep(c("INCREASE","incre","Incre","Increase"), N_sessions)
  ES_ests_B <- 
    batch_calc_ES(dat = dat_B,
                  grouping = c(StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  ES = c("Tau","PND","LRRi","LRRd", "PoGO"),
                  improvement = Direction,
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide")
  
  expect_equal(ES_ests, ES_ests_B)
  
  dat_C$Direction <- rep(c("INCREASE","incrase","direction","IncR"), N_sessions)

  expect_error(
    batch_calc_ES(
      dat = dat_C,
      grouping = c(StudyID_CaseID),
      condition = Condition,
      outcome = Outcome,
      session_number = Session_number,
      ES = c("Tau","PND","LRRi","LRRd", "PoGO"),
      improvement = Direction,
      scale = Procedure,
      goal = Goal_level,
      format = "wide"
    )
  )
  
  dat_D$Direction <- rep(c("DECREASE","decre","Decre","DEcrease"), N_sessions)
  
  ES_ests_D <- 
    batch_calc_ES(dat = dat_D,
                  grouping = c(StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  ES = c("Tau","PND","LRRi","LRRd", "PoGO"),
                  improvement = Direction,
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide")
  
  expect_equal(ES_ests$Tau_Est, -ES_ests_D$Tau_Est)
  expect_equal(ES_ests$Tau_SE, ES_ests_D$Tau_SE)
  expect_equal(ES_ests$LRRi_Est, -ES_ests_D$LRRi_Est)
  expect_equal(ES_ests$LRRi_SE, ES_ests_D$LRRi_SE)
  expect_equal(ES_ests$LRRd_Est, -ES_ests_D$LRRd_Est)
  expect_equal(ES_ests$LRRd_SE, ES_ests_D$LRRd_SE)
  expect_equal(ES_ests$PoGO_Est, ES_ests_D$PoGO_Est)
  expect_equal(ES_ests$PoGO_SE, ES_ests_D$PoGO_SE)
  
  ES_ests_E <- 
    batch_calc_ES(dat = dat_D,
                  grouping = c(StudyID_CaseID),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  ES = c("Tau","PND","LRRi","LRRd", "PoGO"),
                  improvement = "Dec",
                  scale = Procedure,
                  goal = Goal_level,
                  format = "wide")
  
  expect_equal(ES_ests_D, ES_ests_E)
  
})