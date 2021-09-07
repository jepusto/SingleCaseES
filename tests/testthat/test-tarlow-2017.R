context("Tarlow (2017).")

A <- c(1, 2, 3, 4, 5)
B <- c(6, 7, 8, 9, 10)

NOMs <- calc_ES(A, B, improvement = "increase", 
                ES = c("Tau", "Tau_U", "Tau_BC"),
                SE = "none", confidence = NULL)

test_that("Tau is correct.", {
  tau <- Tau(A, B, improvement = "increase", SE = "none", confidence = NULL)
  expect_equal(1, tau$Est)
  expect_equal(tau, subset(NOMs, ES == "Tau"), check.attributes = FALSE)
})

test_that("Tau-U is correct.", {
  tauU <- Tau_U(A, B, improvement = "increase")
  expect_equal(0.6, tauU$Est)
  expect_equal(tauU, subset(NOMs, ES == "Tau-U"), check.attributes = FALSE)
})

test_that("Tau-BC is correct.", {
  tauBC <- Tau_BC(A, B, improvement = "increase", SE = "none", confidence = NULL)
  expect_equal(0, tauBC$Est)
  expect_equal(tauBC, subset(NOMs, ES == "Tau-BC"), check.attributes = FALSE)
})

test_that("Tau-BC is correct regarding pretest_trend argument.", {
  A <- c(20, 20, 26, 25, 22, 23)
  B <- c(28, 25, 24, 27, 30, 30, 29)
  
  NOMs <- suppressWarnings(
    calc_ES(A, B, improvement = "increase", 
            ES = c("Tau", "Tau_BC"),
            SE = "none", confidence = NULL, 
            pretest_trend = .05)
  )
  expect_equal(NOMs[1, ], NOMs[2, ], check.attributes = FALSE)
  
  TauBC <- Tau_BC(A, B, improvement = "increase", SE = "none", confidence = NULL)
  expect_error(expect_equal(TauBC$Est, NOMs$Est[2]))
  
  expect_message(Tau_BC(A, B, pretest_trend = .05))
  
})

library(Kendall)
source("http://ktarlow.com/stats/r/bctau.txt")

test_that("Tau-BC works on an example.", {
  
  A_data <- c(33, 25, 17, 25, 19, 21, 19, 14, 19, 17)
  B_data <- c(14, 15, 15, 9, 12, 7, 10, 6, 5, 2, 2, 3, 5, 4)
  
  pkg_res <- Tau_BC(A_data = A_data, B_data = B_data, report_correction = TRUE)
  
  m <- length(A_data)
  n <- length(B_data)
  session_A <- 1:m
  session_B <- (m + 1) : (m + n)
  session_dummy <- c(rep(0L, m), rep(1L, n))
  intercept <- pkg_res$intercept
  slope <- pkg_res$slope
  
  A_data_corrected <- A_data - intercept - slope * session_A
  B_data_corrected <- B_data - intercept - slope * session_B
  Kendall_tau <- Kendall(session_dummy, c(A_data_corrected, B_data_corrected))$tau
  
  Tarlow_res <- bctau(A_data, B_data)
  
  expect_equal(Tarlow_res$slope, slope)
  expect_equal(Tarlow_res$int, intercept)
  expect_equal(Tarlow_res$correcteda, A_data_corrected)
  expect_equal(Tarlow_res$correctedb, B_data_corrected)
  expect_equal(Tarlow_res$tau, as.numeric(Kendall_tau))
  
  Tau_Tarlow <- Tau(A_data = A_data_corrected, B_data = B_data_corrected)
  
  expect_equal(subset(Tau_Tarlow, select = -ES), 
               subset(pkg_res, select = c(Est, SE, CI_lower, CI_upper)))
})

test_that("Tau-BC works within calc_ES() and batch_calc_ES().", {
  
  library(dplyr)
  
  res_A <- 
    McKissick %>%
    group_by(Case_pseudonym) %>%
    summarise(
      calc_ES(condition = Condition, outcome = Outcome, 
              ES = c("Tau","Tau_BC"),
              improvement = "decrease",
              format = "wide")
    )
  
  res_B <- 
    batch_calc_ES(
      McKissick,
      grouping = Case_pseudonym,
      condition = Condition, 
      outcome = Outcome, 
      session_number = Session_number,
      ES = c("Tau","Tau_BC"),
      improvement = "decrease",
      format = "wide"
    )
  
  res_C <- 
    batch_calc_ES(
      McKissick,
      grouping = Case_pseudonym,
      condition = Condition, 
      outcome = Outcome, 
      session_number = Session_number,
      improvement = "decrease",
      ES = "Tau-BC"
    ) %>%
    select(-ES) %>%
    rename_with(.fn = ~ paste("Tau-BC", ., sep = "_"), .cols = -Case_pseudonym)
  
  res_D <- 
    batch_calc_ES(
      McKissick,
      grouping = Case_pseudonym,
      condition = Condition, 
      outcome = Outcome, 
      session_number = Session_number,
      improvement = "decrease",
      ES = "all",
      warn = FALSE
    ) %>%
    dplyr::filter(ES == "Tau-BC") %>%
    select(-ES) %>%
    rename_with(.fn = ~ paste("Tau-BC", ., sep = "_"), .cols = -Case_pseudonym)
  
  expect_equal(res_A, res_B)
  expect_equal(res_C, select(res_B, Case_pseudonym, starts_with("Tau-BC")))
  expect_equal(res_C, res_D)
  
})
