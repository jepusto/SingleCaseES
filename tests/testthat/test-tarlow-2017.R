context("Tarlow (2017).")

A <- c(1, 2, 3, 4, 5)
B <- c(6, 7, 8, 9, 10)

NOMs <- calc_ES(A, B, improvement = "increase", 
                ES = c("Tau", "Tau_U", "Tau_BC"),
                confidence = NULL)

test_that("Tau is correct.", {
  tau <- Tau(A, B, improvement = "increase", confidence = NULL)
  expect_equal(1, tau$Est)
  expect_equal(tau, subset(NOMs, ES == "Tau"), check.attributes = FALSE)
})

test_that("Tau-U is correct.", {
  tauU <- Tau_U(A, B, improvement = "increase")
  tauU$SE <- NA_real_
  expect_equal(0.6, tauU$Est)
  expect_equal(tauU, subset(NOMs, ES == "Tau-U"), check.attributes = FALSE)
})

test_that("Tau-BC is correct.", {
  tauBC <- Tau_BC(A, B, improvement = "increase", confidence = NULL)
  expect_equal(0, tauBC$Est)
  expect_equal(tauBC, subset(NOMs, ES == "Tau-BC"), check.attributes = FALSE)
  expect_null(tauBC$CI_lower)
  expect_null(tauBC$CI_upper)
  
  tauBC <- Tau_BC(A, B, improvement = "increase", Kendall = TRUE, confidence = NULL)
  expect_equal(0, tauBC$Est)
  expect_error(expect_equal(tauBC$SE, subset(NOMs, ES == "Tau-BC")$SE))
  expect_null(tauBC$CI_lower)
  expect_null(tauBC$CI_upper)
  
})

test_that("The formula for Kendall's rank correlation is correct.", {
  # Example 3.1 in Kendall (1970), p.35
  A <- c(1, 2.5, 2.5, 4.5, 4.5, 6.5, 6.5, 8, 9.5, 9.5)
  B <- c(1, 2, rep(4.5, 4), rep(8, 3), 10)
  
  Kendall_pkg <- Kendall::Kendall(A, B)
  S_pkg <- as.numeric(Kendall_pkg$S)
  D_pkg <- as.numeric(Kendall_pkg$D)
  Tau_pkg <- round(as.numeric(Kendall_pkg$tau), 3)
  
  S_book <- 33
  D_book <- sqrt(41*36)
  Tau_b_book <- .859
  
  expect_equal(S_pkg, S_book)
  expect_equal(D_pkg, D_book, tolerance = .0000001)
  expect_equal(Tau_pkg, Tau_b_book)
  
})


test_that("Tau-BC is correct regarding pretest_trend argument.", {
  
  A <- c(20, 20, 26, 25, 22, 23)
  B <- c(28, 25, 24, 27, 30, 30, 29)
  
  NOMs <- calc_ES(A, B, improvement = "increase", 
                  ES = c("Tau", "Tau_BC"),
                  SE = "none", confidence = NULL, 
                  pretest_trend = .05, warn = FALSE) %>%
      dplyr::select(-ES)
  
  
  expect_equal(NOMs[1, ], NOMs[2, ], check.attributes = FALSE)
  
  TauBC <- Tau_BC(A, B, improvement = "increase", SE = "none", confidence = NULL)
  expect_error(expect_equal(TauBC$Est, NOMs$Est[2]))
  
  expect_message(Tau_BC(A, B, pretest_trend = .05))
  
  
  A_data <- c(1, 2, 3, 4, 5)
  B_data <- c(5, 6, 3, 2, 4)
  m <- length(A_data)
  n <- length(B_data)
  session_A <- 1:m
  session_B <- (m + 1) : (m + n)
  pval_slope_A <- Kendall::Kendall(A_data, session_A)$sl
  
  Tau_BC_05 <- Tau_BC(A_data, B_data, pretest_trend = .05, report_correction = TRUE)
  expect_equal(Tau_BC_05$ES, "Tau-BC")
  expect_lt(Tau_BC_05$pval_slope_A, .05)
  
  expect_message(Tau_BC(A_data, B_data, pretest_trend = .01))
  Tau_BC_01 <- suppressMessages(Tau_BC(A_data, B_data, pretest_trend = .01, report_correction = TRUE)) 
  expect_gt(Tau_BC_01$pval_slope_A, .01)
  
  Tau_BC_increase <- Tau_BC(A_data, B_data, improvement = "increase")
  Tau_BC_decrease <- Tau_BC(A_data, B_data, improvement = "decrease")
  expect_equal(Tau_BC_increase$Est, -Tau_BC_decrease$Est)
})

library(Kendall)
source("http://ktarlow.com/stats/r/bctau.txt")

test_that("Tau-BC works on an example.", {
  
  A_data <- c(33, 25, 17, 25, 19, 21, 19, 14, 19, 17)
  B_data <- c(14, 15, 15, 9, 12, 7, 10, 6, 5, 2, 2, 3, 5, 4)
  
  # check the package result when using Kendall rank correlation
  pkg_res <- Tau_BC(A_data = A_data, B_data = B_data, report_correction = TRUE, Kendall = TRUE)
  
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
  
  expect_equal(pkg_res$Est, as.numeric(Kendall_tau))
  
  Tarlow_res <- bctau(A_data, B_data)
  
  expect_equal(Tarlow_res$slope, slope)
  expect_equal(Tarlow_res$int, intercept)
  expect_equal(Tarlow_res$correcteda, A_data_corrected)
  expect_equal(Tarlow_res$correctedb, B_data_corrected)
  expect_equal(Tarlow_res$tau, pkg_res$Est)
  expect_equal(Tarlow_res$se, pkg_res$SE)
  
  # check package result when using Tau (non-overlap)
  pkg_res_Tau <- Tau_BC(A_data = A_data, B_data = B_data, report_correction = TRUE, Kendall = FALSE)
  Tau_Tarlow <- Tau(A_data = A_data_corrected, B_data = B_data_corrected)
  
  expect_equal(subset(Tau_Tarlow, select = -ES), 
               subset(pkg_res_Tau, select = c(Est, SE, CI_lower, CI_upper)))
  
  # check the case when baseline trend is not significant
  A <- c(3, 3, 4, 5, 5, 2)
  B <- c(5, 6, 3, 2, 4, 4)
  pkg_res_notsig <- Tau_BC(A, B, pretest_trend = .05, Kendall = TRUE)
  Tarlow_res_notsig <- bctau(A, B)
  
  expect_equal(Tarlow_res_notsig$tau, pkg_res_notsig$Est)
  expect_equal(Tarlow_res_notsig$se, pkg_res_notsig$SE)
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
