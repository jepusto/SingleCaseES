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
  
  skip_if_not_installed("Kendall")
  
  # Example 3.1 in Kendall (1970), p.35
  A <- c(1, 2.5, 2.5, 4.5, 4.5, 6.5, 6.5, 8, 9.5, 9.5)
  B <- c(1, 2, rep(4.5, 4), rep(8, 3), 10)
  
  Kendall_pkg <- Kendall::Kendall(A, B)
  S_pkg <- as.numeric(Kendall_pkg$S)
  D_pkg <- as.numeric(Kendall_pkg$D)
  Tau_pkg <- round(as.numeric(Kendall_pkg$tau), 3)
  
  S_book <- 33
  D_book <- sqrt(41 * 36)
  Tau_b_book <- .859
  
  expect_equal(S_pkg, S_book)
  expect_equal(D_pkg, D_book, tolerance = 1e-7)
  expect_equal(Tau_pkg, Tau_b_book)
  
  # Check against formula for Tau_BC^* in the vignette
  y <- c(A, B)
  m <- length(A)
  n <- length(B)
  trt <- c(rep(0L, m), rep(1L, n))
  Kendall_pkg <- Kendall::Kendall(y, trt)
  
  Q_mat <- matrix(sapply(B, function(j) (j > A) - (j < A)), nrow = m, ncol = n)
  S_comp <- sum(Q_mat)
  U <- sum(table(y) * (table(y) - 1) / 2)
  D_comp <- sqrt(m * n * ((m + n) * (m + n - 1) / 2 - U))
  Tau_comp <- S_comp / D_comp

  expect_equal(as.numeric(Kendall_pkg$S), S_comp)
  expect_equal(as.numeric(Kendall_pkg$D), D_comp, tolerance = 1e-7)
  expect_equal(as.numeric(Kendall_pkg$tau), Tau_comp)
    
})


test_that("Tau-BC is correct regarding pretest_trend argument.", {
  
  skip_if_not_installed("Kendall")
  
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
  
  A_data <- c(1, 1, 3, 3, 5, 5)
  B_data <- c(5, 6, 4, 8, 9)
  m <- length(A_data)
  n <- length(B_data)
  session_A <- 1:m
  session_B <- (m + 1) : (m + n)
  
  pval_slope_A <- Kendall::Kendall(A_data, session_A)$sl
  
  Tau_BC_05 <- Tau_BC(A_data, B_data, pretest_trend = .05, report_correction = TRUE)
  expect_equal(Tau_BC_05$ES, "Tau-BC")
  expect_lt(Tau_BC_05$pval_slope_A, .05)
  
  expect_message(Tau_BC(A_data, B_data, pretest_trend = .01))
  Tau_BC_01 <- Tau_BC(A_data, B_data, pretest_trend = .01, report_correction = TRUE, warn = FALSE)
  expect_gt(Tau_BC_01$pval_slope_A, .01)
  
  Tau_BC_increase <- Tau_BC(A_data, B_data, pretest_trend = .01, improvement = "increase")
  Tau_BC_decrease <- Tau_BC(A_data, B_data, pretest_trend = .01, improvement = "decrease")
  expect_equal(Tau_BC_increase$Est, -Tau_BC_decrease$Est)
  expect_equal(Tau_BC_increase$SE, Tau_BC_decrease$SE)
  expect_equal(Tau_BC_increase$CI_lower, -Tau_BC_decrease$CI_upper)
  expect_equal(Tau_BC_increase$CI_upper, -Tau_BC_decrease$CI_lower)
  
  Tau_Kendall_increase <- Tau_BC(A_data, B_data, pretest_trend = .01, Kendall = TRUE, improvement = "increase", warn = FALSE)
  Tau_Kendall_decrease <- Tau_BC(A_data, B_data, pretest_trend = .01, Kendall = TRUE, improvement = "decrease", warn = FALSE)
  expect_equal(Tau_Kendall_increase$Est, -Tau_Kendall_decrease$Est)
  expect_equal(Tau_Kendall_increase$SE, Tau_Kendall_decrease$SE)
  expect_equal(Tau_Kendall_increase$CI_lower, -Tau_Kendall_decrease$CI_upper)
  expect_equal(Tau_Kendall_increase$CI_upper, -Tau_Kendall_decrease$CI_lower)
})




test_that("Tau-BC works on an example.", {
  
  skip_on_cran()
  skip_if_not_installed("Kendall")
  library(Kendall)
  
  source("https://ktarlow.com/stats/r/bctau.txt")
  
  A_data <- c(33, 25, 17, 25, 19, 21, 19, 14, 19, 17)
  B_data <- c(14, 15, 15, 9, 12, 7, 10, 6, 5, 2, 2, 3, 5, 4)
  
  # check the package result when using Kendall rank correlation
  pkg_res <- Tau_BC(A_data = A_data, B_data = B_data, report_correction = TRUE, Kendall = TRUE, trunc_const = TRUE)
  expect_null(pkg_res$trunc)
  
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
  pkg_res_Tau <- Tau_BC(A_data = A_data, B_data = B_data, 
                        report_correction = TRUE, Kendall = FALSE, 
                        trunc_const = TRUE)
  Tau_Tarlow <- Tau(A_data = A_data_corrected, B_data = B_data_corrected)
  
  expect_equal(subset(Tau_Tarlow, select = -ES), 
               subset(pkg_res_Tau, select = c(Est, SE, CI_lower, CI_upper)))
  expect_equal(pkg_res_Tau$trunc, 1 / (m * n))
  
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
              format = "wide", trunc_const = TRUE)
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
      format = "wide",
      trunc_const = TRUE
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
  
  all_names <- c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U",
                 "LOR", "LRRd", "LRRi", "LRM", "SMD")
  
  res_D <- 
    batch_calc_ES(
      McKissick,
      grouping = Case_pseudonym,
      condition = Condition, 
      outcome = Outcome, 
      session_number = Session_number,
      improvement = "decrease",
      ES = all_names,
      warn = FALSE
    ) %>%
    dplyr::filter(ES == "Tau-BC") %>%
    select(-c(ES, baseline_SD)) %>%
    rename_with(.fn = ~ paste("Tau-BC", ., sep = "_"), .cols = -Case_pseudonym)
  
  expect_equal(res_A, res_B)
  expect_equal(res_C, select(res_B, all_of(names(res_C))))
  expect_equal(res_C, res_D)
  
})
