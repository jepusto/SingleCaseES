context("Bonett & Price Jr (2020) examples.")

#------------------------------------------
# Example 1: Independent-Samples Design
#------------------------------------------

A_data <- c(21, 14, 11, 27, 19, 32, 21, 23, 18, 26, 24, 23)
B_data <- c(34, 19, 26, 31, 39, 42, 27, 14, 25, 29, 33, 36)

# function in Bonett & Price (2020)

ci_median_bs <- function(alpha, y1, y2) {
  z <- qnorm(1 - alpha/2)
  n1 <- length(y1)
  y1 <- sort(y1)
  n2 <- length(y2)
  y2 <- sort(y2)
  med1 <- median(y1)
  med2 <- median(y2)
  o1 <- round(n1/2 - sqrt(n1))
  if (o1 < 1) {o1 = 1}
  o2 <- n1 - o1 + 1
  l1 <- log(y1[o1])
  u1 <- log(y1[o2])
  p <- pbinom(o1 - 1, size = n1, prob = .5)
  z0 <- qnorm(1 - p)
  se1 <- (u1 - l1)/(2*z0)
  o1 <- round(n2/2 - sqrt(n2))
  if (o1 < 1) {o1 = 1}
  o2 <- n2 - o1 + 1
  l2 <- log(y2[o1])
  u2 <- log(y2[o2])
  p <- pbinom(o1 - 1, size = n2, prob = .5)
  z0 <- qnorm(1 - p)
  se2 <- (u2 - l2)/(2*z0)
  se <- sqrt(se1^2 + se2^2)
  logratio <- log(med1/med2)
  ll <- exp(logratio - z*se)
  ul <- exp(logratio + z*se)
  out <- data.frame(median1 = med1,
                    median2 = med2,
                    median_ratio = exp(logratio),
                    LL = ll,
                    UL = ul,
                    log_ratio = logratio,
                    se = se)
  return(out)
}


test_that("LRM is correct.", {
  
  res_ci_median_bs <- ci_median_bs(alpha = .05, y1 = B_data, y2 = A_data)
  res_LRM_delta <- LRM(A_data = A_data, B_data = B_data, delta_method = TRUE)
  res_LRM_bar <- LRM(A_data = A_data, B_data = B_data)
  
  expect_equal(res_ci_median_bs$log_ratio, res_LRM_delta$Est)
  expect_error(expect_equal(res_ci_median_bs$se, res_LRM_delta$SE))
  
  expect_equal(res_ci_median_bs$log_ratio, res_LRM_bar$Est)
  expect_equal(res_ci_median_bs$se, res_LRM_bar$SE)
  
  expect_equal(log(res_ci_median_bs$LL), res_LRM_bar$CI_lower)
  expect_equal(log(res_ci_median_bs$UL), res_LRM_bar$CI_upper)
})

test_that("LRM warns for data series of length 1.", {
  A_data <- c(9, 5, 1)
  B_data <- c(2, 3)
  C_data <- c(3)
  
  expect_silent(LRM(A_data = A_data, B_data = B_data, delta_method = TRUE))
  expect_silent(LRM(A_data = A_data, B_data = B_data))

  expect_warning(LRM(A_data = A_data, B_data = C_data, delta_method = TRUE))
  expect_warning(LRM(A_data = A_data, B_data = C_data))
  
})

test_that("LRM works when data series has zeros.", {
  A_data <- c(9, 5, 5, 6, 11, 4, 1, 2, 3, 6, 6)
  B_data <- c(0, 3, 0, 1, 4, 2, 4, 0, 3, 2, 1, 0, 0, 0)
  C_data <- c(0, 0, 0, 1, 0, 2, 4, 0, 3, 2, 1, 0, 0, 0)
  
  expect_silent(LRM(A_data = A_data, B_data = B_data, delta_method = TRUE))
  expect_silent(LRM(A_data = A_data, B_data = B_data))
  
  expect_silent(LRM(A_data = A_data, B_data = C_data, delta_method = TRUE))
  expect_silent(LRM(A_data = A_data, B_data = C_data))
  
})

test_that("LRM works within calc_ES() and batch_calc_ES().", {
  
  library(dplyr)
  
  res_A <- 
    McKissick %>%
    group_by(Case_pseudonym) %>%
    summarise(
      calc_ES(condition = Condition, outcome = Outcome, 
              ES = c("LRRd","LRM"),
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
      ES = c("LRRd","LRM"),
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
      ES = "LRM"
    ) %>%
    select(-ES) %>%
    rename_with(.fn = ~ paste("LRM", ., sep = "_"), .cols = -Case_pseudonym)
  
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
    dplyr::filter(ES == "LRM") %>%
    select(-ES) %>%
    rename_with(.fn = ~ paste("LRM", ., sep = "_"), .cols = -Case_pseudonym)
  
  res_E <- 
    batch_calc_ES(
      McKissick,
      grouping = Case_pseudonym,
      condition = Condition, 
      outcome = Outcome, 
      session_number = Session_number,
      improvement = "decrease",
      ES = "parametric",
      warn = FALSE
    ) %>%
    dplyr::filter(ES == "LRM") %>%
    select(-ES) %>%
    rename_with(.fn = ~ paste("LRM", ., sep = "_"), .cols = -Case_pseudonym)
  
  expect_equal(res_A, res_B)
  expect_equal(res_C, select(res_B, Case_pseudonym, starts_with("LRM")))
  expect_equal(res_C, res_D)
  expect_equal(res_C, res_E)
  
})
