context("Improvement arguments work properly.")

library(dplyr)

data(Thorne)

Thorne_counts <- dplyr::filter(Thorne, Procedure == "count")

symmetric_ES <- c("LRRd","LRRi","SMD","Tau","NAP","PEM")

symmetric_ES_ests <- 
  batch_calc_ES(
    Thorne_counts, 
    grouping_vars = "Case", 
    condition = "Trt", 
    baseline_phase = 0, 
    session_number = "Session_number", 
    outcome = "Outcome",
    ES = symmetric_ES,
    std_dev = "pool",
    improvement = "decrease",
    scale = "count"
)


test_that("Symmetric ES have opposite sign if direction of improvement changes.", {

  switch_improvement_ests <- 
    batch_calc_ES(
      Thorne_counts, 
      grouping_vars = "Case",
      condition = "Trt",
      baseline_phase = 0,
      session_number = "Session_number",
      outcome = "Outcome",
      ES = symmetric_ES,
      std_dev = "pool",
      improvement = "increase",
      scale = "count"
    ) %>%
    mutate(
      Est = ifelse(ES %in% c("LRRd","LRRi","SMD","Tau"), -Est, 1 - Est),
      CI_L = ifelse(ES %in% c("LRRd","LRRi","SMD","Tau"), -CI_upper, 1 - CI_upper),
      CI_U = ifelse(ES %in% c("LRRd","LRRi","SMD","Tau"), -CI_lower, 1 - CI_lower)
    ) %>%
    select(-CI_lower, -CI_upper) %>%
    rename(CI_lower = CI_L, CI_upper = CI_U)
  
  expect_identical(symmetric_ES_ests$Case, switch_improvement_ests$Case)
  expect_identical(symmetric_ES_ests$ES, switch_improvement_ests$ES)
  expect_equal(symmetric_ES_ests$Est, switch_improvement_ests$Est)
  expect_equal(symmetric_ES_ests$SE, switch_improvement_ests$SE)
  expect_equal(symmetric_ES_ests$CI_lower, switch_improvement_ests$CI_lower)
  expect_equal(symmetric_ES_ests$CI_upper, switch_improvement_ests$CI_upper)
  
})

test_that("For symmetric ES, flipping improvement direction and phase coding yields identical results", {
  
  switch_baseline_ests <- 
    batch_calc_ES(
      Thorne_counts, 
      grouping_vars = "Case",
      condition = "Trt",
      baseline_phase = 1,
      session_number = "Session_number",
      outcome = "Outcome",
      ES = setdiff(symmetric_ES,"PEM"),
      std_dev = "pool",
      improvement = "increase",
      scale = "count"
    )
  
  symmetric_ES_ests_no_PEM <- dplyr::filter(symmetric_ES_ests, ES != "PEM")
  expect_identical(symmetric_ES_ests_no_PEM$Case, switch_baseline_ests$Case)
  expect_identical(symmetric_ES_ests_no_PEM$ES, switch_baseline_ests$ES)
  expect_equal(symmetric_ES_ests_no_PEM$Est, switch_baseline_ests$Est)
  expect_equal(symmetric_ES_ests_no_PEM$SE, switch_baseline_ests$SE)
  expect_equal(symmetric_ES_ests_no_PEM$CI_lower, switch_baseline_ests$CI_lower)
  expect_equal(symmetric_ES_ests_no_PEM$CI_upper, switch_baseline_ests$CI_upper)
  
})