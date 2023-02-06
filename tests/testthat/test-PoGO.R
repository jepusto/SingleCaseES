context("Calculating PoGO.")

suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))

test_that("PoGO is calculated correctly in calc_ES().", {
  
  A <- c(20, 20, 26, 25, 22, 23)
  B <- c(28, 25, 24, 27, 30, 30, 29)
  PoGO_calculated <- 100 * (mean(B) - mean(A)) / (25 - mean(A))
  PoGO1 <- PoGO(A_data = A, B_data = B, goal = 25)
  PoGO2 <- calc_ES(A_data = A, B_data = B, ES = "PoGO", goal = 25)
  PoGO_increase <- PoGO(A_data = A, B_data = B, goal = 25, improvement = "increase")
  PoGO_decrease <- PoGO(A_data = A, B_data = B, goal = 25, improvement = "decrease")
  
  expect_equal(PoGO_calculated, PoGO1$Est)
  expect_equal(PoGO1, PoGO2)
  expect_equal(PoGO_increase, PoGO_decrease)
  
  expect_error(calc_ES(A_data = A, B_data = B, ES = "PoGO"))
  expect_error(calc_ES(A_data = A, B_data = B, ES = "PoGO", goal = NULL))
  
})

test_that("PoGO can be calculated correctly in batch_calc_ES().", {
  
  data("McKissick")

  expect_error(batch_calc_ES(McKissick,
                             grouping = Case_pseudonym,
                             condition = Condition,
                             outcome = Outcome,
                             ES = c("PoGO"),
                             improvement = "decrease",
                             scale = "count",
                             format = "long"))
  
  McKissick_goal <- McKissick %>% dplyr::mutate(goal = 10)
  
  expect_error(batch_calc_ES(McKissick_goal,
                             grouping = Case_pseudonym,
                             condition = Condition,
                             outcome = Outcome,
                             ES = c("PoGO"),
                             improvement = "decrease",
                             scale = "count",
                             format = "long"))
  
  expect_error(batch_calc_ES(McKissick_goal,
                             grouping = Case_pseudonym,
                             condition = Condition,
                             outcome = Outcome,
                             ES = "parametric",
                             improvement = "decrease",
                             scale = "count",
                             format = "long"))
  
  expect_error(batch_calc_ES(McKissick_goal,
                             grouping = Case_pseudonym,
                             condition = Condition,
                             outcome = Outcome,
                             ES = "all",
                             improvement = "decrease",
                             scale = "count",
                             format = "long"))
  
  Mck_est1 <- 
    batch_calc_ES(McKissick_goal,
                  grouping = Case_pseudonym,
                  condition = Condition,
                  outcome = Outcome,
                  ES = "PoGO",
                  goal = goal,
                  improvement = "decrease",
                  scale = "count",
                  format = "long")
  
  Mck_est2 <- 
    batch_calc_ES(McKissick_goal,
                  grouping = Case_pseudonym,
                  condition = Condition,
                  outcome = Outcome,
                  ES = "PoGO",
                  goal = 10,
                  improvement = "increase",
                  scale = "percentage",
                  format = "long")
  
  Mck_est_calculated <- 
    McKissick_goal %>%
    group_by(Case_pseudonym, Condition) %>% 
    summarise(
      Outcome = mean(Outcome), 
      goal = mean(goal), 
      .groups = "drop"
    ) %>% 
    pivot_wider(names_from = Condition, values_from = Outcome) %>% 
    mutate(Est = 100 * (B - A) / (goal - A)) %>% 
    select(Case_pseudonym, Est)
  
  expect_equal(Mck_est1, Mck_est2)
  expect_equal(select(Mck_est1, Case_pseudonym, Est), Mck_est_calculated)
  
})


test_that("PoGO results are conceptually correct.", {
  
  data("Schmidt2012")
  gl <- 17
  
  Schmidt2012_goals <- 
    Schmidt2012 %>%
    group_by(Case, Behavior) %>%
    mutate(
      goal_fixed = gl,
      goal_mean = mean(Outcome[Trt==1]),
      goal_var = rpois(1, lambda = 18)
    )
  
  # Fixed variable returns same result as common goal
  
  PoGO_fixed <- 
    batch_calc_ES(
      Schmidt2012_goals,
      grouping = c(Case, Behavior),
      condition = Trt,
      baseline_phase = "0",
      outcome = Outcome,
      ES = "PoGO",
      goal = goal_fixed
    )
  
  PoGO_common <- 
    batch_calc_ES(
      Schmidt2012_goals,
      grouping = c(Case, Behavior),
      condition = Trt,
      baseline_phase = "0",
      outcome = Outcome,
      ES = "PoGO",
      goal = gl
    )
  
  expect_identical(PoGO_fixed, PoGO_common)
  
  # PoGO with zero goal is the same as -LRRi
  
  PoGO_zero <- 
    batch_calc_ES(
      Schmidt2012_goals,
      grouping = c(Case, Behavior),
      condition = Trt,
      baseline_phase = "0",
      # intervention_phase = "1",
      outcome = Outcome,
      ES = c("PoGO","LRRi"),
      goal = 0,
      bias_correct = FALSE,
      pct_change = TRUE,
      confidence = NULL
    )
  
  PoGO_zero %>%
    dplyr::filter(ES %in% c("PoGO","Pct_Change_i")) %>%
    group_by(Case, Behavior) %>%
    summarise(Est = sum(Est), .groups = "drop") %>%
    summarise(Est = max(abs(Est))) %>%
    pull(Est) %>%
    expect_lt(1e-8)
  
  PoGO_mean <- 
    batch_calc_ES(
      Schmidt2012_goals,
      grouping = c(Case, Behavior),
      condition = Trt,
      baseline_phase = "0",
      outcome = Outcome,
      ES = "PoGO",
      goal = goal_mean,
      bias_correct = FALSE,
      pct_change = TRUE
    ) %>%
    mutate(
      hundred = 100
    )
  
  expect_equal(PoGO_mean$Est, PoGO_mean$hundred)
  
  
  PoGO_var <- 
    batch_calc_ES(
      Schmidt2012_goals,
      grouping = c(Case, Behavior),
      condition = Trt,
      baseline_phase = "0",
      outcome = Outcome,
      ES = "PoGO",
      goal = goal_var,
      bias_correct = FALSE,
      pct_change = TRUE
    )
  
  PoGO_signs <- 
    Schmidt2012_goals %>%
    group_by(Case, Behavior, Trt) %>%
    summarise(
      across(c(Outcome, goal_var), mean),
      .groups = "drop_last"
    ) %>%
    summarise(
      base = Outcome[Trt==0],
      outcome_diff = diff(Outcome),
      goal_var = mean(goal_var),
      .groups = "drop"
    ) %>%
    mutate(
      PoGO_sign = sign(goal_var - base) * sign(outcome_diff)
    ) %>%
    left_join(PoGO_var, by = c("Case", "Behavior"))
  
  expect_true(all(sign(PoGO_signs$Est) == PoGO_signs$PoGO_sign))  
  
})


