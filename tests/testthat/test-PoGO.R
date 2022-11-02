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
                  format = "long") %>% 
    dplyr::select(Case_pseudonym, Est)
  
  Mck_est2 <- 
    batch_calc_ES(McKissick_goal,
                  grouping = Case_pseudonym,
                  condition = Condition,
                  outcome = Outcome,
                  ES = "PoGO",
                  goal = 10,
                  improvement = "increase",
                  scale = "percentage",
                  format = "long") %>% 
    dplyr::select(Case_pseudonym, Est)
  
  Mck_est_calculated <- 
    McKissick_goal %>%
    group_by(Case_pseudonym, Condition) %>% 
    summarise(
      Outcome = mean(Outcome), 
      goal = mean(goal), 
      .groups = "drop"
    ) %>% 
    pivot_wider(names_from = Condition, values_from = Outcome) %>% 
    mutate(Est = 100 * (B-A) / (goal - A)) %>% 
    select(Case_pseudonym, Est)
  
  expect_equal(Mck_est1, Mck_est2)
  expect_equal(Mck_est1, Mck_est_calculated)
  
})
