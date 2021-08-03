context("batch_calc_ES() works properly.")

suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))

test_that("batch_calc_ES() returns the same output 
          as calc_ES() for a data frame with no grouping variable.", {
  
  data("McKissick")
  dat1 <- McKissick %>% dplyr::filter(Case_pseudonym == "Period 1")
  
  # use calc_ES()
  res_calc_ES <- calc_ES(condition = dat1$Condition, outcome = dat1$Outcome, baseline_phase = "A") %>% as_tibble()
  
  # use batch_calc_ES()
  res_batch_calc_ES <- batch_calc_ES(dat = dat1, condition = Condition, outcome = Outcome, baseline_phase = "A")
  
  expect_equal(res_calc_ES, res_batch_calc_ES)
  
})


test_that("The aggregate argument in batch_calc_ES() works in long format.", {
  
  data("Schmidt2007")
  
  # long format using 1/V
  ## calculate aggregate by hand
  ES_ests <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym, Phase_num),
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd","SMD","Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  
  ES_agg_calculated <- 
    ES_ests %>% 
    group_by(Behavior_type, Case_pseudonym, ES) %>% 
    mutate(ES_wt = 1 / SE^2) %>% 
    summarise(
      Est = sum(Est * ES_wt) / sum(ES_wt),
      SE = sqrt(1 / sum(ES_wt)),
      .groups = "drop"
    )
  
  ## calculate aggregate by specifying the aggregating variables
  ES_agg <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym),
                  aggregate = Phase_num,
                  weighting = "1/V",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi", "LRRd", "SMD", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  
  expect_equal(ES_agg_calculated, ES_agg)
  
  # long format using equal weights
  ES_agg_equal_calculated <- 
    ES_ests %>% 
    group_by(Behavior_type, Case_pseudonym, ES) %>% 
    mutate(ES_wt = 1/n()) %>% 
    summarise(
      Est = weighted.mean(Est, w = ES_wt),
      SE = sqrt(sum(SE^2 * ES_wt^2) / sum(ES_wt)^2),
      .groups = "drop"
    )
  
  ES_agg_equal <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym),
                  aggregate = Phase_num,
                  weighting = "equal",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd", "SMD","Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  
  expect_equal(ES_agg_equal_calculated, ES_agg_equal)
    
})


test_that("The aggregate argument in batch_calc_ES() works in wide format and with CIs.", {
  
  data("Schmidt2007")
  
  ES_agg_CI <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym),
                  aggregate = Phase_num,
                  weighting = "1/V",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd","SMD","Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = .95,
                  format = "long")
  
  ES_agg_CI_long2wide <- 
    ES_agg_CI %>% 
    tidyr::pivot_wider(
      names_from = ES,
      names_glue = "{ES}_{.value}",
      values_from = Est:CI_upper
    ) %>% 
    select(Behavior_type, Case_pseudonym, 
           starts_with("LRRi_"), starts_with("LRRd_"), 
           starts_with("SMD_"), starts_with("Tau_"))
  
  ES_agg_CI_wide <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym),
                  aggregate = Phase_num,
                  weighting = "1/V",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd","SMD", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = .95,
                  format = "wide")
  
  expect_equal(ES_agg_CI_long2wide, ES_agg_CI_wide)
  
})

test_that("The aggregate argument in batch_calc_ES() works with multiple aggregation variables.", {
  
  data("Schmidt2007")
  
  ES_ests <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym, Phase_num),
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd","SMD","Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  
  ES_agg_calculated <- 
    ES_ests %>% 
    group_by(Case_pseudonym, ES) %>% 
    mutate(ES_wt = 1 / SE^2) %>% 
    summarise(
      Est = sum(Est * ES_wt) / sum(ES_wt),
      SE = sqrt(1 / sum(ES_wt)),
      .groups = "drop"
    )
  
  ## calculate aggregate by specifying the aggregating variables
  ES_agg <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = Case_pseudonym,
                  aggregate = c(Behavior_type, Phase_num),
                  weighting = "1/V",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi", "LRRd", "SMD", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  
  expect_equal(ES_agg_calculated, ES_agg)

  ## aggregating across all cases, behaviors, and phase-pairs
  
  ES_FE_calculated <- 
    ES_ests %>% 
    group_by(ES) %>% 
    mutate(ES_wt = 1 / SE^2) %>% 
    summarise(
      Est = sum(Est * ES_wt) / sum(ES_wt),
      SE = sqrt(1 / sum(ES_wt)),
      .groups = "drop"
    )
  
  ES_FE <- 
    batch_calc_ES(dat = Schmidt2007,
                  aggregate = c(Case_pseudonym, Behavior_type, Phase_num),
                  weighting = "1/V",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi", "LRRd", "SMD", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  
  expect_equal(ES_FE_calculated, ES_FE)
})

test_that("The aggregate argument in batch_calc_ES() has proper error handling.", {
  
  expect_error(
    batch_calc_ES(dat = Schmidt2007,
                  grouping = Case_pseudonym,
                  aggregate = c(Behavior_type, Phase_num),
                  weighting = Outcome,
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi", "LRRd", "SMD", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  )
  
  expect_error(
    batch_calc_ES(dat = Schmidt2007,
                  grouping = Case_pseudonym,
                  aggregate = c(Behavior_type, Phase_num),
                  weighting = Case_pseudonym,
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi", "LRRd", "SMD", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  )
})
