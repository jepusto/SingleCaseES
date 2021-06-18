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


test_that("The aggregate argument works in batch_calc_ES().", {
  
  data("Schmidt2007")
  
  # long format using 1/V
  ## calculate aggregate by hand
  ES_ests <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym, Phase_num),
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd", "Tau"),
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
      SE = sqrt(1 / sum(ES_wt)))
  
  ## calculate aggregate by specifying the aggregating variables
  ES_agg <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym, Phase_num),
                  aggregate = c(Behavior_type, Case_pseudonym),
                  weighting = "1/V",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  expect_equal(ES_agg_calculated, ES_agg)
  
  # CI and wide format
  ES_agg_CI <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym, Phase_num),
                  aggregate = c(Behavior_type, Case_pseudonym),
                  weighting = "1/V",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd", "Tau"),
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
           starts_with("LRRi_"), starts_with("LRRd_"), starts_with("Tau_"))
  
  ES_agg_CI_wide <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym, Phase_num),
                  aggregate = c(Behavior_type, Case_pseudonym),
                  weighting = "1/V",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = .95,
                  format = "wide")
  
  expect_equal(ES_agg_CI_long2wide, ES_agg_CI_wide)
  
  # long format using equal weights
  ES_agg_equal_calculated <- 
    ES_ests %>% 
    group_by(Behavior_type, Case_pseudonym, ES) %>% 
    mutate(ES_wt = 1/n()) %>% 
    summarise(
      Est = weighted.mean(Est, w = ES_wt),
      SE = sqrt(sum(SE^2 * ES_wt^2) / sum(ES_wt)^2))
  
  ES_agg_equal <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym, Phase_num),
                  aggregate = c(Behavior_type, Case_pseudonym),
                  weighting = "equal",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd", "Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  
  expect_equal(ES_agg_equal_calculated, ES_agg_equal)
    
})
