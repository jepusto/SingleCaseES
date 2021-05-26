context("batch_calc_ES() works properly.")

suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))

test_that("batch_calc_ES() returns the same output 
          as calc_ES() for a data frame with no grouping variable.", {
  
  data("McKissick")
  dat1 <- McKissick %>% filter(Case_pseudonym == "Period 1")
  
  # use calc_ES()
  res_calc_ES <- calc_ES(condition = dat1$Condition, outcome = dat1$Outcome, baseline_phase = "A")
  
  # use batch_calc_ES()
  res_batch_calc_ES <- batch_calc_ES(dat = dat1, condition = Condition, outcome = Outcome, baseline_phase = "A")
  
  expect_identical(res_calc_ES, res_batch_calc_ES)
  
})


test_that("The aggregate argument works in batch_calc_ES().", {
  
  data("Schmidt2007")
  
  # using 1/V
  # calculate aggregate in two steps without specifying aggregating variables
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
      ES_est = sum(Est * ES_wt) / sum(ES_wt),
      ES_SE = sqrt(1 / sum(ES_wt)))
  
  # calculate aggregate by specifying the aggregating variables
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
  expect_identical(ES_agg_calculated, ES_agg)
  
  # equal weights
  ES_agg_equal_calculated <- 
    ES_ests %>% 
    group_by(Behavior_type, Case_pseudonym, ES) %>% 
    mutate(ES_wt = 0.5) %>% # ABAB so use 0.5, better average within summarise()
    summarise(
      ES_est = weighted.mean(Est, w = ES_wt),
      ES_SE = sqrt(sum(SE^2 * ES_wt^2) / sum(ES_wt)^2))
  
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
  
  expect_identical(ES_agg_equal_calculated, ES_agg_equal)
    
})
