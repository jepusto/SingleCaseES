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
  
  # long format using nA as weights
  nA_weights <- 
    Schmidt2007 %>% 
    group_by(Behavior_type, Case_pseudonym, Phase_num, Condition) %>% 
    summarise(n = n()) %>%
    dplyr::filter(Condition == "A") %>% 
    dplyr::select(-Condition)
  
  ES_agg_nA_calculated <- 
    ES_ests %>% 
    dplyr::left_join(nA_weights) %>% 
    group_by(Behavior_type, Case_pseudonym, ES) %>% 
    summarise(
      Est = weighted.mean(Est, w = n),
      SE = sqrt(sum(SE^2 * n^2) / sum(n)^2),
      .groups = "drop"
    )
  
  ES_agg_nA <- 
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym),
                  aggregate = Phase_num,
                  weighting = "nA",
                  condition = Condition,
                  outcome = Outcome,
                  ES = c("LRRi","LRRd", "SMD","Tau"),
                  improvement = direction,
                  scale = "count",
                  bias_correct = TRUE,
                  confidence = NULL,
                  format = "long")
  
  expect_equal(ES_agg_nA_calculated, ES_agg_nA)
    
})


test_that("The aggregate argument in batch_calc_ES() works for effect size measures with no SEs.", {
  
  data("Schmidt2007")
  res_wo_SE <- batch_calc_ES(dat = Schmidt2007,
                       grouping = c(Behavior_type, Case_pseudonym),
                       condition = Condition,
                       outcome = Outcome,
                       aggregate = c(Phase_num),
                       weighting = "equal",
                       session_number = Session_number,
                       baseline_phase = "A",
                       intervention_phase = "B",
                       ES = c("IRD", "PAND", "PEM", "PND", "Tau_U"),
                       improvement = direction,
                       pct_change = FALSE,
                       scale = "other",
                       std_dev = "baseline",
                       confidence = 0.95,
                       pretest_trend = FALSE,
                       format = "long")
  
  res_w_SE <- batch_calc_ES(dat = Schmidt2007,
                             grouping = c(Behavior_type, Case_pseudonym),
                             condition = Condition,
                             outcome = Outcome,
                             aggregate = c(Phase_num),
                             weighting = "equal",
                             session_number = Session_number,
                             baseline_phase = "A",
                             intervention_phase = "B",
                             ES = c("IRD", "PAND", "PEM", "PND", "Tau_U", "LRRi","LRRd","SMD","Tau"),
                             improvement = direction,
                             pct_change = FALSE,
                             scale = "other",
                             std_dev = "baseline",
                             confidence = 0.95,
                             pretest_trend = FALSE,
                             format = "long")
  
  res_w_SE <- res_w_SE %>% dplyr::filter(is.na(SE)) %>% dplyr::select(-c(SE, CI_upper, CI_lower))
  
  expect_equal(res_wo_SE, res_w_SE)
  
  expect_error(batch_calc_ES(dat = Schmidt2007,
                             grouping = c(Behavior_type, Case_pseudonym),
                             condition = Condition,
                             outcome = Outcome,
                             aggregate = c(Phase_num),
                             weighting = "1/V",
                             session_number = Session_number,
                             baseline_phase = "A",
                             intervention_phase = "B",
                             ES = c("IRD", "PAND", "PEM", "PND", "Tau_U"),
                             pct_change = FALSE,
                             scale = "other",
                             std_dev = "baseline",
                             confidence = 0.95,
                             pretest_trend = FALSE,
                             format = "long"))
  
  res_nA_wo_SE <- batch_calc_ES(dat = Schmidt2007,
                             grouping = c(Behavior_type, Case_pseudonym),
                             condition = Condition,
                             outcome = Outcome,
                             aggregate = c(Phase_num),
                             weighting = "nA",
                             session_number = Session_number,
                             baseline_phase = "A",
                             intervention_phase = "B",
                             ES = c("IRD", "PAND", "PEM", "PND", "Tau_U"),
                             improvement = direction,
                             pct_change = FALSE,
                             scale = "other",
                             std_dev = "baseline",
                             confidence = 0.95,
                             pretest_trend = FALSE,
                             format = "long")
  
  res_nA_w_SE <- batch_calc_ES(dat = Schmidt2007,
                            grouping = c(Behavior_type, Case_pseudonym),
                            condition = Condition,
                            outcome = Outcome,
                            aggregate = c(Phase_num),
                            weighting = "nA",
                            session_number = Session_number,
                            baseline_phase = "A",
                            intervention_phase = "B",
                            ES = c("IRD", "PAND", "PEM", "PND", "Tau_U", "LRRi","LRRd","SMD","Tau"),
                            improvement = direction,
                            pct_change = FALSE,
                            scale = "other",
                            std_dev = "baseline",
                            confidence = 0.95,
                            pretest_trend = FALSE,
                            format = "long")
  
  res_nA_w_SE <- res_nA_w_SE %>% dplyr::filter(is.na(SE)) %>% dplyr::select(-c(SE, CI_upper, CI_lower))
  
  expect_equal(res_nA_wo_SE, res_nA_w_SE)
  
  
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

test_that("Synonyms for weighting argument in batch_calc_ES() are equivalent.", {
  
  agg_weights <- c("1/V",
                   "equal","Equal",
                   "nA","n_A",
                   "nB","n_B",
                   "nAnB","nA*nB","n_A*n_B","nA * nB","n_A * n_B",
                   "1/nA+1/nB","1/nA + 1/nB","1/n_A+1/n_B","1/n_A + 1/n_B")
  
  agg_res <- lapply(agg_weights, 
                 \(w) batch_calc_ES(
                      dat = Schmidt2007,
                      grouping = Case_pseudonym,
                      aggregate = c(Behavior_type, Phase_num),
                      weighting = w,
                      condition = Condition,
                      outcome = Outcome,
                      ES = c("LRRi", "LRRd", "SMD", "Tau"),
                      improvement = direction,
                      scale = "count",
                      bias_correct = TRUE,
                      confidence = NULL,
                      format = "long"
                      )
  )
  
  expect_identical(agg_res[[2]], agg_res[[3]])  
  expect_identical(agg_res[[4]], agg_res[[5]])
  expect_identical(agg_res[[6]], agg_res[[7]])
  expect_identical(agg_res[[8]], agg_res[[9]])
  expect_identical(agg_res[[8]], agg_res[[10]])
  expect_identical(agg_res[[8]], agg_res[[11]])
  expect_identical(agg_res[[8]], agg_res[[12]])
  expect_identical(agg_res[[13]], agg_res[[14]])
  expect_identical(agg_res[[13]], agg_res[[15]])
  expect_identical(agg_res[[13]], agg_res[[16]])
  
  
})


test_that("batch_calc_ES() works properly if any variable has the same name as the allowable input argument.", {
  
  data("McKissick")
  data("Schmidt2007")
  
  McKissick_improvement1 <- 
    McKissick %>%
    mutate(decrease = "decrease", increase = "increase", count = "count") %>%
    batch_calc_ES(
      grouping = Case_pseudonym,
      weighting = "1/V",
      condition = Condition,
      outcome = Outcome,
      ES = c("LRRi", "LRRd", "SMD", "Tau"),
      improvement = "decrease",
      scale = "count",
      bias_correct = TRUE,
      confidence = NULL,
      format = "long"
    )
  
  McKissick_improvement2 <- 
    McKissick %>%
    mutate(decrease = "decrease", increase = "increase", ) %>%
    batch_calc_ES(
      grouping = Case_pseudonym,
      weighting = "1/V",
      condition = Condition,
      outcome = Outcome,
      ES = c("LRRi", "LRRd", "SMD", "Tau"),
      improvement = decrease,
      scale = "count",
      bias_correct = TRUE,
      confidence = NULL,
      format = "long"
    )
  
  expect_equal(McKissick_improvement1, McKissick_improvement2)
  
  McKissick_improvement3 <- 
    McKissick %>%
    mutate(decrease = "something", increase = "or other", count = "your blessing") %>%
    batch_calc_ES(
      grouping = Case_pseudonym,
      weighting = "1/V",
      condition = Condition,
      outcome = Outcome,
      ES = c("LRRi", "LRRd", "SMD", "Tau"),
      improvement = "decrease",
      scale = "count",
      bias_correct = TRUE,
      confidence = NULL,
      format = "long"
    )
  
  Schmidt_scale <- 
    Schmidt2007 %>%
    mutate(
      proportion = "rate", 
      count = "count", 
      proportion = "proportion", 
      percentage = "percentage", 
      other = "other"
    ) %>% 
    batch_calc_ES(
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
      format = "long"
    )
  
  expect_output(str(Schmidt_scale), "tibble")
  
})

test_that("The trunc_const argument for NAP and Tau works inside batch_calc_ES().", {
  
  Shogren_res <- 
    batch_calc_ES(dat = Shogren,
                  grouping = c(Study, Case, Measure),
                  condition = Phase,
                  outcome = outcome,
                  ES = c("NAP","Tau", "Tau-BC"),
                  improvement = direction,
                  Kendall = FALSE,
                  trunc_const = TRUE,
                  format = "wide"
                  ) %>%
    select(ends_with("_trunc"))
  
  expect_identical(Shogren_res$Tau_trunc, Shogren_res$`Tau-BC_trunc`)
  expect_equal(Shogren_res$Tau_trunc, 2 * Shogren_res$NAP_trunc)
})


test_that("Passing a variable to the scale argument works when the variable is also a function name.", {

  data("Shogren")
  
  Shogren <- Shogren %>%
    mutate_at(vars(Study, Measure, Case), as.character) %>%
    group_by(Study, Case, Measure) %>%
    mutate(
      session_number = row_number(),
      DV_scale = ifelse(Recording_procedure == "EC", "count", "proportion"),
      scale = DV_scale,
      intervals = Session_length * 60 / interval_length,
      mean = intervals,
      smd_improvement = ifelse(direction == "increase", "decrease", "increase")
    ) %>%
    ungroup()
  
  Shogren_DV_scale <- batch_calc_ES(
      dat = Shogren,
      grouping = c(Study, Measure, Case),
      condition = Phase,
      baseline_phase = "No Choice",
      outcome = outcome,
      session_number = session_number,
      improvement = smd_improvement,
      ES = c("SMD","LRRi","LRRd","NAP"),
      scale = DV_scale,
      intervals = intervals,
      observation_length = Session_length,
      format = "wide",
      std_dev = "both",
      bias_correct = FALSE
    )
  
  Shogren_scale <- batch_calc_ES(
    dat = Shogren,
    grouping = c(Study, Measure, Case),
    condition = Phase,
    baseline_phase = "No Choice",
    outcome = outcome,
    session_number = session_number,
    improvement = smd_improvement,
    ES = c("SMD","LRRi","LRRd","NAP"),
    scale = scale,
    intervals = intervals,
    observation_length = Session_length,
    format = "wide",
    std_dev = "both",
    bias_correct = FALSE
  )

  Shogren_mean <- batch_calc_ES(
    dat = Shogren,
    grouping = c(Study, Measure, Case),
    condition = Phase,
    baseline_phase = "No Choice",
    outcome = outcome,
    session_number = session_number,
    improvement = smd_improvement,
    ES = c("SMD","LRRi","LRRd","NAP"),
    scale = DV_scale,
    intervals = mean,
    observation_length = Session_length,
    format = "wide",
    std_dev = "both",
    bias_correct = FALSE
  )
  
  expect_identical(Shogren_DV_scale, Shogren_scale)
  expect_identical(Shogren_DV_scale, Shogren_mean)
})

