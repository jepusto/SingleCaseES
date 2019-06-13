context("Phase selection arguments in batch_calc_ES() work as intended.")

data("Wright2012")

Wright_long <- tidyr::gather(Wright2012, Behavior, Outcome, tidyselect::ends_with("_behavior"))

A_B1_manual <- 
  batch_calc_ES(
    subset(Wright_long, Condition %in% c("baseline","intervention A")),
    grouping = c(Participant, Behavior),
    condition = Condition,
    outcome = Outcome
  )

A_B1 <- 
  batch_calc_ES(
    Wright_long,
    grouping = c(Participant, Behavior),
    condition = Condition,
    outcome = Outcome,
    baseline_phase = "baseline",
    intervention_phase = "intervention A"
  )

A_B2_manual <- 
  batch_calc_ES(
    subset(Wright_long, Condition %in% c("baseline","intervention B")),
    grouping = c(Participant, Behavior),
    condition = Condition,
    outcome = Outcome
  )

A_B2 <- 
  batch_calc_ES(
    Wright_long,
    grouping = c(Participant, Behavior),
    condition = Condition,
    outcome = Outcome,
    baseline_phase = "baseline",
    intervention_phase = "intervention B"
  )

B1_B2_manual <- 
  batch_calc_ES(
    subset(Wright_long, Condition %in% c("intervention A","intervention B")),
    grouping = c(Participant, Behavior),
    condition = Condition,
    outcome = Outcome
  )

B1_B2 <- 
  batch_calc_ES(
    Wright_long,
    grouping = c(Participant, Behavior),
    condition = Condition,
    outcome = Outcome,
    baseline_phase = "intervention A",
    intervention_phase = "intervention B"
  )

test_that("Phase selection arguments work properly.", {
  expect_identical(A_B1, A_B1_manual)
  expect_identical(A_B2, A_B2_manual)
  expect_identical(B1_B2, B1_B2_manual)
})

test_that("Warnings if intervention_phase is not specified when condition has more than two levels.",{
  expect_warning(
    batch_calc_ES(
      Wright_long,
      grouping = c(Participant, Behavior),
      condition = Condition,
      outcome = Outcome
    )
  )
})

test_that("Errors if baseline_phase or intervention_phase is not a value of condition.",{
  expect_error(
    batch_calc_ES(
      Wright_long,
      grouping = c(Participant, Behavior),
      condition = Condition,
      outcome = Outcome,
      baseline_phase = "base",
      intervention_phase = "intervention A"
    )
  )
  
  expect_error(
    batch_calc_ES(
      Wright_long,
      grouping = c(Participant, Behavior),
      condition = Condition,
      outcome = Outcome,
      baseline_phase = "baseline",
      intervention_phase = "intervention"
    )
  )
  
})