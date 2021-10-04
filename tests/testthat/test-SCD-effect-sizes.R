context("Test SCD_effect_sizes Shiny app")

suppressWarnings(library(shiny))
suppressWarnings(library(shinytest))
suppressWarnings(library(testthat))
suppressWarnings(library(dplyr))
suppressWarnings(library(stringr))
suppressWarnings(library(rvest))
suppressWarnings(library(purrr))

test_that("Title and tabs are correct", {
  
  appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")
  app <- ShinyDriver$new(appDir)
  
  # title
  appTitle <- app$getTitle()[[1]]
  expect_equal(appTitle, "Single-case effect size calculator")
  
  # tabs
  app$waitForValue("SCD_es_calculator")
  app$findWidget("SCD_es_calculator")$listTabs()
  expect_equal(app$findWidget("SCD_es_calculator")$listTabs(), c("About", "Calculator", "Batch Entry"))
  
})


check_single_NOMs <- function(ES, A_data, B_data) {

  appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")
  app <- ShinyDriver$new(appDir)
  app$setInputs(
    SCD_es_calculator = "Calculator",
    A_dat = toString(A_data),
    B_dat = toString(B_data),
    ES_family = "Non-overlap",
    NOM_ES = ES,
    improvement = "increase",
    digits = 3
  )

  output_ES_name <- app$getValue(name = "ES_name")
  output_ES_value <- app$getValue(name = "result")

  return(data.frame(ES_name = output_ES_name, ES_value = output_ES_value))

}


check_single_param <- function(ES, A_data, B_data) {

  appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")
  app <- ShinyDriver$new(appDir)
  improvement <- ifelse(ES == "LRRd", "decrease", "increase")

  app$setInputs(
    SCD_es_calculator = "Calculator",
    A_dat = toString(A_data),
    B_dat = toString(B_data),
    ES_family = "Parametric",
    parametric_ES = ES,
    improvement = improvement,
    digits = 3
  )

  output_ES_name <- app$getValue(name = "ES_name")
  output_ES_value <- app$getValue(name = "result")

  return(data.frame(ES_name = output_ES_name, ES_value = output_ES_value))

}


test_that("Single-entry calculator works properly", {

  full_names <- list(IRD = "Robust Improvement Rate Difference",
                     NAP = "Non-overlap of All Pairs",
                     PAND = "Percentage of All Non-overlapping Data",
                     PEM = "Percent Exceeding the Median",
                     PND = "Percentage of Non-overlapping Data",
                     Tau = "Tau",
                     Tau_BC = "Tau-BC",
                     Tau_U = "Tau-U",
                     LOR = "Log Odds Ratio",
                     LRRd = "Log Response Ratio (decreasing)",
                     LRRi = "Log Response Ratio (increasing)",
                     LRM = "Log Ratio of Medians",
                     SMD = "Standardized Mean Difference (within-case)")

  # Non-overlap
  A_dat <- c(20, 20, 26, 25, 22, 23)
  B_dat <- c(28, 25, 24, 27, 30, 30, 29)
  NOMs_name <- c("PND", "PAND", "PEM", "IRD", "Tau_U", "NAP", "Tau", "Tau_BC")
  NOMs_app <- map_dfr(NOMs_name, ~ check_single_NOMs(.x, A_data = A_dat, B_data = B_dat))
  NOMs_app_clean <-
    NOMs_app %>%
      mutate(ES_value = gsub("(.*)<br><br><br><br>.*", "\\1", ES_value)) %>%
      tidyr::separate(ES_value, c("Est", "SE", "CI"), "<br>", fill = "right") %>%
      mutate(
        Est = as.numeric(stringr::str_remove(Est, "Effect size estimate: ")),
        SE = as.numeric(stringr::str_remove(SE, "Standard error: ")),
        CI = stringr::str_remove(CI, "95% CI: ")
      ) %>%
      arrange(ES_name) %>%
      rename(ES = ES_name)

  NOMs_pkg <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "increase", ES = NOMs_name)
  
  NOMs_pkg_clean <-
    NOMs_pkg %>%
    mutate(
      ES = ifelse(ES %in% c("Tau-U", "Tau-BC"), ES, full_names[ES]),
      ES = as.character(ES),
      across(Est:CI_upper, ~ round(.x, 3)),
      CI = paste("[", CI_lower, ", ", CI_upper, "]", sep = ""),
      CI = ifelse(is.na(SE), NA, CI)
    ) %>%
    select(-c(CI_lower, CI_upper)) %>%
    arrange(ES)

  expect_equal(NOMs_pkg_clean, NOMs_app_clean, check.attributes = FALSE)


  # Parametric
  
  Parametric_name <- c("LOR", "LRRi", "LRRd", "LRM", "SMD")
  Parametric_app <- map_dfr(Parametric_name, ~ check_single_param(.x, A_data = A_dat, B_data = B_dat))
  Parametric_app_clean <-
    Parametric_app %>%
    mutate(ES_value = gsub("(.*)<br><br><br><br>.*", "\\1", ES_value)) %>%
    tidyr::separate(ES_value, c("Est", "SE", "CI"), "<br>") %>%
    mutate(
      Est = as.numeric(stringr::str_remove(Est, "Effect size estimate: ")),
      SE = as.numeric(stringr::str_remove(SE, "Standard error: ")),
      CI = stringr::str_remove(CI, "95% CI: ")
    ) %>%
    arrange(ES_name) %>%
    rename(ES = ES_name)

  Parametric_pkg <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "increase", ES = setdiff(Parametric_name, "LRRd"))
  Parametric_pkg_LRRd <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "decrease", ES = "LRRd")
  Parametric_pkg_clean <-
    Parametric_pkg %>%
    rbind(Parametric_pkg_LRRd) %>%
    mutate(
      ES = as.character(full_names[ES]),
      across(Est:CI_upper, ~ round(.x, 3)),
      CI = paste("[", CI_lower, ", ", CI_upper, "]", sep = "")
    ) %>%
    arrange(ES) %>%
    select(-c(CI_lower, CI_upper))

  expect_equal(Parametric_app_clean, Parametric_pkg_clean)
  
})



test_that("Batch calculator is correct", {

  # Shiny app
  appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")
  app <- ShinyDriver$new(appDir)

  app$setInputs(SCD_es_calculator = "Batch Entry")
  app$setInputs(example = "Schmidt2007")
  app$setInputs(BatchEntryTabs = "Variables")
  app$setInputs(b_clusters = c("Behavior_type", "Case_pseudonym"))
  app$setInputs(b_aggregate = "Phase_num")
  app$setInputs(BatchEntryTabs = "Estimate")
  app$setInputs(bESno = c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U"))
  app$setInputs(bESpar = c("LOR", "LRRd", "LRRi", "LRM", "SMD"))
  app$setInputs(batchest = "click")

  output_app <- app$getValue(name = "batchTable")
  output_app_table <-
    as.data.frame(read_html(output_app) %>% html_table(fill=TRUE)) %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))


  # Package
  data(Schmidt2007)
  dat <- Schmidt2007
  output_pkg <-
    suppressWarnings(
      batch_calc_ES(dat = dat,
                    grouping = c(Behavior_type, Case_pseudonym),
                    condition = Condition,
                    outcome = Outcome,
                    aggregate = c(Phase_num),
                    weighting = "1/V",
                    session_number = Session_number,
                    baseline_phase = "A",
                    intervention_phase = "B",
                    ES = c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U",
                           "LOR", "LRRd", "LRRi", "LRM", "SMD"),
                    improvement = "direction",
                    pct_change = FALSE,
                    scale = "Metric",
                    std_dev = "baseline",
                    confidence = 0.95,
                    pretest_trend = FALSE,
                    format = "long")
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2)))

  expect_equal(output_pkg, output_app_table, check.attributes = FALSE)

})

