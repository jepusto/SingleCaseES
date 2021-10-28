context("Test SCD_effect_sizes Shiny app")

suppressWarnings(library(shiny))
suppressWarnings(library(shinytest))
suppressWarnings(library(dplyr))
suppressWarnings(library(stringr))
suppressWarnings(library(rvest))
suppressWarnings(library(purrr))

appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")
app <- ShinyDriver$new(appDir)


test_that("Title and tabs are correct", {
  
  skip_on_cran()
  
  # title
  appTitle <- app$getTitle()[[1]]
  expect_equal(appTitle, "Single-case effect size calculator")
  
  # tabs
  app$waitForValue("SCD_es_calculator")
  app$findWidget("SCD_es_calculator")$listTabs()
  expect_equal(app$findWidget("SCD_es_calculator")$listTabs(), c("About", "Calculator", "Batch Entry"))
  
})



check_single <- function(ES, ES_family, A_data, B_data) {
  
  improvement <- ifelse(ES == "LRRd", "decrease", "increase")
  
  app$setInputs(
    SCD_es_calculator = "Calculator",
    A_dat = toString(A_data),
    B_dat = toString(B_data),
    ES_family = ES_family,
    wait_=FALSE, values_=FALSE
  )
  
  if (ES_family == "Non-overlap") {
    app$setInputs(NOM_ES = ES)
  } else if (ES_family == "Parametric") {
    app$setInputs(parametric_ES = ES)
  }

  app$setInputs(improvement = improvement, digits = 5,
                wait_=FALSE, values_=FALSE)
  
  output_ES_name <- app$getValue(name = "ES_name")
  output_ES_value <- app$getValue(name = "result")
  
  return(data.frame(ES_name = output_ES_name, ES_value = output_ES_value))
  
}

test_that("Single-entry calculator works properly", {
  
  skip_on_cran()
  
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

  A_dat <- c(20, 20, 26, 25, 22, 23)
  B_dat <- c(28, 25, 24, 27, 30, 30, 29)
  
  # app
  NOMs_name <- c("PND", "PAND", "PEM", "IRD", "Tau_U", "NAP", "Tau", "Tau_BC")
  NOMs_app <- map_dfr(NOMs_name, ~ check_single(.x, ES_family = "Non-overlap", A_data = A_dat, B_data = B_dat))
  
  Parametric_name <- c("LOR", "LRRi", "LRRd", "LRM", "SMD")
  Parametric_app <- map_dfr(Parametric_name, ~ check_single(.x, ES_family = "Parametric", A_data = A_dat, B_data = B_dat))
  
  output_app <- 
    bind_rows(NOMs_app, Parametric_app) %>% 
    mutate(ES_value = gsub("(.*)<br><br><br><br>.*", "\\1", ES_value)) %>%
    tidyr::separate(ES_value, c("Est", "SE", "CI"), "<br>", fill = "right") %>%
    mutate(
      Est = as.numeric(stringr::str_remove(Est, "Effect size estimate: ")),
      SE = as.numeric(stringr::str_remove(SE, "Standard error: ")),
      CI = stringr::str_remove(CI, "95% CI: ")
    ) %>%
    arrange(ES_name) %>%
    rename(ES = ES_name)
  
  # package
  NOMs_pkg <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "increase", ES = NOMs_name)
  
  Parametric_pkg <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "increase", ES = setdiff(Parametric_name, "LRRd"))
  Parametric_pkg_LRRd <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "decrease", ES = "LRRd")
  
  output_pkg <- 
    bind_rows(NOMs_pkg, Parametric_pkg, Parametric_pkg_LRRd) %>% 
    mutate(
      ES = ifelse(ES %in% c("Tau-U", "Tau-BC"), ES, full_names[ES]),
      ES = as.character(ES),
      across(Est:CI_upper, ~ round(.x, 5)),
      CI = paste("[", CI_lower, ", ", CI_upper, "]", sep = ""),
      CI = ifelse(is.na(SE), NA, CI)
    ) %>%
    select(-c(CI_lower, CI_upper)) %>%
    arrange(ES)

  expect_equal(output_pkg, output_app, check.attributes = FALSE)
  
})



check_batch <- function(example_dat) {
  
  app$setInputs(SCD_es_calculator = "Batch Entry")
  app$setInputs(example = example_dat)
  app$setInputs(BatchEntryTabs = "Variables")

  if (example_dat == "McKissick") {
    app$setInputs(b_clusters = "Case_pseudonym")
  } else if (example_dat == "Schmidt2007") {
    app$setInputs(
      b_clusters = c("Behavior_type", "Case_pseudonym"),
      b_aggregate = "Phase_num"
    )
  } else if (example_dat == "Wright2012") {
    app$setInputs(b_clusters = "Participant")
  }

  app$setInputs(BatchEntryTabs = "Estimate")
  app$setInputs(bESno = c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U"))
  app$setInputs(bESpar = c("LOR", "LRRd", "LRRi", "LRM", "SMD"))
  app$setInputs(batchest = "click")
  
  output_app <- app$getValue(name = "batchTable")
  output_app_table <-
    as.data.frame(rvest::read_html(output_app) %>% rvest::html_table(fill = TRUE)) %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))

  return(output_app_table)
  
}

test_that("Batch calculator is correct", {
  
  skip_on_cran()
  
  # Shiny app
  McKissick_app <- check_batch("McKissick")
  Schmidt_app <- check_batch("Schmidt2007")
  Wright_app <- check_batch("Wright2012")

  # Package
  all_names <- c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U",
                 "LOR", "LRRd", "LRRi", "LRM", "SMD")
  data(McKissick)
  McKissick_pkg <-
    batch_calc_ES(dat = McKissick,
                  grouping = Case_pseudonym,
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  baseline_phase = "A",
                  intervention_phase = "B",
                  ES = all_names,
                  improvement = "decrease",
                  pct_change = FALSE,
                  scale = "count",
                  std_dev = "baseline",
                  confidence = 0.95,
                  pretest_trend = FALSE,
                  format = "long",
                  warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2)))
  
  data(Schmidt2007)
  Schmidt_pkg <-
      batch_calc_ES(dat = Schmidt2007,
                    grouping = c(Behavior_type, Case_pseudonym),
                    condition = Condition,
                    outcome = Outcome,
                    aggregate = c(Phase_num),
                    weighting = "equal",
                    session_number = Session_number,
                    baseline_phase = "A",
                    intervention_phase = "B",
                    ES = all_names,
                    improvement = "direction",
                    pct_change = FALSE,
                    scale = "Metric",
                    std_dev = "baseline",
                    confidence = 0.95,
                    pretest_trend = FALSE,
                    format = "long",
                    warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2)))
  
  data(Wright2012)
  Wright_pkg <-
      batch_calc_ES(dat = Wright2012,
                    grouping = c(Participant),
                    condition = Condition,
                    outcome = Prosocial_behavior,
                    session_number = Session,
                    baseline_phase = "baseline",
                    intervention_phase = "intervention A",
                    ES = all_names,
                    improvement = "increase",
                    pct_change = FALSE,
                    scale = "count",
                    std_dev = "baseline",
                    confidence = 0.95,
                    pretest_trend = FALSE,
                    format = "long",
                    warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2))) %>% 
    mutate(Participant = as.character(Participant))

  expect_equal(McKissick_pkg, McKissick_app, check.attributes = FALSE)
  expect_equal(Schmidt_pkg, Schmidt_app, check.attributes = FALSE)
  expect_equal(Wright_pkg, Wright_app, check.attributes = FALSE)

})




# Check data uploading
check_load <- function(file) {

  # myfile <- tempfile()
  # write.csv(file, file = myfile)

  data_path <- paste0("../testdata/", file)
  app$setInputs(SCD_es_calculator = "Batch Entry")

  if (str_detect(file, "csv")) {
    
    app$setInputs(dat_type = "dat")
    app$uploadFile(dat = data_path)
    
  } else if (str_detect(file, "xlsx")) {
    
    app$setInputs(dat_type = "xlsx")
    app$uploadFile(xlsx = data_path)
    
  }
  
  app$setInputs(BatchEntryTabs = "Variables")
  app$setInputs(b_clusters = "Case_pseudonym")
  app$setInputs(b_phase = "Condition")
  app$setInputs(session_number = "Session_number")
  app$setInputs(b_out = "Outcome")
  app$setInputs(bimprovement = "decrease")
  app$setInputs(BatchEntryTabs = "Estimate")
  app$setInputs(bESno = c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U"))
  app$setInputs(bESpar = c("LOR", "LRRd", "LRRi", "LRM", "SMD"))
  app$setInputs(boutScale = "count")
  app$setInputs(batchest = "click")
  
  output_app <- app$getValue(name = "batchTable")
  output_app_table <-
    as.data.frame(rvest::read_html(output_app) %>% rvest::html_table(fill = TRUE)) %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))
  
  return(output_app_table)

}


test_that("Data are uploaded correctly.", {
  
  skip_on_cran()
  
  # csv file
  output_csv <- check_load("McKissick.csv")
  
  # excel file
  output_xlsx <- check_load("McKissick.xlsx")
  
  all_names <- c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U",
                 "LOR", "LRRd", "LRRi", "LRM", "SMD")
  data(McKissick)
  McKissick_pkg <-
    batch_calc_ES(dat = McKissick,
                  grouping = Case_pseudonym,
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  baseline_phase = "A",
                  intervention_phase = "B",
                  ES = all_names,
                  improvement = "decrease",
                  pct_change = FALSE,
                  scale = "count",
                  std_dev = "baseline",
                  confidence = 0.95,
                  pretest_trend = FALSE,
                  format = "long",
                  warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2)))

  expect_equal(output_csv, McKissick_pkg, check.attributes = FALSE)
  expect_equal(output_xlsx, McKissick_pkg, check.attributes = FALSE)

})
