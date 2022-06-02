context("Test SCD_effect_sizes Shiny app")

suppressWarnings(library(shiny))
suppressWarnings(library(shinytest))
suppressWarnings(library(dplyr))
suppressWarnings(library(stringr))
suppressWarnings(library(rvest))
suppressWarnings(library(purrr))

appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")

test_that("Title and tabs are correct", {
  
  skip_on_cran()
  
  if (dependenciesInstalled()) {
    app <- ShinyDriver$new(appDir, loadTimeout = 6e+05)
  }
  
  # title
  appTitle <- app$getTitle()[[1]]
  expect_equal(appTitle, "Single-case effect size calculator")
  
  # tabs
  app$waitForValue("SCD_es_calculator")
  app$findWidget("SCD_es_calculator")$listTabs()
  expect_equal(app$findWidget("SCD_es_calculator")$listTabs(), c("About", "Single-Series Calculator", "Multiple-Series Calculator"))
  
})



check_single <- function(app, ES, ES_family, A_data, B_data, Kendall = FALSE) {
  
  improvement <- ifelse(ES == "LRRd", "decrease", "increase")
  
  
  app$setInputs(
    SCD_es_calculator = "Single-Series Calculator",
    A_dat = toString(A_data),
    B_dat = toString(B_data),
    ES_family = ES_family,
    wait_ = FALSE,
    values_ = FALSE
  )
  
  if (ES_family == "Non-overlap") {
    app$setInputs(NOM_ES = ES, wait_=FALSE, values_=FALSE)
    if (ES == "Tau_BC") {
      tau_calculation <- if (Kendall) "Kendall" else "Nlap"
      app$setInputs(tau_calculation = tau_calculation, wait_=FALSE, values_=FALSE)
    }
  } else if (ES_family == "Parametric") {
    app$setInputs(parametric_ES = ES, wait_=FALSE, values_=FALSE)
  }
  
  app$setInputs(improvement = improvement, digits = 5, wait_=FALSE, values_=FALSE)
  
  Sys.sleep(0.5)
  output_ES_name <- app$getValue(name = "ES_name")
  output_ES_value <- app$getValue(name = "result")
  
  return(data.frame(ES_name = output_ES_name, ES_value = output_ES_value))
  
}

test_that("Single-entry calculator works properly", {
  
  skip_on_cran()

  if (dependenciesInstalled()) {
    app <- ShinyDriver$new(appDir, loadTimeout = 6e+05)
  }
  
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
  NOMs_app <- map_dfr(NOMs_name, ~ check_single(app, .x, ES_family = "Non-overlap", A_data = A_dat, B_data = B_dat))
  
  Parametric_name <- c("LOR", "LRRi", "LRRd", "LRM", "SMD")
  Parametric_app <- map_dfr(Parametric_name, ~ check_single(app, .x, ES_family = "Parametric", A_data = A_dat, B_data = B_dat))
  
  output_app <- 
    bind_rows(NOMs_app, Parametric_app) %>% 
    mutate(ES_value = str_remove(ES_value, "(<br>){4,}.*")) %>%
    tidyr::separate(ES_value, c("Est", "SE", "CI","baseline_SD"), "<br>", fill = "right") %>%
    mutate(
      Est = as.numeric(str_remove(Est, "Effect size estimate: ")),
      SE = as.numeric(str_remove(SE, "Standard error: ")),
      CI = str_remove(CI, "95% CI: "),
      baseline_SD = as.numeric(str_remove(baseline_SD, "Baseline SD: "))
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
      across(-ES, ~ round(.x, 5)),
      CI = paste("[", CI_lower, ", ", CI_upper, "]", sep = ""),
      CI = ifelse(is.na(SE), NA, CI),
    ) %>%
    select(-c(CI_lower, CI_upper)) %>%
    arrange(ES)

  expect_equal(output_pkg$ES, output_app$ES)
  expect_equal(output_pkg$Est, output_app$Est)
  expect_equal(output_pkg$SE, output_app$SE)
  expect_equal(output_pkg$CI, output_app$CI)
  expect_equal(output_pkg$baseline_SD, output_app$baseline_SD)
  
  # check when Kendall == TRUE for Tau_BC
  Kendall_app_res <- 
    check_single(app, ES = "Tau_BC", ES_family = "Non-overlap", A_data = A_dat, B_data = B_dat, Kendall = TRUE) %>% 
    mutate(ES_value = str_remove(ES_value, "(<br>){4,}.*")) %>%
    tidyr::separate(ES_value, c("Est", "SE", "CI"), "<br>", fill = "right") %>%
    mutate(
      Est = as.numeric(str_remove(Est, "Effect size estimate: ")),
      SE = as.numeric(str_remove(SE, "Standard error: ")),
      CI = str_remove(CI, "95% CI: ")
    ) 
  
  Kendall_pkg_res <- 
    calc_ES(A_data = A_dat, B_data = B_dat, ES = "Tau_BC", Kendall = TRUE) %>% 
    mutate(
      ES = as.character(ES),
      across(Est:CI_upper, ~ round(.x, 5)),
      CI = paste("[", CI_lower, ", ", CI_upper, "]", sep = ""),
      CI = ifelse(is.na(SE), NA, CI)
    ) %>%
    select(-c(CI_lower, CI_upper))
  
  expect_equal(Kendall_app_res, Kendall_pkg_res, check.attributes = FALSE)
  
})



check_batch <- function(app, example_dat, ES, Kendall = FALSE) {
  NOMs <- c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U")
  Parametrics <- c("LOR", "LRRd", "LRRi", "LRM", "SMD")
  
  bESno <- ES[ES %in% NOMs]
  bESpar <- ES[ES %in% Parametrics]
  
  app$setInputs(
    SCD_es_calculator = "Multiple-Series Calculator",
    example = example_dat,
    BatchEntryTabs = "Variables"
  )

  if (example_dat == "McKissick") {
    app$setInputs(b_clusters = "Case_pseudonym", wait_=FALSE, values_=FALSE)
  } else if (example_dat == "Schmidt2007") {
    app$setInputs(
      b_clusters = c("Behavior_type", "Case_pseudonym"),
      b_aggregate = "Phase_num", 
      wait_=FALSE, values_=FALSE
    )
  } else if (example_dat == "Wright2012") {
    app$setInputs(b_clusters = "Participant", wait_=FALSE, values_=FALSE)
  }
  
  app$setInputs(
    BatchEntryTabs = "Estimate", 
    bESno = bESno, 
    bESpar = bESpar, 
    wait_=FALSE, values_=FALSE
  )
  
  if (Kendall) {
    app$setInputs(btau_calculation = "Kendall", wait_=FALSE, values_=FALSE)
  } else {
    app$setInputs(btau_calculation = "Nlap", wait_=FALSE, values_=FALSE)
  }
  
  app$setInputs(batchest = "click")
  
  Sys.sleep(2)
  
  output_app <- app$getValue(name = "batchTable")
  
  output_app_table <-
    xml2::read_html(output_app) %>% 
    rvest::html_table(fill = TRUE) %>%
    as.data.frame() %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))

  return(output_app_table)
  
}

test_that("Batch calculator is correct", {
  
  skip_on_cran()

  if (dependenciesInstalled()) {
    app <- ShinyDriver$new(appDir, loadTimeout = 6e+05)
  }
  
  all_names <- c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U",
                 "LOR", "LRRd", "LRRi", "LRM", "SMD")
  
  # Shiny app
  McKissick_app <- 
    check_batch(app, example_dat = "McKissick", ES = all_names, Kendall = FALSE) %>% 
    dplyr::select(-baseline_SD)
  
  Schmidt_app <- check_batch(app, example_dat = "Schmidt2007", ES = all_names, Kendall = FALSE) 
  
  Wright_app <- 
    check_batch(app, example_dat = "Wright2012", ES = all_names, Kendall = FALSE) %>% 
    dplyr::select(-baseline_SD)

  # Package
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
                  Kendall = FALSE,
                  pretest_trend = FALSE,
                  format = "long",
                  warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2))) %>% 
    dplyr::select(-baseline_SD)
  
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
                    improvement = direction,
                    pct_change = FALSE,
                    scale = Metric,
                    std_dev = "baseline",
                    confidence = 0.95,
                    Kendall = FALSE,
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
                    Kendall = FALSE,
                    pretest_trend = FALSE,
                    format = "long",
                    warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2))) %>% 
    mutate(Participant = as.character(Participant)) %>% 
    dplyr::select(-baseline_SD)

  expect_equal(McKissick_pkg, McKissick_app, check.attributes = FALSE)
  expect_equal(Schmidt_pkg, Schmidt_app, check.attributes = FALSE)
  expect_equal(Wright_pkg, Wright_app, check.attributes = FALSE)
  
  
  # Kendall == TRUE
  # Shiny app
  McKissick_app_Kendall <- check_batch(app, "McKissick", ES = "Tau_BC", Kendall = TRUE)
  Schmidt_app_Kendall <- check_batch(app, "Schmidt2007", ES = "Tau_BC", Kendall = TRUE)
  Wright_app_Kendall <- check_batch(app, "Wright2012", ES = "Tau_BC", Kendall = TRUE)
  
  # Package
  McKissick_pkg_Kendall <-
    batch_calc_ES(dat = McKissick,
                  grouping = Case_pseudonym,
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  baseline_phase = "A",
                  intervention_phase = "B",
                  ES = "Tau_BC",
                  improvement = "decrease",
                  pct_change = FALSE,
                  scale = "count",
                  std_dev = "baseline",
                  confidence = 0.95,
                  Kendall = TRUE,
                  pretest_trend = FALSE,
                  format = "long",
                  warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2)))
  
  Schmidt_pkg_Kendall <-
    batch_calc_ES(dat = Schmidt2007,
                  grouping = c(Behavior_type, Case_pseudonym),
                  condition = Condition,
                  outcome = Outcome,
                  aggregate = c(Phase_num),
                  weighting = "equal",
                  session_number = Session_number,
                  baseline_phase = "A",
                  intervention_phase = "B",
                  ES = "Tau_BC",
                  improvement = direction,
                  pct_change = FALSE,
                  scale = Metric,
                  std_dev = "baseline",
                  confidence = 0.95,
                  Kendall = TRUE,
                  pretest_trend = FALSE,
                  format = "long",
                  warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2)))
  
  data(Wright2012)
  Wright_pkg_Kendall <-
    batch_calc_ES(dat = Wright2012,
                  grouping = c(Participant),
                  condition = Condition,
                  outcome = Prosocial_behavior,
                  session_number = Session,
                  baseline_phase = "baseline",
                  intervention_phase = "intervention A",
                  ES = "Tau_BC",
                  improvement = "increase",
                  pct_change = FALSE,
                  scale = "count",
                  std_dev = "baseline",
                  confidence = 0.95,
                  Kendall = TRUE,
                  pretest_trend = FALSE,
                  format = "long",
                  warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 2))) %>% 
    mutate(Participant = as.character(Participant))
  
  expect_equal(McKissick_pkg_Kendall, McKissick_app_Kendall, check.attributes = FALSE)
  expect_equal(Schmidt_pkg_Kendall, Schmidt_app_Kendall, check.attributes = FALSE)
  expect_equal(Wright_pkg_Kendall, Wright_app_Kendall, check.attributes = FALSE)

})



# Check data uploading

check_load <- function(app, file, Kendall = FALSE) {

  data_path <- paste0("../testdata/", file)
  # data_path <- system.file("tests","testdata", file, package = "SingleCaseES")
  
  app$setInputs(SCD_es_calculator = "Multiple-Series Calculator")

  if (str_detect(file, "csv")) {
    
    app$setInputs(dat_type = "dat")
    app$uploadFile(dat = data_path)
    
  } else if (str_detect(file, "xlsx")) {
    
    app$setInputs(dat_type = "xlsx", wait_ = FALSE, values_ = FALSE)
    app$uploadFile(xlsx = data_path)
    
  }
  
  app$setInputs(
    BatchEntryTabs = "Variables"
  )
  
  app$setInputs(
    b_clusters = "Case_pseudonym",
    b_phase = "Condition",
    session_number = "Session_number",
    b_out = "Outcome",
    bimprovement = "decrease",
    wait_ = FALSE, values_ = FALSE
  )
  
  app$setInputs(
    BatchEntryTabs = "Estimate"
  )
  
  app$setInputs(
    bESno = c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U"),
    bESpar = c("LOR", "LRRd", "LRRi", "LRM", "SMD")
  )
  
  app$setInputs(
    boutScale = "count",
    wait_ = FALSE, values_ = FALSE
  )
  
  if (Kendall == TRUE) {
    app$setInputs(btau_calculation = "Kendall", wait_=FALSE, values_=FALSE)
  } else if (Kendall == FALSE) {
    app$setInputs(btau_calculation = "Nlap", wait_=FALSE, values_=FALSE)
  }
  
  app$setInputs(batchest = "click")
  
  Sys.sleep(2)
  
  output_app <- app$getValue(name = "batchTable")
  
  output_app_table <-
    xml2::read_html(output_app) %>% 
    rvest::html_table(fill = TRUE) %>%
    as.data.frame() %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))
  
  return(output_app_table)

}


test_that("Data are uploaded correctly.", {
  
  skip_on_cran()
  
  if (dependenciesInstalled()) {
    app <- ShinyDriver$new(appDir, loadTimeout = 6e+05)
  }
  
  # csv file
  output_csv <- 
    check_load(app, "McKissick.csv") %>% 
    mutate(baseline_SD = as.numeric(if_else(baseline_SD == "-", NA_character_, baseline_SD)))
           
  # excel file
  output_xlsx <- 
    check_load(app, "McKissick.xlsx") %>% 
    mutate(baseline_SD = as.numeric(if_else(baseline_SD == "-", NA_character_, baseline_SD)))
  
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
                  Kendall = FALSE,
                  pretest_trend = FALSE,
                  format = "long",
                  warn = FALSE
    ) %>%
    mutate(across(Est:baseline_SD, ~ round(., 2))) 

  expect_equivalent(output_csv, McKissick_pkg)
  expect_equivalent(output_xlsx, McKissick_pkg)

})
