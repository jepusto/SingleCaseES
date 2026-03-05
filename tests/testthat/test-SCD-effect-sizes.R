data("McKissick", package = "SingleCaseES")
#skip("Need to refactor Shiny app tests using shinytest2.")
skip_if_not_installed("shiny")
skip_if_not_installed("shinytest2")
skip_if_not_installed("stringr")
skip_if_not_installed("rvest")
skip_if_not_installed("ggplot2")
skip_if_not_installed("markdown")
skip_if_not_installed("readxl")
skip_if_not_installed("glue")
skip_if_not_installed("janitor")
skip_if_not_installed("rclipboard")
skip_if_not_installed("Kendall")


suppressWarnings(library(shiny))
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(stringr))
suppressWarnings(library(rvest))
suppressWarnings(library(xml2))
suppressWarnings(library(purrr))
suppressWarnings(library(shinytest2))

#skip_if_not(dependenciesInstalled())

appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")

test_that("Title and tabs are correct", {
  
  skip_on_cran()
  #ShinyDriver$new -> AppDriver$new
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  
  app$wait_for_idle()
  appTitle <- unlist(app$get_js("document.title"))[[1]]
  expect_equal(appTitle, "Single-case effect size calculator")
  
  tab_html <- app$get_html("#SCD_es_calculator")
  expect_true(grepl("About", tab_html, fixed = TRUE))
  expect_true(grepl("Single-Series Calculator", tab_html, fixed = TRUE))
  expect_true(grepl("Multiple-Series Calculator", tab_html, fixed = TRUE))
  
})



check_single <- function(app, ES, ES_family, A_data, B_data, Kendall = FALSE, goal = NULL) {
  
  improvement <- ifelse(ES == "LRRd", "decrease", "increase")
  # CHANGED: setInputs -> set_inputs
  app$set_inputs(
    SCD_es_calculator = "Single-Series Calculator",
    A_dat = toString(A_data),
    B_dat = toString(B_data),
    ES_family = ES_family,
  )
  app$wait_for_idle()
  if (ES_family == "Non-overlap") {
    app$set_inputs(NOM_ES = ES)
    app$wait_for_idle()
    if (ES == "Tau_BC") {
      tau_calculation <- if (Kendall) "Kendall" else "Nlap"
      app$set_inputs(tau_calculation = tau_calculation)
      app$wait_for_idle()
    }
  } else if (ES_family == "Parametric") {
    app$set_inputs(parametric_ES = ES)
    if (ES == "PoGO") {
      app$set_inputs(goal_level = goal)
      app$wait_for_idle()
    }
  }
  
  app$set_inputs(improvement = improvement, digits = 5)
  # CHANGED: getValue(name=) -> get_value(output=)
  output_ES_name <- app$get_value(output = "ES_name")
  output_ES_value <- app$get_value(output = "result")
  
  return(data.frame(ES_name = output_ES_name, ES_value = output_ES_value))
  
}

test_that("Single-entry calculator works properly", {
  skip_on_cran()
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  
  full_names <- list(
    IRD = "Robust Improvement Rate Difference",
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
    PoGO = "Percent of Goal Obtained",
    SMD = "Standardized Mean Difference (within-case)"
  )
  
  A_dat <- c(20, 20, 26, 25, 22, 23)
  B_dat <- c(28, 25, 24, 27, 30, 30, 29)
  goal <- 40
  
  NOMs_name <- c("PND", "PAND", "PEM", "IRD", "Tau_U", "NAP", "Tau", "Tau_BC")
  NOMs_app <- map_dfr(
    NOMs_name,
    ~ check_single(app, .x, ES_family = "Non-overlap", A_data = A_dat, B_data = B_dat)
  )
  
  Parametric_name <- c("LOR", "LRRi", "LRRd", "LRM", "PoGO", "SMD")
  Parametric_app <- map_dfr(
    Parametric_name,
    ~ check_single(app, .x, ES_family = "Parametric", A_data = A_dat, B_data = B_dat, goal = goal)
  )
  
  output_app <-
    bind_rows(NOMs_app, Parametric_app) %>%
    mutate(ES_value = str_remove(ES_value, "(<br>){4,}.*")) %>%
    separate(ES_value, c("Est","SE","CI","baseline_SD"), "<br\\s*/?>", fill = "right", extra = "drop")%>%
    mutate(
      Est = as.numeric(na_if(gsub("[^0-9.-]", "", Est), "")),
      SE  = as.numeric(na_if(gsub("[^0-9.-]", "", SE), "")),
      CI  = str_remove(CI, "95% CI: "),
      baseline_SD = as.numeric(na_if(gsub("[^0-9.-]", "", baseline_SD), ""))
    )%>%
    arrange(ES_name) %>%
    rename(ES = ES_name)
  
  NOMs_pkg <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "increase", ES = NOMs_name)
  Parametric_pkg <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "increase", goal = goal, ES = setdiff(Parametric_name, "LRRd"))
  Parametric_pkg_LRRd <- calc_ES(A_data = A_dat, B_data = B_dat, improvement = "decrease", ES = "LRRd")
  
  output_pkg <-
    bind_rows(NOMs_pkg, Parametric_pkg, Parametric_pkg_LRRd) %>%
    mutate(
      ES = ifelse(ES %in% c("Tau-U", "Tau-BC"), ES, full_names[ES]),
      ES = as.character(ES),
      across(c(Est, SE, baseline_SD), ~ round(.x, 5)),
      across(starts_with("CI_"), ~ formatC(.x, digits = 5, format = "f")),
      CI = paste("[", CI_lower, ", ", CI_upper, "]", sep = ""),
      CI = ifelse(is.na(SE), NA, CI)
    ) %>%
    select(-c(CI_lower, CI_upper)) %>%
    arrange(ES)
  
  expect_equal(output_pkg$ES, output_app$ES)
  expect_equal(output_pkg$Est, output_app$Est)
  expect_equal(output_pkg$SE, output_app$SE)
  expect_equal(output_pkg$CI, output_app$CI)
  expect_equal(output_pkg$baseline_SD, output_app$baseline_SD)
  
  Kendall_app_res <-
    check_single(app, ES = "Tau_BC", ES_family = "Non-overlap", A_data = A_dat, B_data = B_dat, Kendall = TRUE) %>%
    mutate(ES_value = str_remove(ES_value, "(<br>){4,}.*")) %>%
    separate(ES_value, c("Est", "SE", "CI"), "<br\\s*/?>", fill = "right", extra = "drop") %>%
    mutate(
      Est = as.numeric(gsub("[^0-9.-]", "", Est)),
      SE  = as.numeric(gsub("[^0-9.-]", "", SE)),
      CI  = str_remove(CI, "95% CI: ")
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

check_batch <- function(app, example_dat, ES, digits = 4, goal = NULL, Kendall = FALSE) {
  NOMs <- c("IRD", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_BC", "Tau_U")
  Parametrics <- c("LOR", "LRRd", "LRRi", "LRM", "PoGO", "SMD")
  
  bESno <- ES[ES %in% NOMs]
  bESpar <- ES[ES %in% Parametrics]
  
  app$set_inputs(
    SCD_es_calculator = "Multiple-Series Calculator",
    example = example_dat,
    BatchEntryTabs = "Variables"
  )
  
  if (example_dat == "McKissick") {
    app$set_inputs(b_clusters = "Case_pseudonym")
  } else if (example_dat == "Schmidt2007") {
    app$set_inputs(
      b_clusters = c("Behavior_type", "Case_pseudonym"),
      b_aggregate = "Phase_num"
    )
  } else if (example_dat == "Wright2012") {
    app$set_inputs(b_clusters = "Participant")
  }
  
  app$set_inputs(
    BatchEntryTabs = "Estimate",
    bESno = bESno,
    bESpar = bESpar,
    bdigits = digits
  )
  
  app$set_inputs(btau_calculation = if (Kendall) "Kendall" else "Nlap")
  app$set_inputs(bcomgoal = goal)
  
  app$set_inputs(batchest = "click")
  
  app$wait_for_idle()
  
  output_app <- app$get_value(output = "batchTable")
  
  tbl <- rvest::html_table(rvest::read_html(output_app), fill = TRUE)[[1]]
  tbl[tbl == "-"] <- NA
  tbl[, intersect(c("Est","SE","CI_lower","CI_upper","baseline_SD"), names(tbl))] <-
    lapply(tbl[, intersect(c("Est","SE","CI_lower","CI_upper","baseline_SD"), names(tbl)), drop = FALSE], as.numeric)
  tbl
  
}

test_that("Batch calculator is correct", {
  skip_on_cran()
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  
  all_names <- c(
    "IRD","NAP","PAND","PEM","PND","Tau","Tau_BC","Tau_U",
    "LOR","LRRd","LRRi","LRM","PoGO","SMD"
  )
  
  expect_error(check_batch(app, example_dat = "McKissick", ES = all_names, Kendall = FALSE, goal = NULL))
  
  
  McKissick_app <-
    check_batch(app, example_dat = "McKissick", ES = all_names, Kendall = FALSE, goal = 1) %>%
    select(-baseline_SD)
  
  Schmidt_app <- check_batch(app, example_dat = "Schmidt2007", ES = all_names, Kendall = FALSE, goal = 50)
  
  Wright_app <-
    check_batch(app, example_dat = "Wright2012", ES = all_names, Kendall = FALSE, goal = 0) %>%
    select(-baseline_SD)
  
  Olszewski_app <-
    check_batch(app, example_dat = "Olszewski2017", ES = all_names, Kendall = FALSE, goal = 20) %>%
    select(-baseline_SD)
  
  data(McKissick)
  McKissick_pkg <-
    batch_calc_ES(
      dat = McKissick,
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
      goal = 1,
      Kendall = FALSE,
      pretest_trend = FALSE,
      format = "long",
      warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 4L))) %>%
    dplyr::select(-baseline_SD)
  
  data(Schmidt2007)
  Schmidt_pkg <-
    batch_calc_ES(
      dat = Schmidt2007,
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
      goal = 50,
      Kendall = FALSE,
      pretest_trend = FALSE,
      format = "long",
      warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 4L)))
  
  data(Wright2012)
  Wright_pkg <-
    batch_calc_ES(
      dat = Wright2012,
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
      goal = 0,
      Kendall = FALSE,
      pretest_trend = FALSE,
      format = "long",
      warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 4L))) %>%
    mutate(Participant = as.character(Participant)) %>%
    select(-baseline_SD)
  
  data("Olszewski2017")
  Olszewski_pkg <-
    batch_calc_ES(
      dat = Olszewski2017,
      condition = "phase",
      outcome = "score",
      grouping = "behavior",
      phase_vals = c("A", "B"),
      direction = "increase",
      session_num = "session",
      scale = "count",
      intervals = NA,
      observation_length = NA,
      ES = all_names,
      std_dev = "baseline",
      confidence = 0.95,
      goal = 20,
      Kendall = FALSE,
      pretest_trend = FALSE,
      format = "long",
      warn = FALSE
    ) %>%
    select(-baseline_SD) %>%
    mutate(across(Est:CI_upper, ~ round(., 4L)))
  
  expect_equal(McKissick_pkg, McKissick_app, check.attributes = FALSE)
  expect_equal(Schmidt_pkg, Schmidt_app, check.attributes = FALSE)
  expect_equal(Wright_pkg, Wright_app, check.attributes = FALSE)
  expect_equal(Olszewski_pkg, Olszewski_app, check.attributes = FALSE)
  
  McKissick_app_Kendall <- check_batch(app, "McKissick", ES = "Tau_BC", Kendall = TRUE)
  Schmidt_app_Kendall <- check_batch(app, "Schmidt2007", ES = "Tau_BC", Kendall = TRUE)
  Wright_app_Kendall <- check_batch(app, "Wright2012", ES = "Tau_BC", Kendall = TRUE)
  Olszewski_app_Kendall <- check_batch(app, "Olszewski2017", ES = "Tau_BC", Kendall = TRUE)
  
  McKissick_pkg_Kendall <-
    batch_calc_ES(
      dat = McKissick,
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
    mutate(across(Est:CI_upper, ~ round(., 4L)))
  
  Schmidt_pkg_Kendall <-
    batch_calc_ES(
      dat = Schmidt2007,
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
    mutate(across(Est:CI_upper, ~ round(., 4L)))
  
  Wright_pkg_Kendall <-
    batch_calc_ES(
      dat = Wright2012,
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
    mutate(across(Est:CI_upper, ~ round(., 4L))) %>%
    mutate(Participant = as.character(Participant))
  
  Olszewski_pkg_Kendall <-
    batch_calc_ES(
      dat = Olszewski2017,
      condition = "phase",
      outcome = "score",
      grouping = "behavior",
      phase_vals = c("A", "B"),
      direction = "increase",
      session_num = "session",
      scale = "count",
      intervals = NA,
      observation_length = NA,
      ES = "Tau-BC",
      confidence = 0.95,
      Kendall = TRUE,
      pretest_trend = FALSE,
      format = "long",
      warn = FALSE
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 4L)))
  
  expect_equal(McKissick_pkg_Kendall, McKissick_app_Kendall, check.attributes = FALSE)
  expect_equal(Schmidt_pkg_Kendall, Schmidt_app_Kendall, check.attributes = FALSE)
  expect_equal(Wright_pkg_Kendall, Wright_app_Kendall, check.attributes = FALSE)
  expect_equal(Olszewski_pkg_Kendall, Olszewski_app_Kendall, check.attributes = FALSE)
})

check_load <- function(app, file, digits = 6, Kendall = FALSE) {
  
  data_path <- testthat::test_path("..", "testdata", file)
  
  app$set_inputs(SCD_es_calculator = "Multiple-Series Calculator")
  
  if (str_detect(file, "csv")) {
    app$set_inputs(dat_type = "dat")
    app$upload_file(dat = data_path)
  } else if (str_detect(file, "xlsx")) {
    app$set_inputs(dat_type = "xlsx")
    app$upload_file(xlsx = data_path)
  }
  
  app$wait_for_idle()
  
  app$set_inputs(BatchEntryTabs = "Variables")
  app$wait_for_idle()
  
  app$set_inputs(
    b_clusters = "Case_pseudonym",
    b_phase = "Condition",
    session_number = "Session_number",
    b_out = "Outcome",
    bimprovement = "decrease"
  )
  
  app$set_inputs(BatchEntryTabs = "Estimate")
  app$wait_for_idle()
  app$set_inputs(
    bESno = c("IRD","NAP","PAND","PEM","PND","Tau","Tau_BC","Tau_U"),
    bESpar = c("LOR","LRRd","LRRi","LRM","SMD")
  )
  
  app$wait_for_idle()
  
  app$set_inputs(
    boutScale = "count", 
    bdigits = digits
  )
  app$set_inputs(btau_calculation = if (Kendall) "Kendall" else "Nlap")
  
  # app$wait_for_idle()
  app$set_inputs(batchest = "click")
  
  output_app <- app$get_value(output = "batchTable")
  
  read_html(output_app) %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))
}

test_that("Data are uploaded correctly.", {
  
  skip_on_cran()
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  
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
    mutate(across(Est:baseline_SD, ~ round(as.numeric(.), 6L))) %>%
    as.data.frame()
  
  expect_equal(output_csv, McKissick_pkg, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(output_xlsx, McKissick_pkg, ignore_attr = TRUE, tolerance = 1e-4)
})

#block 19
test_that("calcPhasePair works in the app.", {
  skip_on_cran()
  
  NOMs <- c("IRD","NAP","PAND","PEM","PND","Tau","Tau_BC","Tau_U")
  Parametrics <- c("LRRd","LRRi","LRM","SMD")
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  on.exit(app$stop(), add = TRUE)
  data_path <- testthat::test_path("..", "testdata", "ex_issue73.csv")
  
  app$set_inputs(
    SCD_es_calculator = "Multiple-Series Calculator",
    dat_type = "dat"
  )
  
  app$upload_file(dat = data_path)
  
  app$set_inputs(BatchEntryTabs = "Variables", calcPhasePair = TRUE)
  
  app$set_inputs(
    b_clusters = c("Behavior_type", "Case_pseudonym")
  )
  
  app$set_inputs(
    b_aggregate = "phase_pair_calculated",
    b_phase = "Condition",
    session_number = "Session_number",
    b_out = "Outcome",
    bimprovement = "series"
  )
  
  app$set_inputs(bseldir = "Direction")
  
  app$set_inputs(
    BatchEntryTabs = "Estimate",
    bESno = NOMs,
    bESpar = Parametrics
  )
  
  app$set_inputs(
    boutScale = "series",
    bscalevar = "Metric",
    bdigits = 4
  )
  
  app$set_inputs(batchest = "click")
  
  output_app <- app$get_value(output = "batchTable")
  output_app_table <-
    read_html(output_app) %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))
  data <- read.csv(data_path)
  
  dat <-
    data %>%
    group_by(Behavior_type, Case_pseudonym) %>%
    mutate(phase_pair_calculated = calc_phase_pairs(Condition, session = Session_number)) %>%
    ungroup()
  
  output_pkg <-
    SingleCaseES::batch_calc_ES(
      dat = dat,
      grouping = c(Behavior_type, Case_pseudonym),
      condition = Condition,
      outcome = Outcome,
      aggregate = c(phase_pair_calculated),
      weighting = "equal",
      session_number = Session_number,
      baseline_phase = "A",
      intervention_phase = "B",
      ES = c(NOMs, Parametrics),
      improvement = Direction,
      pct_change = FALSE,
      scale = Metric,
      intervals = NA,
      observation_length = NA,
      std_dev = "baseline",
      confidence = 0.95,
      Kendall = FALSE,
      pretest_trend = FALSE,
      format = "long"
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 4L)))%>%
    as.data.frame()
  
  expect_equal(output_app_table, output_pkg, check.attributes = FALSE)
})

#block 20
check_bint_bobslen <- function(file, bint = NA, bobslen = NA) {
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  data_path <- testthat::test_path("..", "testdata", file)

  app$set_inputs(SCD_es_calculator = "Multiple-Series Calculator")
  app$set_inputs(dat_type = "dat")
  app$upload_file(dat = data_path)
  app$set_inputs(BatchEntryTabs = "Variables")
  app$set_inputs(b_clusters = "Case_pseudonym")
  app$set_inputs(session_number = "Session_number")
  app$set_inputs(b_phase = "Condition")
  app$set_inputs(b_out = "Outcome")
  app$set_inputs(BatchEntryTabs = "Estimate")
  app$set_inputs(bESpar = c("LOR", "LRRi", "LRRd"))
  app$wait_for_idle()
  app$set_inputs(bimprovement = "increase")
  app$set_inputs(boutScale = "Percentage")
  app$set_inputs(bintervals = bint)
  app$set_inputs(bobslength = bobslen)
  app$set_inputs(bdigits = 4)
  app$wait_for_idle()
  app$set_inputs(batchest = "click")
  app$wait_for_idle()
  output_app <- app$get_value(output = "batchTable")
  
  output_app_table <-
    read_html(output_app) %>% 
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))
  
  return(output_app_table)
  
}

test_that("The bintervals and bobslength options work in the app.", {
  
  skip_on_cran()
  
  out_app_NA <- check_bint_bobslen(file = "ex_issue74.csv")
  out_app_1 <- check_bint_bobslen(file = "ex_issue74.csv", bint = "n_intervals1", bobslen = "Session_length1")
  out_app_2 <- check_bint_bobslen(file = "ex_issue74.csv", bint = "n_intervals2", bobslen = "Session_length2")
  
  data <- read.csv(testthat::test_path("..", "testdata", "ex_issue74.csv"))
  out_pkg_1 <-
    batch_calc_ES(dat = data,
                  grouping = c(Case_pseudonym),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  baseline_phase = "A",
                  intervention_phase = "B",
                  ES = c("LOR", "LRRd", "LRRi"),
                  improvement = "increase",
                  pct_change = FALSE,
                  scale = "Percentage",
                  intervals = n_intervals1,
                  observation_length = Session_length1,
                  D_const = NA,
                  std_dev = "baseline",
                  confidence = 0.95,
                  Kendall = FALSE,
                  pretest_trend = FALSE,
                  format = "long"
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 4L)))%>%
    as.data.frame()
  
  out_pkg_2 <-
    batch_calc_ES(dat = data,
                  grouping = c(Case_pseudonym),
                  condition = Condition,
                  outcome = Outcome,
                  session_number = Session_number,
                  baseline_phase = "A",
                  intervention_phase = "B",
                  ES = c("LOR", "LRRd", "LRRi"),
                  improvement = "increase",
                  pct_change = FALSE,
                  scale = "Percentage",
                  intervals = n_intervals2,
                  observation_length = Session_length2,
                  D_const = NA,
                  std_dev = "baseline",
                  confidence = 0.95,
                  Kendall = FALSE,
                  pretest_trend = FALSE,
                  format = "long"
    ) %>%
    mutate(across(Est:CI_upper, ~ round(., 4L)))%>%
    as.data.frame()
  # the results did not match,it might be calculation error?
  
  expect_error(expect_equal(out_app_NA, out_app_1, check.attributes = FALSE))
  expect_error(expect_equal(out_app_NA, out_app_2, check.attributes = FALSE))
  expect_equal(out_app_1, out_pkg_1, check.attributes = FALSE)
  expect_equal(out_app_2, out_pkg_2, check.attributes = FALSE)
  
})
#block 21
check_PoGO <- function(file) {
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  data_path <- testthat::test_path("..", "testdata", file)
  
  app$set_inputs(SCD_es_calculator = "Multiple-Series Calculator")
  
  if (str_detect(file, "csv")) {
    app$set_inputs(dat_type = "dat")
    app$upload_file(dat = data_path)
    app$wait_for_idle()
  } else if (str_detect(file, "xlsx")) {
    app$set_inputs(dat_type = "xlsx")
    app$upload_file(xlsx = data_path)
    app$wait_for_idle()
  }
  
  app$set_inputs(BatchEntryTabs = "Variables")
  app$wait_for_idle()
  app$set_inputs(calcPhasePair = TRUE)
  app$wait_for_idle()
  app$set_inputs(b_clusters = c("Study_ID", "Study_Case_ID"))
  app$set_inputs(b_aggregate = "phase_pair_calculated")
  app$set_inputs(b_phase = "Condition")
  app$set_inputs(session_number = "Session_number")
  app$set_inputs(b_out = "Outcome")
  app$wait_for_idle()
  app$set_inputs(BatchEntryTabs = "Plot")
  app$set_inputs(BatchEntryTabs = "Estimate")
  app$set_inputs(bESpar = c("PoGO"))
  app$wait_for_idle()
  app$set_inputs(bgoalLevel = c("goals"))
  app$set_inputs(bgoalvar = c("Goal_Level"))
  app$wait_for_idle()
  app$set_inputs(bdigits = 4)
  app$wait_for_idle()
  app$set_inputs(batchest = "click")
  
  app$wait_for_idle()
  
  output_app <- app$get_value(output = "batchTable")
  
  output_app_table <-
    read_html(output_app) %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    mutate(across(Est:CI_upper, ~ ifelse(. == "-", NA, .))) %>%
    mutate(across(Est:CI_upper, as.numeric))
  
  return(output_app_table)
  
}
#block 22
test_that("The multiple series calculator works for PoGO.", {
  
  skip_on_cran()
  
  out_app_csv <- 
    check_PoGO(file = "CSESdata.csv") %>%
    mutate(Study_ID = as.character(Study_ID))
  
  out_app_xlsx <- 
    check_PoGO(file = "CSESdata.xlsx") %>%
    mutate(Study_ID = as.character(Study_ID))
  
  data <- read.csv(
    testthat::test_path("..", "testdata", "CSESdata.csv")
  ) %>%
    janitor::clean_names(case = "parsed")
  
  xlsx_data <- readxl::read_excel(
    testthat::test_path("..", "testdata", "CSESdata.xlsx")
  ) %>%
    janitor::clean_names(case = "parsed")
  
  out_pkg_csv <-
    data %>% 
    mutate(Study_ID = as.character(Study_ID)) %>% 
    group_by(Study_ID, Study_Case_ID) %>% 
    mutate(
      phase_pair_calculated = calc_phase_pairs(Condition, session = Session_number)
    ) %>% 
    ungroup() %>%
    batch_calc_ES(
      grouping = c(Study_ID, Study_Case_ID),
      condition = Condition,
      outcome = Outcome,
      aggregate = phase_pair_calculated,
      session_number = Session_number,
      baseline_phase = "A",
      intervention_phase = "B",
      ES = c("PoGO"),
      improvement = "increase",
      pct_change = FALSE,
      scale = Procedure,
      goal = Goal_Level,
      format = "long"
    ) %>%
    mutate(
      across(Est:CI_upper, ~ round(., 4L))
    ) %>%
    as.data.frame()
  
  out_pkg_xlsx <-
    xlsx_data %>% 
    mutate(Study_ID = as.character(Study_ID)) %>% 
    group_by(Study_ID, Study_Case_ID) %>% 
    mutate(
      phase_pair_calculated = calc_phase_pairs(Condition, session = Session_number)
    ) %>% 
    ungroup() %>%
    batch_calc_ES(
      grouping = c(Study_ID, Study_Case_ID),
      condition = Condition,
      outcome = Outcome,
      aggregate = phase_pair_calculated,
      session_number = Session_number,
      baseline_phase = "A",
      intervention_phase = "B",
      ES = c("PoGO"),
      improvement = "increase",
      pct_change = FALSE,
      scale = Procedure,
      goal = Goal_Level,
      format = "long"
    ) %>%
    mutate(
      across(Est:CI_upper, ~ round(., 4L))
    ) %>%
    as.data.frame()
  
  expect_equal(out_pkg_csv, out_pkg_xlsx, check.attributes = FALSE)
  expect_equal(out_app_csv, out_app_xlsx, check.attributes = FALSE)
  expect_equal(out_app_csv, out_pkg_csv, check.attributes = FALSE)
  expect_equal(out_app_xlsx, out_pkg_xlsx, check.attributes = FALSE)
  
  
})
test_that("The warning message is shown when an outcome measurement type is not acceptable.", {
  
  skip_on_cran()
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  data_path <- testthat::test_path("..", "testdata", "warnings_issue.csv")
  app$set_inputs(SCD_es_calculator = "Multiple-Series Calculator")
  app$set_inputs(dat_type = "dat")
  app$upload_file(dat = data_path)
  
  
  app$set_inputs(BatchEntryTabs = "Variables")
  app$set_inputs(calcPhasePair = TRUE)
  app$set_inputs(b_clusters = c("Study_ID", "Study_Case_ID"))
  app$set_inputs(b_aggregate = "phase_pair_calculated")
  app$set_inputs(b_phase = "Condition")
  app$set_inputs(session_number = "Session_number")
  app$set_inputs(b_out = "Outcome")
  app$set_inputs(BatchEntryTabs = "Plot")
  app$set_inputs(BatchEntryTabs = "Estimate")
  app$set_inputs(bESpar = c("LRRi"))
  app$set_inputs(boutScale = "series")
  app$set_inputs(bscalevar = "Procedure")
  
  app$wait_for_idle()
  
  warning_html <- app$get_value(output = "outcomeScale")
  warning <- sub(".*>The", "The", warning_html)
  warning <- sub("other.*", "other.", warning)
  warning <- warning[1]
  expect_equal(warning,
               "The scale variable contains non-acceptable types: blah. The acceptable scale types are: count, rate, proportion, percentage, or other.")
  
  
})

test_that("The warning message is shown when an improvement direction is not acceptable.", {
  
  skip_on_cran()
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  data_path <- testthat::test_path("..", "testdata", "warnings_issue.csv")
  
  app$set_inputs(SCD_es_calculator = "Multiple-Series Calculator")
  app$set_inputs(dat_type = "dat")
  app$upload_file(dat = data_path)
  app$set_inputs(BatchEntryTabs = "Variables")
  app$set_inputs(calcPhasePair = TRUE)
  app$set_inputs(b_clusters = c("Study_ID", "Study_Case_ID"))
  app$set_inputs(b_aggregate = "phase_pair_calculated")
  app$set_inputs(b_phase = "Condition")
  app$set_inputs(session_number = "Session_number")
  app$set_inputs(b_out = "Outcome")
  app$set_inputs(bimprovement = "series")
  app$set_inputs(bseldir = "Direction")
  
  app$wait_for_idle()
  
  warning_html <- app$get_value(output = "improvementDir")
  warning <- sub(".*>The", "The", warning_html)
  warning <- sub("decrease.*", "decrease.", warning)
  # The warning output is returned  a “list()” with two elements, so its length is 2.
  warning <- warning[1]
  expect_equal(warning,
               "The improvement direction variable contains non-acceptable types: incrase, direction. The acceptable improvement directions are: increase or decrease.")
  
})


