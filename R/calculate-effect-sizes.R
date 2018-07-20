
#' @title Calculate effect sizes
#'
#' @description Calculates one or more effect size estimates, along with
#'   associated standard errors and confidence intervals, if available, for a
#'   single-case data series.
#'
#' @param A_data vector of numeric data for A phase. Missing values are dropped.
#' @param B_data vector of numeric data for B phase. Missing values are dropped.
#' @param condition vector identifying the treatment condition for each
#'   observation in the series.
#' @param outcome vector of outcome data for the entire series.
#' @param baseline_phase character string specifying which value of
#'   \code{condition} corresponds to the baseline phase. Defaults to first
#'   observed value of \code{condition}.
#' @param ES character string or character vector specifying which effect size
#'   index or indices to calculate. Available effect sizes are \code{"LRRd"},
#'   \code{"LRRi"}, \code{"LOR"}, \code{"SMD"}, \code{"NAP"}, \code{"IRD"},
#'   \code{"PND"}, \code{"PEM"}, \code{"PAND"}, \code{"Tau"}, and
#'   \code{"Tau-U"}. Set to \code{"all"} for all available effect sizes. Set to
#'   \code{"parametric"} for all parametric effect sizes. Set to \code{"NOM"}
#'   for all non-overlap measures. Defaults to calculating the LRRd, LRRi, SMD,
#'   and Tau indices.
#' @param improvement character string indicating direction of improvement.
#'   Default is "increase".
#' @param ... further arguments used for calculating some of the effect size
#'   indices.
#' @param confidence confidence level for the reported interval estimate. Set to
#'   \code{NULL} to omit confidence interval calculations.
#' @param format character string specifying whether to organize the results in
#'   \code{"long"} format or \code{"wide"} format. Defaults to \code{"long"}.
#'
#' @details Calculates one or more effect size indices
#'
#' @export
#'
#' @return A data.frame containing the estimate, standard error, and/or
#'   confidence interval for each specified effect size.
#'
#' @examples
#' # Using the A_data and B_data arguments
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' calc_ES(A_data = A, B_data = B)
#'
#' # Using the condition and outcome arguments
#' phase <- c(rep("A", length(A)), rep("B", length(B)))
#' outcome <- c(A, B)
#' calc_ES(condition = phase, outcome = outcome, baseline_phase = "A")
#'
#' # Example from Parker & Vannest (2009)
#' yA <- c(4, 3, 4, 3, 4, 7, 5, 2, 3, 2)
#' yB <- c(5, 9, 7, 9, 7, 5, 9, 11, 11, 10, 9)
#' calc_ES(yA, yB)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @importFrom rlang !!
#' @importFrom dplyr .data
#'   

calc_ES <- function(A_data, B_data, 
                    condition, outcome, 
                    baseline_phase = NULL,
                    ES = c("LRRd","LRRi","SMD","Tau"), 
                    improvement = "increase", 
                    ..., 
                    confidence = .95,
                    format = "long") {
  
  if (missing(A_data) | missing(B_data)) {
    if (missing(outcome) | missing(condition)) stop("You must provide the data using the arguments 'A_data' and 'B_data' or 'condition' and 'outcome'.")
    
    conditions <- as.character(unique(condition))
    if (length(conditions) != 2) stop("The 'condition' variable must have exactly two unique values.")

    if (is.null(baseline_phase)) baseline_phase <- conditions[1]
    if (!is.character(baseline_phase)) baseline_phase <- as.character(baseline_phase)
    treatment_phase <- setdiff(conditions, baseline_phase)
    
    dat <- split(outcome, condition)
    
    A_data <- dat[[baseline_phase]]
    B_data <- dat[[treatment_phase]]
  } 
  
  if (length(improvement) > 1L) improvement <- names(sort(table(improvement), decreasing = TRUE)[1])
  
  A_data <- A_data[!is.na(A_data)]
  B_data <- B_data[!is.na(B_data)]
  
  ES_names <- if (identical(ES, "all")) {
    c("LRRd","LRRi","LOR","SMD","NAP","IRD","PAND","PND","PEM","Tau","Tau_U")
  } else if (identical(ES,"NOM")) {
    c("NAP","IRD","PAND","PND","PEM","Tau","Tau_U")
  } else if (identical(ES, "parametric")) {
    c("LRRd","LRRi","LOR","SMD")
  } else {
    ES
  }

  # Allow for Tau-U variant names
  Tau_U_names <- c("Tau_U","Tau-U","TauU")
  if (any(Tau_U_names %in% ES)) ES_names <- union(setdiff(ES_names, Tau_U_names), "Tau_U") 
  ES_to_calc <- paste0("calc_", ES_names)
  
  res <- purrr::invoke_map_dfr(
    ES_to_calc,  
    A_data = A_data, B_data = B_data,
    improvement = improvement, confidence = confidence, ...
  )
  
  if (format != "long") {
    
    if(any(ES_names == "Tau_U")) ES_names[ES_names == "Tau_U"] <- "Tau-U"
    
    val_names <- setdiff(names(res), "ES")
    sym_val_names <- rlang::syms(val_names)
    val <- rlang::sym("val")
    
    res <- res %>%
      tidyr::gather("q", !!val, !!!sym_val_names)%>%
      tidyr::unite("q", from = c("ES","q")) %>%
      dplyr::filter(!is.na(val)) %>%
      tidyr::spread("q", !!val) 
    
    # re-order names
    long_names <- 
      purrr::cross2(val_names, ES_names) %>%
      purrr::map(.f = function(x) paste(rev(x), collapse = "_"))  %>%
      intersect(names(res)) %>%
      rlang::syms()
    
    res <- dplyr::select(res, !!!long_names)
  }
  
  return(res)
}


#' @title Calculate effect sizes from a dataset for multiple series
#'
#' @description Calculates one or more effect size estimates, along with
#'   associated standard errors and confidence intervals, if available, for a
#'   single-case data series.
#' @param dat A dataframe containing SCD series for which effect sizes will be
#'   calculated.
#' @param ... A selection of columns that uniquely identify each series (e.g. 
#'   pseudonym, outcome type, study). You can supply bare variable names.
#' @param condition A string containing the variable name that identifies the
#'   treatment condition for each observation in the series.
#' @param outcome A string containing the variable name of the outcome data.
#' @param session_number A string containing the name of a variable used to
#'   order outcomes within each series.
#' @param baseline_phase character string specifying which value of
#'   \code{condition} corresponds to the baseline phase. If \code{NULL} (the
#'   default), the first observed value of \code{condition} within the series
#'   will be used.
#'   @param ES character string or character vector specifying which effect size
#'   index or indices to calculate. Available effect sizes are \code{"LRRd"},
#'   \code{"LRRi"}, \code{"LOR"}, \code{"SMD"}, \code{"NAP"}, \code{"IRD"},
#'   \code{"PND"}, \code{"PEM"}, \code{"PAND"}, \code{"Tau"}, and
#'   \code{"Tau-U"}. Set to \code{"all"} for all available effect sizes. Set to
#'   \code{"parametric"} for all parametric effect sizes. Set to \code{"NOM"}
#'   for all non-overlap measures. Defaults to calculating the LRRd, LRRi, SMD,
#'   and Tau indices.
#' @param improvement character string either indicating the direction of
#'   uniform improvement ("increase" or "decrease") or the variable name of a
#'   variable identifying the direction of improvement for each series. Default
#'   is "increase".
#' @param scale character string indicating the common scale of the outcome
#'   variable across all of the series in the data set or the name of a variable
#'   within the dataset that identifies the outcome scale within each series.
#'   Possible values for the scale are \code{"percentage"} for a percentage with
#'   range 0-100, \code{"proportion"} for a proportion with range 0-1,
#'   \code{"count"} for a frequency count (0 or positive integers),
#'   \code{"rate"} for a standardized rate per minute. If a vector, the most
#'   frequent unique value will be used and missing values will be ignored.
#'   Defaults to \code{NA}.
#' @param intervals for interval recording procedures. Either the total number
#'   of intervals per observation session common to all series in the dataset,
#'   or the name of a variable within the dataset that identifies the number of
#'   intervals for each observation. If a variable name, the mean number of
#'   intervals within each series will be used. Missing values will be ignored.
#'   Defaults to \code{NA}.
#' @param observation_length Used for the log-response ratio. Either the common
#'   observation session length (in minutes) across all series in the dataset or
#'   a variable name containing the observation session length for each
#'   observation. If a variable name, the mean observation session length within
#'   each series will be used. Missing values will be ignored. Defaults to
#'   \code{NA}.
#' @param warn logical indicating whether warnings should be displayed. Default
#'   is \code{TRUE}.
#' @inheritParams calc_ES
#'
#' @details Calculates one or more effect size indices for each series in a
#'   dataset
#'
#' @export
#'
#' @return A tibble containing the estimate, standard error, and/or
#'   confidence interval for each specified effect size.

batch_calc_ES <- function(dat, 
                          ...,
                          condition, outcome,
                          session_number,
                          baseline_phase = NULL,
                          ES = c("LRRd","LRRi","SMD","Tau"), 
                          improvement = "increase",
                          scale = NA,
                          intervals = NA,
                          observation_length = NA,
                          bias_correct = TRUE,
                          D_const = NULL,
                          SE = "unbiased",
                          std_dev = "baseline",
                          confidence = .95,
                          format = "long",
                          warn = TRUE
                          ) {
  group_vars <- rlang::enquos(...)
  
  condition <- tryCatch(tidyselect::vars_pull(names(dat), !! rlang::enquo(condition)), error = function(e) stop("condition variable is not in the provided dataset."))
  
  outcome <- tryCatch(tidyselect::vars_pull(names(dat), !! rlang::enquo(outcome)), error = function(e) stop("outcome variable is not in the provided dataset."))
  
  session_number <- tryCatch(tidyselect::vars_pull(names(dat), !! rlang::enquo(outcome)), error = function(e) stop("session number variable is not in the provided dataset."))
  
  improvement <- tryCatch(tidyselect::vars_pull(c(names(dat), "increase", "decrease"), !! rlang::enquo(improvement)), error = function(e) stop("improvement must be a variable name, or string specifying 'increase' or 'decraease'."))

  scale <- tryCatch(tidyselect::vars_pull(c(names(dat), "percentage", "proportion", "count", "rate", "other"), !! rlang::enquo(scale)), error = function(e) stop("scale must be a variable name or one of the accepted scale types. See ?batch_calcES for more details."))
  
  # if (!is.na(intervals) && typeof(intervals) == "character" && !(intervals %in% names(dat))) stop("The intervals variable name is not in the provided dataset.")
  
  if (!is.na(observation_length) && typeof(observation_length) == "character" && !(observation_length %in% names(dat))) stop("Observation session length variable name not in dataset.")
  
  if (improvement %in% c("increase", "decrease")) {
    dat$improvement <- improvement
    improvement <- "improvement"
  }
  
  if (scale %in% c("count", "rate", "proportion", "percentage")) {
    dat$scale <- scale
    scale <- "scale"
  }
  
  if (tryCatch(typeof(intervals) %in% c("double", "integer") || is.na(intervals), error = function(e) FALSE)) {
    dat$intervals <- intervals
    intervals <- "intervals"
  }else{
    intervals <- tryCatch(tidyselect::vars_pull(names(dat), !! rlang::enquo(intervals)), error = function(e) stop("intervals variable is not in the provided dataset."))
    
  }
  
  if (typeof(observation_length) != "character") {
    dat$observation_length <- observation_length
    observation_length <- "observation_length"
  }
  
  if (warn & "LOR" %in% ES & !all(dat$scale %in% c("proportion","percentage"))) {
    warning("LOR can only be calculated for proportions or percentages. Will return NAs for other outcome scales.", call. = FALSE)
  }
  
  ES_names <- if (identical(ES, "all")) {
    c("LRRd","LRRi","LOR","SMD","NAP","IRD","PAND","PND","PEM","Tau","Tau_U")
  } else if (identical(ES,"NOM")) {
    c("NAP","IRD","PAND","PND","PEM","Tau","Tau_U")
  } else if (identical(ES, "parametric")) {
    c("LRRd","LRRi","LOR","SMD")
  } else {
    ES
  }
  
    res <- dat %>%  
      dplyr::group_by(!!!group_vars) %>%
      dplyr::arrange(!!rlang::sym(session_number)) %>%
      dplyr::do(calc_ES(condition = .data[[condition]], 
                        outcome = .data[[outcome]], 
                        baseline_phase = baseline_phase,
                        ES = ES_names, 
                        improvement = .data[[improvement]][1], 
                        scale =  .data[[scale]],
                        intervals = .data[[intervals]],
                        observation_length = .data[[observation_length]],
                        bias_correct = bias_correct,
                        D_const = D_const,
                        SE = SE,
                        std_dev = std_dev,
                        confidence = confidence, 
                        format = format,
                        warn = FALSE)) %>%
      dplyr::ungroup()

 res
}

