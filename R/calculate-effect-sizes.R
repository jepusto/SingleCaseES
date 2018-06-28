
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
#'   index or indices to calculate. Defaults to calculating the LRRd, LRRi, SMD,
#'   and Tau indices.
#' @param improvement character string indicating direction of improvement.
#'   Default is "increase".
#' @param ... further arguments used for calculating some of the effect size
#'   indices.
#' @param confidence confidence level for the reported interval estimate. Set to
#'   \code{NULL} to omit confidence interval calculations.
#' @param format charaacter string specifying whether to organize the results in
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
                    baseline_phase = unique(condition)[1],
                    ES = c("LRRd","LRRi","SMD","Tau"), 
                    improvement = "increase", 
                    ..., 
                    confidence = .95,
                    format = "long") {
  
  if (missing(A_data) | missing(B_data)) {
    if (missing(outcome) | missing(condition)) stop("You must provide the data using the arguments 'A_data' and 'B_data' or 'condition' and 'outcome'.")
    
    conditions <- unique(condition)
    if (length(conditions) != 2) stop("The 'condition' variable must have exactly two unique values.")
    dat <- split(outcome, condition)
    treatment_phase <- setdiff(conditions, baseline_phase)
    A_data <- dat[[baseline_phase]]
    B_data <- dat[[treatment_phase]]
  } 
  
  if (length(improvement) > 1L) improvement <- names(sort(table(improvement), decreasing = TRUE)[1])
  
  A_data <- A_data[!is.na(A_data)]
  B_data <- B_data[!is.na(B_data)]
  
  ES_to_calc <- paste0("calc_", ES)
  
  res <- purrr::invoke_map_dfr(
    ES_to_calc,  
    A_data = A_data, B_data = B_data,
    improvement = improvement, confidence = confidence, ...
  )
  
  if (format != "long") {
    
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
      purrr::cross2(val_names, ES) %>%
      purrr::map(.f = function(x) paste(rev(x), collapse = "_")) %>%
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
#' @param dat A dataframe containing SCD series for which effect sizes
#'   will be calculated.
#' @param condition A string containing the variable name that identifies
#'  the treatment condition for each observation in the series.
#' @param outcome A string containing the variable name of the outcome data.
#' @param session_number A string containing the name of a variable used to
#'   order outcomes within each series.
#' @param grouping_vars A vector of strings of the names of the varibles that
#'   uniquely identify each series (e.g. pseudonym, outcome type, study)
#' @param baseline_phase character string specifying which value of
#'   \code{condition} corresponds to the baseline phase. Defaults to first
#'   observed value of \code{condition}.
#' @param ES character string or character vector specifying which effect size
#'   index or indices to calculate. Defaults to calculating the LRRd, LRRi, SMD,
#'   and Tau indices.
#' @param improvement character string either indicating the direction of
#'   uniform improvement ("increase" or "decrease") or the variable name of 
#'   a variable identifying the direction of improvement for each series. 
#'   Default is "increase".
#' @param scale character string indicating the common scale of the outcome
#'   variable across all of the series in the data set or the name of a variable 
#'   within the dataset that identifies the outcome scale within each series. 
#'   Possible values for the scle are \code{"percentage"} for a percentage with 
#'   range 0-100, \code{"proportion"} for a proportion with range 0-1, 
#'   \code{"count"} for a frequency count (0 or positive integers), 
#'   \code{"rate"} for a standardized rate per minute. If a vector, the most 
#'   frequent unique value will be used and missing values will be ignored. 
#'   Defaults to \code{NA}.
#' @param intervals for interval recording procedures. Either the total number of
#'   intervals per observation session common to all series in the dataset, or the
#'   name of a variable within the dataset that identifies the number of intervals
#'   for each observation. If a variable name, the mean number of intervals within
#'   each series will be used. Missing values will be ignored. Defaults to
#'   \code{NA}.
#' @param observation_length Used for the log-response ratio. Either the common
#'   observation session length (in minutes) across all series in the dataset or
#'   a variable name containing the observation session length for each 
#'   observation. If a variable name, the mean observation session length within 
#'   each series will be used. Missing values will be ignored. Defaults to 
#'   \code{NA}.
#' @param ... further arguments used for calculating some of the effect size
#'   indices.
#' @param confidence confidence level for the reported interval estimate. Set to
#'   \code{NULL} to omit confidence interval calculations.
#' @param format charaacter string specifying whether to organize the results in
#'   \code{"long"} format or \code{"wide"} format. Defaults to \code{"long"}.
#'
#' @details Calculates one or more effect size indices for each series in a dataset
#'
#' @export
#'
#' @return A data.frame containing the estimate, standard error, and/or
#'   confidence interval for each specified effect size.

batch_calc_ES <- function(dat, 
                          condition, outcome,
                          session_number,
                          grouping_vars,
                          baseline_phase = dat[[condition]][1],
                          ES = c("LRRd","LRRi","SMD","Tau"), 
                          improvement = "increase",
                          scale = NA,
                          intervals = NA,
                          observation_length = NA,
                          ...,
                          confidence = .95,
                          format = "long"
                          ) {

  if(!(condition %in% names(dat))) stop("The condition variable name is not in the provided dataset.")

  if(!(outcome %in% names(dat))) stop("The outcome variable name is not in the provided dataset.")
  
  if(!(baseline_phase %in% unique(dat[[condition]])) && !(baseline_phase %in% names(dat))) stop ("The provided baseline phase is not one of the values in the provided condition variable.")
    
  if(!(session_number %in% names(dat))) stop("The session number variable name is not in the provided dataset.")

  if(!all(grouping_vars %in% names(dat))) stop(paste0("The following grouping variables are not in the provided dataset:
                                                      ", grouping_vars[!(grouping_vars %in% names(dat))]))

  if(!(improvement %in% c("increase", "decrease")) && !(improvement %in% names(dat))) stop("The improvement variable name is not in the provided dataset.")
  
  if(!is.na(scale) && !(scale %in% c("count", "rate", "proportion", "percentage")) && !(scale %in% names(dat))) stop("The scale variable name is not in the provided dataset.")
  
  if(!is.na(intervals) && typeof(intervals) == "character" && !(intervals %in% names(dat))) stop("The intervals variable name is not in the provided dataset.")
  
  if(!is.na(observation_length) && typeof(observation_length) == "character" && !(observation_length %in% names(dat))) stop("Observation session length variable name not in dataset.")
  
  if(baseline_phase %in% unique(dat[[condition]])){
    dat$baseline_phase <- baseline_phase
    baseline_phase <- "baseline_phase"
  }
  
  if(improvement %in% c("increase", "decrease")){
    dat$improvement <- improvement
    improvement <- "improvement"
  }
  
  if(scale %in% c("count", "rate", "proportion", "percentage")){
    dat$scale <- scale
    scale <- "scale"
  }
  
  if(typeof(intervals) != "character"){
    dat$intervals <- intervals
    intervals <- "intervals"
  }
  
  if(typeof(observation_length) != "character"){
    dat$observation_length <- observation_length
    observation_length <- "observation_length"
  }
  
  
  
    ES <- dat %>%  
      dplyr::group_by(!!!rlang::syms(grouping_vars)) %>%
      dplyr::arrange(!!rlang::sym(session_number)) %>%
      dplyr::do(calc_ES(condition = .data[[condition]], 
                        outcome = .data[[outcome]], 
                        baseline_phase = .data[[baseline_phase]][1],
                        ES = ES, 
                        improvement = .data[[improvement]][1], 
                        scale =  .data[[scale]],
                        intervals = .data[[intervals]],
                        observation_length = .data[[observation_length]],
                        confidence = confidence, 
                        format = format,
                        ...)) %>%
      dplyr::ungroup()

  ES
}

