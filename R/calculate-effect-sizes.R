
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
#'   index or indices to calculate. Defaults to calculating all available
#'   indices.
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

calc_ES <- function(A_data, B_data, 
                    condition, outcome, 
                    baseline_phase = unique(condition)[1],
                    ES = c("LRRd","LRRi","SMD","NAP","Tau","Tau_U", "PND","IRD","PAND","PEM"), 
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
  
  A_data <- A_data[!is.na(A_data)]
  B_data <- B_data[!is.na(B_data)]
  
  ES_to_calc <- paste0("calc_", ES)
  
  res <- purrr::map_df(ES_to_calc, do.call, 
                       args = list(A_data = A_data, B_data = B_data,
                                   improvement = improvement, 
                                   confidence = confidence, ...))
  
  if (format != "long") {
  
  }
  
  return(res)
}
