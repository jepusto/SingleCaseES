#' @title SCD effect size calculator
#'   
#' @description An interactive tool for calculating effect size indices for
#' single-case designs.
#' 
#' @param browser logical value indicating whether to launch the app in the
#'   system's default web-browser. Defaults to \code{TRUE}.
#'   
#' @export
#' @import stats


SCD_effect_sizes <- function(browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The simulator requires the shiny package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The simulator requires the ggplot2 package. Please install it.", call. = FALSE)
  }
  
  appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")
  if (appDir == "") {
    stop("Could not find the application directory. Try re-installing SingleCaseES.", call. = FALSE)
  }
  
  if (!browser) browser <- getOption("shiny.launch.browser", interactive())
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = browser)
}

#' Data from a study by Schmidt (2007). The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Behavior_type}. Outcome measure description (disruptive behavior or on task behavior).
#'   \item \code{Procedure}. The type of observation method used to measure the outcome.  
#'   \item \code{Session_length}. Time (in minutes) of the observation sessions
#'   \item \code{Interval_length}. If an interval method was used, the time in seconds of the intervals, \code{NA} otherwise.
#'   \item \code{Case_Psuedonym}. Case psuedonym provided by the authors.
#'   \item \code{Session_number}. Within-series session-number
#'   \item \code{Phase}. Variable uniquely describe the phase. For instance A1 is the first baseline phase, B2 is the second treatment phase.
#'   \item \code{Condition}. Describes whether the outcome is in the baseline (A) or treatment (B) phase.
#'   \item \code{Outcome}. Value for the outcome.
#'   \item \code{Phase_num}. A number describing which phase pair the outcome is a part of.
#'   \item \code{direction}. Describes the direction of improvement for an outcome type.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 172 rows and 12 variables
#' @name Schmidt2007
#' @source Schmidt, A. C. (2007). The effects of a group contingency on group and individual behavior in an urban 
#' first-grade classroom. University of Kansas. Retrieved from http://gradworks.umi.com/14/43/1443719.html
NULL

#' Data from a study by McKissick et al. (2010). The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Case_Psuedonym}. Case psuedonym provided by the authors.
#'   \item \code{Session_number}. Within-series session-number
#'   \item \code{Condition}. Describes whether the outcome is in the baseline (A) or treatment (B) phase.
#'   \item \code{Outcome}. Value for the outcome.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 35 rows and 4 variables
#' @name McKissick
#' @source McKissick, C., Hawkins, R. O., Lentz, F. E., Hailley, J., & McGuire, S. (2010). 
#' Randomizing multiple contingency components to decrease disruptive behaviors and increase 
#' student engagement in an urban second-grade classroom. \emph{Psychology in the Schools, 47}(9), 
#' 944–959. https://doi.org/10.1002/pits.20516
NULL
