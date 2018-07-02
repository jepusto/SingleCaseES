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

#' @title Schmidt (2007) 
#' 
#' @description Data from a study by Schmidt (2007). The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Behavior_type}. Outcome measure description (disruptive behavior or on task behavior).
#'   \item \code{Procedure}. The observation recording procedure used to measure the outcome.  
#'   \item \code{Metric}. The metric in which the outcome measurement is expressed ("count" for natural counts; "percentage" for percentage of intervals)
#'   \item \code{Session_length}. Length (in minutes) of the observation sessions
#'   \item \code{Interval_length}. If an interval method was used, the length of the intervals (in seconds); \code{NA} otherwise.
#'   \item \code{Case_Pseudonym}. Case Pseudonym provided by the authors.
#'   \item \code{Session_number}. Within-series session-number.
#'   \item \code{Phase}. Label for each unique phase (e.g., A1 is the first baseline phase, B2 is the second treatment phase).
#'   \item \code{Condition}. Label indicating whether the outcome is in the baseline (A) or treatment (B) phase.
#'   \item \code{Outcome}. Outcome measurement.
#'   \item \code{Phase_num}. Indicator for each pair of baseline and treatment phases.
#'   \item \code{direction}. Direction of therapeutic improvement for the outcome.
#'   \item \code{n_Intervals}. If an interval method was used, the total number of intervals; \code{NA} otherwise.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 172 rows and 13 variables
#' @name Schmidt2007
#' @source Schmidt, A. C. (2007). The effects of a group contingency on group
#'   and individual behavior in an urban first-grade classroom.  Masters Thesis,
#'   University of Kansas, Department of Applied Behavioral Sciences. ProQuest
#'   Dissertations & Theses Global, thesis number 1443719.
NULL

#' @title McKissick et al. (2010)
#' 
#' @description  Disruptive behavior data from a study by McKissick et al. (2010). All data were collected
#' via event counting. The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Case_Pseudonym}. Case Pseudonym provided by the authors.
#'   \item \code{Session_number}. Within-series session-number
#'   \item \code{Condition}. Describes whether the outcome is in the baseline (A) or treatment (B) phase.
#'   \item \code{Outcome}. Value for the outcome.
#'   \item \code{Session_length}. Length of the observation session.
#'   \item \code{Procedure}. The metric in which the outcome measurement is expressed, all "count".
#'   \item \code{Session_length}. The length of the observation session.
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

#' @title Shogren et al. (2004)
#' 
#' @description Data from a systematic review by Shogren et al. (2004) 
#' on the effects of choice-making interventions. 
#' These data were compiled and re-analyzed in Pustejovsky (2015).
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Study}. An ID for each study in the systematic review.
#'   \item \code{Case}. Case Pseudonym provided by the authors.
#'   \item \code{Measure}. Type of behavior observed as the outcome measure
#'   \item \code{Phase}. Phase indicator, baseline phase is "No Choice" and treatment phase is "Choice."
#'   \item \code{Percentage}. For those outcomes measured as percentage, outcomes value. \code{NA} for count outcomes.
#'   \item \code{Observed}. For those outcomes measured as count, outcome value. \code{NA} for percentage outcomes.
#'   \item \code{Possible}. For counts out of a maximum, lists the maximum value. \item \code{Recording_procedure}  Recording procedure. CDR = "Continuous Duration Recording", EC = "Event Counting", "MTS = "Momentary Time Sampling", and PIR = "Partial Interval Recording."
#'   \item \code{Session_length}. Length of the observation session in minutes.
#'   \item \code{interval_length}. Length of the observation intervals for data observed using MTS or PIR.
#'   \item \code{outcome}. Value for the outcome for all outcome types.
#'   \item \code{direction}. Direction of therapeutic improvement for the outcome.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 634 rows and 15 variables
#' @name Shogren
#' @source Shogren, K. A., Faggella-Luby, M. N., Bae, S. J., & Wehmeyer, M. L. (2004). 
#' The effect of choice-making as an intervention for problem behavior. \emph{Journal 
#' of Positive Behavior Interventions, 6}(4), 228–237. 
#'
#' @references Pustejovsky, J.E. (2015). Measurement-comparable effect 
#' sizes for single-case studies of free-operant behavior. 
#' \emph{Psychological Methods, 20}(3), 342–359.
#' 

NULL

#' @title Thorne and Kamps (2008)
#' 
#' @description Data from an ABAB design conducted by Thorne and Kamps (2008). 
#' These data were used as an example in Swan and Pustejovsky (2017). Academic engagement 
#' was collected via continuous recording (marked as "other") and inappropriate 
#' verbalizations were collected via event counting (marked as "count").
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Measure}. Outcome measure description (academic engagement or inappropriate verbalizations).
#'   \item \code{Case}. Participant identifier.  
#'   \item \code{Session_number}. Measurement occasion.
#'   \item \code{Outcome}. Outcome scores
#'   \item \code{Trt}. Treatment indicators.
#'   \item \code{Session_length}. Length of the observation session.
#'   \item \code{Measure}. The metric in which the outcome measurement is expressed (count or other).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 776 rows and 5 variables
#' @name Thorne
#' @source Thorne, S., & Kamps, D. (2008). The effects of a group contingency intervention on academic engagement and problem 
#' behavior of at-risk students. \emph{Behavior Analysis in Practice, 1}(2), 12-18.
#' 
#' @references Swan, D. M., & Pustejovsky, J. E. (2017). 
#' A gradual effects model for single-case designs. http://doi.org/10.17605/OSF.IO/GAXRV
NULL

#' @title Schmidt and Stichter (2012)
#' 
#' @description Data from an ABAB design conducted by Schmidt and 
#' Stichter (2012). All data were collected via continuous recording.
#'  The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Case}. Participant identifier.
#'   \item \code{Behavior}. Behavior type (Conversation, Initiations, or Responses).
#'   \item \code{Trt} Treatment indicators.
#'   \item \code{Outcome}. Outcome scores.
#'   \item \code{Session_num}. Measurement occasion.
#'   \item \code{Session_length} Length of the observation session.
#'   \item \code{Procedure}  The metric in which the outcome measurement is expressed, all "other".
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 180 rows and 5 variables
#' @name Schmidt2012
#' @source Schmidt, C., & Stichter, J. P. (2012). The use of peer-mediated interventions to 
#' promote the generalization of social competence for adolescents with high-functioning autism 
#' and Asperger's syndrome. \emph{Exceptionality}, 20(2), 94-113. doi:10.1080/09362835.2012
NULL