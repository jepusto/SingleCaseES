#' @title SCD effect size calculator
#' 
#' @description
#' An interactive tool for calculating effect size indices for single-case designs. 
#' 
#' @export
#' 

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
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = browser)
}