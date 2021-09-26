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
  
  req_pkgs <- c("shiny", "ggplot2", "markdown", "tidyr", "dplyr", "readxl", 
                "janitor", "rlang", "glue", "rclipboard")
  missing_pkgs <- unlist(lapply(req_pkgs, check_for_package))
  
  if (length(missing_pkgs) > 1) {
    missing_pkgs <- paste(missing_pkgs, collapse = ", ")
    stop(paste0("The SingleCaseES app requires the following packages: ", missing_pkgs,". Please install them."), call. = FALSE)
  } else if (length(missing_pkgs) == 1) {
    stop(paste("The SingleCaseES app requires the", missing_pkgs,"package. Please install it."), call. = FALSE)
  }
  
  appDir <- system.file("shiny-examples", "SCD-effect-sizes", package = "SingleCaseES")
  if (appDir == "") {
    stop("Could not find the application directory. Try re-installing SingleCaseES.", call. = FALSE)
  }
  
  if (!browser) browser <- getOption("shiny.launch.browser", interactive())
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = browser)
}

check_for_package <- function(pkg) {
  req <- requireNamespace(pkg, quietly = TRUE)
  if (!req) pkg else NULL
}
