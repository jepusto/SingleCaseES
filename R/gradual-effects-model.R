
#------------------------------------------------------------------------
# D_linear_predictor_eq() generates the value of the derivative for 
# the linear predictor at each time point with respect to omega for 
# the equilibrium treatment effect.
# Arguments:
# omega: The value of the delay parameter
# Trt:   A vector of treatment assignments for a set of n observations sessions.
# 0 = untreated
# 1 = treateed
# n:     Total length of the series.
#------------------------------------------------------------------------

D_linear_predictor_eq <- function(omega, Trt, n = length(Trt)) {
  
  #pre-fill vector
  deriv_vec <- vector(mode = "double", length = n) 
  
  #Fill the first element if the series starts on treatment
  if (Trt[1] == 1) deriv_vec[1] <- -1
  
  #Fill the rest of the vector iteratively
  deriv_vec[2:n] <- sapply(X = 2:n, function(x, omega, Trt) {
    -sum(omega^((x-1):0) * Trt[1:x]) + (1-omega) * sum(((x-2):0) * omega^((x-3):-1) * Trt[2:x])
  }, omega = omega, Trt = Trt)
  
  return(deriv_vec)
}


#--------------------------------------------------------------------
# D_linear_predictor_obs() generates the value of the derivative for the 
# linear predictor at each time point with respect to omega for the observed 
# treatment effect. Makes use of D_linear_predictor_eq()  Arguments: 
# omega: The value of the delay parameter
# Trt:   A vector of treatment assignments for a set of n observations sessions.
#        0 = untreated
#        1 = treateed
# m:     The number of observations sessions the observed treatment effect should
#        be pegged to. e.g. for an intervention 5 sessions long, m = 5.
# n:     Total length of the series.
#--------------------------------------------------------------------

D_linear_predictor_obs <- function(omega, Trt, m, n = length(Trt)) {
  
  #calculate the equilibrium treatment effect derivative as the numerator
  #of the observed treatmente effect derivative
  f_p <- D_linear_predictor_eq(omega, Trt, n)
  
  #fill an empty vector
  deriv_vec <- rep(0, length = n)
  
  #Fill the rest of the vector iteratively
  deriv_vec <- sapply(X = 1:n, function(x, omega, Trt, m) {
    f_p[x] / (1 - omega^m) + ((m * omega ^(m-1))/((1 - omega ^m)^2)) * ((1-omega) * sum(omega ^((x-1):0) * Trt[1:x]))
  }, omega = omega, Trt = Trt, m = m)
  
  return(deriv_vec)
}

#' A function to estimate the gradual effects model for an SCD
#' 
#' @param Trt A vector of treatment assignments where 0 = untreated and 1 =
#'   treated.
#' @param outcome A vector of outcomes.
#' @param m The number of treatment occasions to estimate a treatment effect
#'   for.
#' @param fam A description of the error distribution and link function to be
#'   used in the model. This can be supplied in any way that `glm` will accept.
#'   (See \code{\link[stats]{family}} for details of family functions.)
#'   
#' @return `gem_scd` returns an object of class \code{glm}. In addition to the
#'   normal contents of a \code{glm} object, it also contains an estimate of
#'   `omega` and a variance covariance matrix called `varcov`.
#'   
#' @export

gem_scd <- function(Trt, outcome, m, fam){
  
  #calculate the number of observations
  n <- length(outcome)
  
  #estimate the value of omega using profile likelihood
  opts<- optimize(greff_glm_dev, interval = c(0,1), outcome = outcome, Trt = Trt, fam = fam, m = m)
  omega <- opts$minimum
  
  #fill the empty vector of linear covariates
  linear_covariate <- rep(0, times = n)
  
  #calculate the linear covariate for the estimated value of omega
  for(i in 1:n){
    linear_covariate[i] <- (1 - omega) * sum(omega^((i-1):0) * Trt[1:i])/(1-omega^m)
  }
  
  #fit the model using the estimated value of omega to estimate
  #baseline and treatment effect model parameters
  model <- glm(outcome ~ linear_covariate, family = fam, x = TRUE)
  
  #caclulate the derivative of the linear predcictor for use in variance estimation
  d_f <- D_linear_predictor_obs(omega = omega, Trt = Trt, n = n, m = m)
  
  #fill the modified design matrix
  X <- matrix(nrow = n, ncol = 3)
  X[,1] <- 1
  X[,2] <- linear_covariate
  X[,3] <- model$coefficients[2] * d_f
  
  #extract the weights from the final iteration of the GLM parameter estimation
  w <- model$weights
  
  #estimate a new covariance matrix
  varcov <- summary(model)$dispersion * chol2inv(chol(crossprod(X, (w * X))))
  
  #attach the estimated omega and covariance matrix to the model
  model$omega <- omega
  model$varCov <- varcov
  
  return(model)
}

#--------------------------------------------------------------------
# greff_norm_ll() is a function specifically for use in optimization. The
# function calcultes the log-likelihood of the model for a given value
# of omega.
# Arguments:
# omega: The value of the delay parameter
# outcome: A vector of length n of the outcome of interest
# Trt:   A vector of treatment assignments for a set of n observations sessions.
#        0 = untreated
#        1 = treated
# fam:   Specifying error distribution and link.
# m:     The number of observations sessions the observed treatment effect should
#        be pegged to. e.g. for an intervention 5 sessions long, m = 5.
#--------------------------------------------------------------------

greff_glm_dev <- function(omega, Trt, outcome, m, fam){
  
  n <- length(outcome)
  linear_covariate <- rep(0, times = n)
  
  for(i in 1:n){
    linear_covariate[i] <- (1 - omega) * sum(omega^((i-1):0) * Trt[1:i])/(1-omega^m)
  }
  
  model <- glm(outcome ~ linear_covariate, family = fam)
  
  return(model$deviance)
}

#' @title Gradual Effects Model SCD Calculator
#'   
#' @description Open an interactive tool for calculating the gradual effects model
#' for SCDs.
#' 
#' @param browser logical value indicating whether to launch the app in the
#'   system's default web-browser. Defaults to \code{TRUE}.
#'   
#' @export
#' @import stats
#' 
#' @examples 
#' \dontrun{
#' shine_gem_scd()
#' }
#' 


shine_gem_scd <- function(browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The GEM calculator requires the shiny package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The GEM calculator requires the ggplot2 package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("The GEM calculator requires the tidyr package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("purrrlyr", quietly = TRUE)) {
    stop("The GEM calculator requires the purrrlyr package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The GEM calculator requires the dplyr package. Please install it.", call. = FALSE)
  }
  
  appDir <- system.file("shiny-examples", "gem-scd", package = "SingleCaseES")
  if (appDir == "") {
    stop("Could not find the application directory. Try re-installing SingleCaseES.", call. = FALSE)
  }
  
  if (!browser) browser <- getOption("shiny.launch.browser", interactive())
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = browser)
}
