# Calculate mean, variance, and sample size by phase

summary_stats <- function(A_data, B_data, warn = TRUE) {
  
  n <- c(sum(!is.na(A_data)), sum(!is.na(B_data)))
  M <- c(mean(A_data, na.rm = TRUE), mean(B_data, na.rm = TRUE))
  V <- c(var(A_data, na.rm = TRUE), var(B_data, na.rm = TRUE))
  
  if (warn & any(n < 2)) {
    short_phases <- c("A","B")[n < 2]
    phases <- ifelse(all(n < 2), "phases", "phase")
    warning(paste(paste(short_phases, collapse = ", "), phases, "contains less than 2 observations."))
  }
  
  data.frame(n = n, M = M, V = V)
}


trunc_constant <- function(scale = NULL, observation_length = NULL, intervals = NULL) {
  
  #Allow for a vector of NAs from batch calculator
  if (all(is.na(observation_length))) observation_length <- NA
  if (all(is.na(intervals))) intervals <- NA
  
  if (length(scale) > 1L) scale <- names(sort(table(scale), decreasing = TRUE)[1])
  if (length(observation_length) > 1L) observation_length <- mean(observation_length, na.rm = TRUE)
  if (length(intervals) > 1L) intervals <- mean(intervals, na.rm = TRUE)
  
  A <- is.null(scale) 
  B <- (scale == "rate" & is.null(observation_length))
  C <- (scale %in% c("percentage","proportion") & is.null(intervals))
  if (A | B | C) return(Inf)

  if (is.null(observation_length)) observation_length <- NA
  if (is.null(intervals)) intervals <- NA

  D <- is.na(scale)
  E <- (scale == "rate" & is.na(observation_length))
  G <- (scale %in% c("percentage","proportion") & is.na(intervals))
  if (D | E | G) return(Inf)
  
  switch(scale,
         count = 1L,
         rate = observation_length,
         proportion = intervals,
         percentage = intervals / 100,
         other = Inf)
}

#' @name LOR
#' @title Log-odds ratio
#'
#' @description Calculates the log-odds ratio effect size index, with or without
#'   bias correction (Pustejovsky, 2015)
#'
#' @param scale character string indicating the scale of the outcome variable.
#'   Must be either \code{"percentage"} for percentages with range 0-100 or
#'   \code{"proportion"} for proportions with range 0-1. If a vector, the most
#'   frequent unique value will be used. \code{"percentage"} is assumed by
#'   default.
#' @param D_const constant used for calculating the truncated sample mean (see
#'   Pustejovsky, 2015). If a vector, the mean value will be used.
#' @inheritParams LRR
#'
#' @details The odds ratio parameter is the ratio of the odds of the outcome.
#'   The log-odds ratio is the natural logarithm of the odds ratio. This effect
#'   size is appropriate for outcomes measured on a percentage or proportion
#'   scale. Unlike the LRRd and LRRi, the LOR is symmetric in valence, so that
#'   the LOR for an positively-valenced outcome is equal to -1 times the LOR
#'   calculated after reversing the scale of the outcome so that it is
#'   negatively valenced.
#'
#'   Without bias correction, the log-odds ratio is estimated by substituting
#'   the sample mean level in each phase in place of the corresponding
#'   parameter. A delta-method bias correction to the estimator is used by
#'   default.
#'
#'   The standard error of LOR is calculated based on a delta-method
#'   approximation, allowing for the possibility of different degrees of
#'   dispersion in each phase. The confidence interval for LOR is based on a
#'   large-sample (z) approximation.
#'
#'   To account for the possibility of sample means of zero, a truncated mean is
#'   calculated following the method described in Pustejovsky (2015). Truncated
#'   sample variances are also calculated to ensure that standard errors will be
#'   strictly larger than zero. The truncation constant depends on the total
#'   number of intervals per session (or the total number of items for other
#'   percentage/proportion scales). The arguments \code{scale} and
#'   \code{intervals} must be specified in order to calculate an appropriate
#'   truncation constant. For outcomes measured using continuous recording
#'   procedures, set \code{intervals} equal to 60 times the length of the
#'   observation session in minutes.
#'
#' @references Pustejovsky, J. E. (2015). Measurement-comparable effect sizes
#'   for single-case studies of free-operant behavior. \emph{Psychological
#'   Methods, 20}(3), 342--359.
#'   doi:\doi{10.1037/met0000019}
#'
#' @return A data.frame containing the estimate, standard error, and approximate
#'   confidence interval.
#'
#' @examples
#' A_pct <- c(20, 20, 25, 25, 20, 25)
#' B_pct <- c(30, 25, 25, 25, 35, 30, 25)
#' LOR(A_data = A_pct, B_data = B_pct,
#'     scale = "percentage", intervals = 20, bias_correct = FALSE)
#' LOR(A_data = A_pct, B_data = B_pct,
#'     scale = "percentage", intervals = 20)
#'
#' LOR(A_data = A_pct, B_data = B_pct, scale = "percentage")
#' LOR(A_data = A_pct / 100, B_data = B_pct / 100, scale = "proportion")
#' LOR(A_data = A_pct, B_data = B_pct, scale = "percentage", improvement = "decrease")
#'
#' @export

# Check against calculations in Pustejovsky (2015)

LOR <- function(A_data, B_data, condition, outcome, baseline_phase,
                 improvement = "increase", 
                 scale = "percentage", 
                 intervals = NULL, D_const = NULL,
                 bias_correct = TRUE, confidence = .95) {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          ES = "LOR", improvement = improvement, 
          scale = scale, intervals = intervals, D_const = D_const,
          bias_correct = bias_correct, confidence = confidence)  
  
}

calc_LOR <- function(A_data, B_data, improvement = "increase", 
                      scale = "percentage", intervals = NULL, D_const = NULL,
                      bias_correct = TRUE, confidence = .95, ..., warn = TRUE) {

  if (length(scale) > 1L) scale <- names(sort(table(scale), decreasing = TRUE)[1])
  
  if (!scale %in% c("proportion", "percentage")) {
    if (warn) warning("LOR can only be calculated for proportions or percentages. Will return NAs for other outcome scales.", call. = FALSE)
    res <- data.frame(ES = "LOR", Est = NA, 
                      SE = NA, stringsAsFactors = FALSE)
    if (!is.null(confidence)) {
      res$CI_lower <- NA
      res$CI_upper <- NA 
    }
    return(res)
  }
    
  # calculate truncation constant if not supplied
  if (is.null(D_const)) D_const <- trunc_constant(scale, observation_length = NULL, intervals)
  if (all(is.na(D_const))) D_const <- trunc_constant(scale, observation_length = NULL, intervals)
  if (length(D_const) > 1) D_const <- mean(D_const, na.rm = TRUE)
  
  # check for valid outcome range, re-scale percentages to proportions
  all_dat <- c(A_data, B_data)
  if (scale == "proportion" & any(all_dat < 0 | all_dat > 1)) stop("Proportions must be between 0 and 1!") 
  if (scale == "percentage" & any(all_dat < 0 | all_dat > 100)) stop("Percentages must be between 0 and 100!") 

  if (scale == "percentage") {
    A_data <- A_data / 100
    B_data <- B_data / 100
    D_const <- 100 * D_const
  }  
  
  dat <- summary_stats(A_data, B_data, warn = warn)
  
  trunc_lower <- 1 / (2 * D_const * dat$n)
  if (D_const > 0) {
    dat$M <- pmin(1 - trunc_lower, pmax(trunc_lower, dat$M))
    dat$V <- pmax(1 / (D_const^2 * dat$n^3), dat$V)
  }
    
  
  log_odds <- with(dat, log(M) - log(1 - M))

  if (bias_correct == TRUE) {
    BC <- with(dat, (2 * M - 1) * V / (2 * n * M^2 * (1 - M)^2))
    lOR <- diff(log_odds - BC)
  } else {
    lOR <- diff(log_odds)
  }
  
  if (improvement == "decrease") {
    lOR <- -1L * lOR
  }
  
  SE <- with(dat, sqrt(sum(V / (n * M^2 * (1 - M)^2))))
  
  res <- data.frame(ES = "LOR", Est = lOR, 
                    SE = SE, stringsAsFactors = FALSE)
  
  if (!is.null(confidence)) {
    CI <- lOR + c(-1, 1) * qnorm(1 - (1 - confidence) / 2) * SE
    res$CI_lower <- CI[1]
    res$CI_upper <- CI[2] 
  }
  
  res
  
}

#' @name LRR
#' @title Log-response ratio
#'
#' @description Calculates the increasing or decreasing version of the
#'   log-response ratio effect size index, with or without bias correction
#'   (Pustejovsky, 2015)
#'
#' @param scale character string indicating the scale of the outcome variable,
#'   with possible values \code{"percentage"} for a percentage with range 0-100,
#'   \code{"proportion"} for a proportion with range 0-1, \code{"count"} for a
#'   frequency count (0 or positive integers), \code{"rate"} for a standardized
#'   rate per minute. If a vector, the most frequent unique value will be used.
#' @param observation_length length of observation session (in minutes). If a
#'   vector, the mean observation session length will be used.
#' @param intervals for interval recording procedures, the total number of
#'   intervals per observation session. If a vector, the mean number of
#'   intervals will be used.
#' @param D_const constant used for calculating the truncated sample mean (see
#'   Pustejovsky, 2018). If a vector, the mean value will be used.
#' @param bias_correct  logical value indicating whether to use bias-correction.
#'   Default is \code{TRUE}.
#' @param pct_change logical value indicating whether to convert the LRR
#'   estimate and confidence interval into percentage change.
#' @inheritParams calc_ES
#'
#' @details The response ratio parameter is the ratio of the mean level of the
#'   outcome during phase B to the mean level of the outcome during phase A. The
#'   log response ratio is the natural logarithm of the response ratio. This
#'   effect size is appropriate for outcomes measured on a ratio scale (so that
#'   zero corresponds to the true absence of the outcome. There are two versions
#'   of the LRR. The LRR-increasing (\code{LRRi}) is defined so that positive
#'   values correspond to therapeutic improvements. The LRR-decreasing
#'   (\code{LRRd}) is defined so that negative values correspond to therapeutic
#'   improvements. For outcomes measured as frequency counts or rates, the two
#'   versions will have the same magnitude but opposite sign; for outcomes
#'   measured as percentages or proportions, the LRRd and LRRi will differ in
#'   both sign and magnitude (Pustejovsky, 2018).
#'
#'   Without bias correction, the log response ratio is estimated as the natural
#'   logarithm of the phase B sample mean, minus the natural logarithm of the
#'   phase A sample mean. A delta-method bias correction to the estimator is
#'   used by default.
#'
#'   The standard error of LRR is calculated based on a delta-method
#'   approximation, allowing for the possibility of different degrees of
#'   dispersion in each phase. The confidence interval for LRR is based on a
#'   large-sample (z) approximation.
#'
#'   To account for the possibility of sample means of zero, a truncated mean is
#'   calculated following the method described in Pustejovsky (2018). Truncated
#'   sample variances are also calculated to ensure that standard errors will be
#'   strictly larger than zero. The truncation constant depends on the scale of
#'   the outcome, the length of the observation sessions used to measure the
#'   dependent variable, and (for interval recording procedures) the total
#'   number of intervals per session (or the total number of items for other
#'   percentage/proportion scales). The argument \code{scale} must be specified
#'   in order to calculate an appropriate truncation constant. For standardized
#'   rates, the argument \code{observation_length} must also be specified; for
#'   percentages or proportions, the argument \code{intervals} must be
#'   specified. For outcomes measured using continuous recording procedures, set
#'   \code{intervals} equal to 60 times the length of the observation session in
#'   minutes.
#'
#'   If \code{pct_change} is \code{TRUE}, then the LRR estimate and confidence
#'   interval are converted into percentage change using the formula 
#'   Percentage change = 100 * (exp(LRR) - 1).
#'
#' @references Pustejovsky, J. E. (2015). Measurement-comparable effect sizes
#'   for single-case studies of free-operant behavior. \emph{Psychological
#'   Methods, 20}(3), 342--359.
#'   doi:\doi{10.1037/met0000019}
#'
#'   Pustejovsky, J. E. (2018). Using response ratios for meta-analyzing
#'   single-case designs with behavioral outcomes. \emph{Journal of School
#'   Psychology, 16}, 99-112.
#'   doi:\doi{10.1016/j.jsp.2018.02.003}
#'
#'
#'
#' @return A data.frame containing the estimate, standard error, and approximate
#'   confidence interval.
#'
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' LRRd(A_data = A, B_data = B, bias_correct = FALSE)
#' LRRd(A_data = A, B_data = B)
#' LRRd(A_data = A, B_data = B, pct_change = TRUE)
#' 
#' @export

# Check against calculations in Pustejovsky (2015, 2018)

#' @rdname LRR

LRRd <- function(A_data, B_data, condition, outcome, 
                 baseline_phase = unique(condition)[1],
                 improvement = "decrease", 
                 scale = "count", observation_length = NULL, 
                 intervals = NULL, D_const = NULL,
                 bias_correct = TRUE, pct_change = FALSE,
                 confidence = .95) {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          ES = "LRRd", improvement = improvement, 
          scale = scale, observation_length = observation_length, 
          intervals = intervals, D_const = D_const,
          bias_correct = bias_correct, pct_change = pct_change,
          confidence = confidence)  
}

#' @rdname LRR
#' @export

LRRi <- function(A_data, B_data, condition, outcome, 
                 baseline_phase = unique(condition)[1],
                 improvement = "increase", 
                 scale = "count", observation_length = NULL, 
                 intervals = NULL, D_const = NULL,
                 bias_correct = TRUE, pct_change = FALSE, 
                 confidence = .95) {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          ES = "LRRi", improvement = improvement, 
          scale = scale, observation_length = observation_length, 
          intervals = intervals, D_const = D_const,
          bias_correct = bias_correct, pct_change = pct_change, 
          confidence = confidence)  
  
}

pct_change <- function(LRR) 100 * (exp(LRR) - 1)

calc_LRRd <- function(A_data, B_data, improvement = "decrease", 
                      scale = "count", observation_length = NULL, 
                      intervals = NULL, D_const = NULL,
                      bias_correct = TRUE, pct_change = FALSE,
                      confidence = .95, warn = TRUE, ...) {

  if (length(scale) > 1L) scale <- names(sort(table(scale), decreasing = TRUE)[1])
  
  dat <- summary_stats(A_data, B_data, warn = warn)
  
  if (improvement == "increase") { 
    if (scale == "percentage") {
      dat$M <- 100 - dat$M
    } 
    if (scale == "proportion") {
      dat$M <- 1 - dat$M
    }
  }
  
  if (is.null(D_const)) D_const <- trunc_constant(scale, observation_length, intervals)
  if (all(is.na(D_const))) D_const <- trunc_constant(scale, observation_length, intervals)
  if (length(D_const) > 1) D_const <- mean(D_const, na.rm = TRUE)
  if (D_const > 0) {
    dat$M <- pmax(1 / (2 * D_const * dat$n), dat$M)
    dat$V <- pmax(1 / (D_const^2 * dat$n^3), dat$V)
  }
  
  
  if (bias_correct == TRUE) {
    BC <- with(dat, log(M) + V / (2 * n * M^2))
    lRR <- diff(BC)
  } else {
    lRR <- diff(log(dat$M))
  }
  
  if (improvement == "increase" & ! scale %in% c("percentage","proportion")) {
    lRR <- -1L * lRR
  }
  
  SE <- with(dat, sqrt(sum(V / (n * M^2))))
  
  res <- data.frame(ES = "LRRd", Est = lRR, 
                    SE = SE, stringsAsFactors = FALSE)
  
  if (pct_change) {
    pct_est <- data.frame(ES = "Pct_Change_d", Est = pct_change(lRR), 
                          SE = NA, stringsAsFactors = FALSE)
  }
  
  if (!is.null(confidence)) {
    CI <- lRR + c(-1, 1) * qnorm(1 - (1 - confidence) / 2) * SE
    res$CI_lower <- CI[1]
    res$CI_upper <- CI[2] 
    if (pct_change) {
      pct_est$CI_lower <- pct_change(CI[1])
      pct_est$CI_upper <- pct_change(CI[2])
    }
  }

  if (pct_change) {
    res <- rbind(res, pct_est)
  }
    
  res
  
}

calc_LRRi <- function(A_data, B_data, improvement = "increase", 
                      scale = "count", observation_length = NULL, 
                      intervals = NULL, D_const = NULL,
                      bias_correct = TRUE, pct_change = FALSE,
                      confidence = .95, warn = TRUE, ...) {

  if (length(scale) > 1L) scale <- names(sort(table(scale), decreasing = TRUE)[1])
  
  dat <- summary_stats(A_data, B_data, warn = warn)
  
  if (improvement == "decrease") { 
    if (scale == "percentage") {
      dat$M <- 100 - dat$M
    } 
    if (scale == "proportion") {
      dat$M <- 1 - dat$M
    }
  }
  
  if (is.null(D_const)) D_const <- trunc_constant(scale, observation_length, intervals)
  if (all(is.na(D_const))) D_const <- trunc_constant(scale, observation_length, intervals)
  if (length(D_const) > 1) D_const <- mean(D_const, na.rm = TRUE)
  if (D_const > 0) {
    dat$M <- pmax(1 / (2 * D_const * dat$n), dat$M)
    dat$V <- pmax(1 / (D_const^2 * dat$n^3), dat$V)
  }
    
  if (bias_correct == TRUE) {
    BC <- with(dat, log(M) + V / (2 * n * M^2))
    lRR <- diff(BC)
  } else {
    lRR <- diff(log(dat$M))
  }
  
  if (improvement == "decrease" & ! scale %in% c("percentage","proportion")) {
    lRR <- -1L * lRR
  }
  
  SE <- with(dat, sqrt(sum(V / (n * M^2))))
  res <- data.frame(ES = "LRRi", Est = lRR, 
                    SE = SE, stringsAsFactors = FALSE)
  
  if (pct_change) {
    pct_est <- data.frame(ES = "Pct_Change_i", Est = pct_change(lRR),
                          SE = NA, stringsAsFactors = FALSE)
  }
  
  if (!is.null(confidence)) {
    CI <- lRR + c(-1, 1) * qnorm(1 - (1 - confidence) / 2) * SE
    res$CI_lower <- CI[1]
    res$CI_upper <- CI[2] 
    if (pct_change) {
      pct_est$CI_lower <- pct_change(CI[1])
      pct_est$CI_upper <- pct_change(CI[2])
    }
  }
  
  if (pct_change) {
    res <- rbind(res, pct_est)
  }

  res
  
}

#' @title Within-case standardized mean difference
#'
#' @description Calculates the within-case standardized mean difference effect
#'   size index
#'
#' @inheritParams calc_ES
#' @param std_dev character string controlling how to calculate the standard
#'   deviation in the denominator of the effect size. Set to \code{"baseline"}
#'   (the default) to use the baseline standard deviation. Set to \code{"pool"}
#'   to use the pooled standard deviation.
#' @param bias_correct logical value indicating whether to use bias-correction
#'   (i.e., Hedges' g). Default is \code{TRUE}
#'
#' @details The standardized mean difference parameter is defined as the
#'   difference between the mean level of the outcome in phase B and the mean
#'   level of the outcome in phase A, scaled by the within-case standard
#'   deviation of the outcome in phase A. The parameter is estimated using
#'   sample means and sample standard deviations and (optionally) making a
#'   small-sample correction.
#'
#'   By default, the scaling factor is estimated using the sample standard
#'   deviation in phase A (the baseline phase) only. Set \code{std_dev = "pool"}
#'   to use the sample standard deviation pooled across both phases. Hedges'
#'   (1981) small-sample bias correction is applied by default.
#'
#' @return A list containing the estimate, standard error, and confidence
#'   interval.
#'
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' SMD(A_data = A, B_data = B, bias_correct = FALSE)
#' SMD(A_data = A, B_data = B)
#' SMD(A_data = A, B_data = B, std_dev = "pool")
#'
#' @export

# Check against calculations in Pustejovsky (2015)!

SMD <- function(A_data, B_data, condition, outcome, baseline_phase,
                improvement = "increase",
                std_dev = "baseline", 
                bias_correct = TRUE, 
                confidence = .95) {
 
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          ES = "SMD", improvement = improvement,
          std_dev = std_dev,
          bias_correct = bias_correct, confidence = confidence)
}


calc_SMD <- function(A_data, B_data, 
                     improvement = "increase",
                     std_dev = "baseline", 
                     bias_correct = TRUE, 
                     confidence = .95, warn = TRUE, ...) {
  
  dat <- summary_stats(A_data, B_data, warn = warn)
  
  if (std_dev == "baseline") {
    df <- dat$n[1] - 1
    s_sq <- dat$V[1]
    SV1 <- with(dat, 1 / n[1] + V[2] / (n[2] * V[1]))
  } else {
    df <- sum(dat$n - 1)
    s_sq <-  with(dat, sum((n - 1) * V, na.rm = TRUE) / df)
    SV1 <- sum(1 / dat$n)
  }
  
  J <- if (bias_correct) 1  - 3 / (4 * df - 1) else 1
  
  if (is.na(s_sq) | s_sq == 0) stop("There is no variation in the outcome! The SMD is not an appropriate ES for these data.")
  
  d <- J * diff(dat$M) / sqrt(s_sq) 
  
  if (improvement=="decrease") d <- -d
  
  SE <- J * sqrt(SV1 + d^2 / (2 * df))
  
  res <- data.frame(ES = "SMD", Est = d, 
                    SE = SE, stringsAsFactors = FALSE)
  
  if (!is.null(confidence)) {
    CI <- d + c(-1, 1) * qnorm(1 - (1 - confidence) / 2) * SE
    res$CI_lower <- CI[1]
    res$CI_upper <- CI[2] 
  }
  
  res

}
