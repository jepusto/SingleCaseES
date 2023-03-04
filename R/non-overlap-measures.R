#' @title Non-overlap of all pairs
#'
#' @description Calculates the non-overlap of all pairs index (Parker & Vannest,
#'   2009).
#'
#' @param SE character value indicating which formula to use for calculating the
#'   standard error of NAP, with possible values \code{"unbiased"} for the
#'   exactly unbiased estimator, \code{"Hanley"} for the Hanley-McNeil
#'   estimator, \code{"null"} for the (known) variance under the null hypothesis
#'   of no effect, or \code{"none"} to not calculate a standard error. Defaults
#'   to "unbiased".
#' @param trunc_const logical value indicating whether to return the truncation
#'   constant used to calculate the standard error.
#'   
#' @inheritParams calc_ES
#'
#'
#' @details NAP is calculated as the proportion of all pairs of one observation
#'   from each phase in which the measurement from the B phase improves upon the
#'   measurement from the A phase, with pairs of data points that are exactly
#'   tied being given a weight of 0.5. The range of NAP is [0,1], with a null
#'   value of 0.5.
#'
#'   The unbiased variance estimator was described by Sen (1967) and Mee (1990).
#'   The Hanley estimator was proposed by Hanley and McNeil (1982). The null
#'   variance is a known function of sample size, equal to the exact sampling
#'   variance when the null hypothesis of no effect holds. When the null
#'   hypothesis does not hold, the null variance will tend to over-estimate the
#'   true sampling variance of NAP.
#'
#'   The confidence interval for NAP is calculated based on the symmetrized
#'   score-inversion method (Method 5) proposed by Newcombe (2006).
#'
#' @references
#'
#' Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under
#' a receiver operating characteristic (ROC) curve. \emph{Radiology, 143},
#' 29--36. doi:\doi{10.1148/radiology.143.1.7063747}
#'
#' Mee, W. (1990). Confidence intervals for probabilities and tolerance regions
#' based on a generalization of the Mann-Whitney statistic. \emph{Journal of the
#' American Statistical Association, 85}(411), 793-800.
#' doi:\doi{10.1080/01621459.1990.10474942}
#'
#' Newcombe, R. G. (2006). Confidence intervals for an effect size measure based
#' on the Mann-Whitney statistic. Part 2: Asymptotic methods and evaluation.
#' \emph{Statistics in Medicine, 25}(4), 559--573. doi:\doi{10.1002/sim.2324}
#'
#' Parker, R. I., & Vannest, K. J. (2009). An improved effect size for
#' single-case research: Nonoverlap of all pairs. \emph{Behavior Therapy,
#' 40}(4), 357--67. doi:\doi{10.1016/j.beth.2008.10.006}
#'
#' Sen, P. K. (1967). A note on asymptotically distribution-free confidence
#' bounds for P{X<Y}, based on two independent samples. \emph{The Annals of
#' Mathematical Statistics, 29}(1), 95-102.
#' \href{https://www.jstor.org/stable/25049448}{https://www.jstor.org/stable/25049448}
#'
#' @export
#'
#' @return A data.frame containing the estimate, standard error, and/or
#'   confidence interval.
#'
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' NAP(A_data = A, B_data = B)
#'
#' # Example from Parker & Vannest (2009)
#' yA <- c(4, 3, 4, 3, 4, 7, 5, 2, 3, 2)
#' yB <- c(5, 9, 7, 9, 7, 5, 9, 11, 11, 10, 9)
#' NAP(yA, yB)
#' 

NAP <- function(A_data, B_data, condition, outcome, 
                baseline_phase = NULL,
                intervention_phase = NULL,
                improvement = "increase", 
                SE = "unbiased", confidence = .95, trunc_const = FALSE) {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = "NAP", improvement = improvement, SE = SE, 
          confidence = confidence, trunc_const = trunc_const)
}
  
calc_NAP <- function(A_data, B_data, 
                     improvement = "increase", 
                     SE = "unbiased", confidence = .95, trunc_const = FALSE, ...) {
  
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  
  m <- length(A_data)
  n <- length(B_data)
  
  Q_mat <- matrix(sapply(B_data, function(j) (j > A_data) + 0.5 * (j == A_data)), nrow = m, ncol = n)
  NAP <- mean(Q_mat)
  
  res <- data.frame(ES = "NAP", Est = NAP, stringsAsFactors = FALSE)
  
  if (SE != "none") {
    Q1 <- sum(rowSums(Q_mat - NAP)^2) / (m * n^2)
    Q2 <- sum(colSums(Q_mat - NAP)^2) / (m^2 * n)
    
    trunc <- 0.5 / (m * n)
    NAP_trunc <- min(max(NAP, trunc), 1 - trunc)
    
    if (SE == "unbiased") {
      X <- sum((Q_mat - NAP)^2) / (m * n)
      V <- (NAP_trunc * (1 - NAP_trunc) + n * Q1 + m * Q2 - 2 * X) / ((m - 1) * (n - 1))
    }
    if (SE == "Hanley") {
      V <- (NAP_trunc * (1 - NAP_trunc) + (n - 1) * Q1 + (m - 1) * Q2) / (m * n)  
    } 
    if (SE == "null") {
      V <- (m + n + 1) / (12 * m * n)
    }
    
    res$SE <- sqrt(V)  
    if (trunc_const) res$trunc <- trunc
    
    if (!is.null(confidence)) {
      h <- (m + n) / 2 - 1
      z <- qnorm(1 - (1 - confidence) / 2)
      f <- function(x) m * n * (NAP - x)^2 * (2 - x) * (1 + x) - 
        z^2 * x * (1 - x) * (2 + h + (1 + 2 * h) * x * (1 - x))
      res$CI_lower <- if (NAP > 0) uniroot(f, c(0, NAP))$root else 0
      res$CI_upper <- if (NAP < 1) uniroot(f, c(NAP, 1))$root else 1
    }
    
  } 
  
  res
}


#' @title Tau (non-overlap)
#'   
#' @description Calculates the Tau (non-overlap) index (Parker, Vannest, Davis, 
#'   & Sauber 2011).
#'   
#' @inheritParams NAP
#'   
#' @details Tau (non-overlap) a linear re-scaling of \code{\link{NAP}} to the
#'   range [-1,1], with a null value of 0.
#'   
#'   Standard errors and confidence intervals for Tau are based on 
#'   transformations of the corresponding SEs and CIs for \code{\link{NAP}}.
#'   
#' @references Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. 
#'   (2011). Combining nonoverlap and trend for single-case research: Tau-U. 
#'   \emph{Behavior Therapy, 42}(2), 284--299. 
#'   doi:\doi{10.1016/j.beth.2010.08.006}
#'   
#'   
#' @export
#' 
#' @return A list containing the estimate, standard error, and/or confidence 
#'   interval.
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' Tau(A_data = A, B_data = B)
#' 

Tau <- function(A_data, B_data, condition, outcome, 
                baseline_phase = NULL,
                intervention_phase = NULL,
                improvement = "increase", 
                SE = "unbiased", confidence = .95, trunc_const = FALSE) {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = "Tau", improvement = improvement, SE = SE, 
          confidence = confidence, trunc_const = trunc_const)
  
}

calc_Tau <- function(A_data, B_data, 
                     improvement = "increase", 
                     SE = "unbiased", CI = TRUE, 
                     confidence = .95, trunc_const = FALSE, ...) {
  
  nap <- calc_NAP(A_data = A_data, B_data = B_data, 
                  improvement = improvement, 
                  SE = SE, CI = CI, confidence = confidence, trunc_const = trunc_const)
  
  res <- data.frame(ES = "Tau", Est = 2 * nap$Est - 1, stringsAsFactors = FALSE)
  
  if (SE != "none") {
    res$SE <- 2 * nap$SE
    if (trunc_const) res$trunc <- 2 * nap$trunc
  }
  
  if (!is.null(confidence)) {
    res$CI_lower <- 2 * nap$CI_lower - 1
    res$CI_upper <- 2 * nap$CI_upper - 1
  } 
  
  res
}


#' @title Tau-U
#'   
#' @description Calculates the Tau-U index with baseline trend correction 
#'   (Parker, Vannest, Davis, & Sauber 2011).
#'   
#' @param A_data vector of numeric data for A phase, sorted in order of session 
#'   number. Missing values are dropped.
#' @inheritParams NAP
#'   
#' @details Tau-U is an elaboration of the \code{\link{Tau}} that includes a 
#'   correction for baseline trend. It is calculated as Kendall's S statistic
#'   for the comparison between the phase B data and the phase A data, plus
#'   Kendall's S statistic for the A phase observations, scaled by the product
#'   of the number of observations in each phase.
#'   
#'   Note that \code{A_data} must be ordered by session number.
#'   
#' @references Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. 
#'   (2011). Combining nonoverlap and trend for single-case research: Tau-U. 
#'   \emph{Behavior Therapy, 42}(2), 284--299. 
#'   doi:\doi{10.1016/j.beth.2010.08.006}
#'   
#' @export
#' 
#' @return Numeric value
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' Tau_U(A_data = A, B_data = B)
#' 

Tau_U <- function(A_data, B_data, condition, outcome, 
                  baseline_phase = NULL,
                  intervention_phase = NULL,
                  improvement = "increase") {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = "Tau_U", improvement = improvement)
}
  
calc_Tau_U <- function(A_data, B_data, improvement = "increase", ...) {
  
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  
  m <- length(A_data)
  n <- length(B_data)
  
  Q_P <- sapply(B_data, function(j) (j > A_data) - (j < A_data))
  Q_B <- sapply(A_data, function(j) (j > A_data) - (j < A_data))
  
  TauU <- (sum(Q_P) - sum(Q_B[upper.tri(Q_B)])) / (m * n)

  data.frame(ES = "Tau-U", Est = TauU, stringsAsFactors = FALSE)
  
}


#' @title Tau-BC
#'
#' @description Calculates the baseline-corrected Tau index (Tarlow 2017).
#'
#' @param SE character value indicating which formula to use for calculating the
#'   standard error of Tau-BC, with possible values \code{"unbiased"} for the
#'   exactly unbiased estimator, \code{"Hanley"} for the Hanley-McNeil
#'   estimator, \code{"null"} for the (known) variance under the null hypothesis
#'   of no effect, or \code{"none"} to not calculate a standard error. Defaults
#'   to "unbiased". Note that the "unbiased" standard error is unbiased for
#'   \code{\link{Tau}}, but not necessarily unbiased for \code{\link{Tau_BC}}.
#'   None of the standard error formulas account for the additional uncertainty
#'   due to use of the baseline trend correction.
#' @param Kendall logical value indicating whether to use Kendall's rank
#'   correlation to calculate the Tau effect size measure. If \code{TRUE}, the
#'   Kendall's rank correlation (with adjustment for ties) is calculated between
#'   the data and a dummy coded phase variable, which is consistent with the
#'   method used in Tarlow (2017). Default is \code{FALSE}, which calculates
#'   \code{\link{Tau}} (non-overlap) index (without adjustment for ties).
#' @param pretest_trend significance level for the initial baseline trend test.
#'   The raw data are corrected and \code{\link{Tau_BC}} is calculated only if
#'   the baseline trend is statistically significant. Otherwise,
#'   \code{\link{Tau_BC}} is equal to \code{\link{Tau}}. Default is
#'   \code{FALSE}, which always adjusts for the baseline trend.
#' @param report_correction logical value indicating whether to report the
#'   baseline corrected slope and intercept values. Default is \code{FALSE}.
#' @param warn logical value indicating whether to print a message regarding the
#'   outcome of the baseline trend test. Default is \code{TRUE}.
#' @inheritParams NAP
#'
#' @details Tau-BC is an elaboration of the \code{\link{Tau}} that includes a
#'   correction for baseline trend. The calculation of Tau-BC involves two or
#'   three steps, depending on the \code{pretest_trend} argument.
#'
#'   If \code{pretest_trend = FALSE} (the default), the first step involves
#'   adjusting the outcomes for baseline trend estimated using Theil-Sen
#'   regression. In the second step, the residuals from Theil-Sen regression are
#'   used to calculate the \code{Tau} (using either Kendall's rank correlation,
#'   with adjustment for ties, or computing Tau directly, without adjustment for
#'   ties).
#'
#'   Alternately, \code{pretest_trend} can be set equal to a significance level
#'   between 0 and 1 (e.g. \code{pretest_trend = .05}, as suggested by Tarlow
#'   (2017). In this case, the first step involves a significance test for the
#'   slope of the baseline trend based on Kendall's rank correlation. If the
#'   slope is not significantly different from zero, then no baseline trend
#'   adjustment is made and Tau-BC is set equal to \code{Tau} index. If the
#'   slope is significantly different from zero, then in the second step, the
#'   outcomes are adjusted for baseline trend using Theil-Sen regression. Then,
#'   in the third step, the residuals from Theil-Sen regression are used to
#'   calculate the \code{Tau} index. If \code{Kendall = FALSE} (the default),
#'   then \code{\link{Tau}} (non-overlap) index is calculated. If \code{Kendall
#'   = TRUE}, then Kendall's rank correlation is calculated, including
#'   adjustment for ties, as in Tarlow (2017).
#'
#'   Note that the standard error formulas are based on the standard errors for
#'   \code{\link{Tau}} (non-overlap) and they do not account for the additional
#'   uncertainty due to use of the baseline trend correction (nor to the
#'   pre-test for statistical significance of baseline trend, if used).
#'
#' @seealso \code{\link{Tau}}, \code{\link{Tau_U}}
#' 
#' @references Tarlow, K. R. (2017). An improved rank correlation effect size
#'   statistic for single-case designs: Baseline corrected Tau. \emph{Behavior
#'   modification, 41}(4), 427-467. doi:\doi{10.1177/0145445516676750}
#'
#' @export
#'
#' @return A list containing the estimate, standard error, and/or confidence
#'   interval.
#'
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' Tau_BC(A_data = A, B_data = B)
#'
#' @importFrom utils combn
#'   

Tau_BC <- function(A_data, B_data, condition, outcome,
                  baseline_phase = NULL,
                  intervention_phase = NULL,
                  improvement = "increase", 
                  SE = "unbiased", confidence = .95,
                  trunc_const = FALSE,
                  Kendall = FALSE,
                  pretest_trend = FALSE,
                  report_correction = FALSE,
                  warn = TRUE
                  ) {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = "Tau_BC", improvement = improvement,
          SE = SE, confidence = confidence, trunc_const = trunc_const,
          Kendall = Kendall,
          pretest_trend = pretest_trend,
          report_correction = report_correction,
          warn = warn
          )
}

calc_Tau_BC <- function(A_data, B_data, 
                        improvement = "increase", 
                        SE = "unbiased", CI = TRUE,
                        confidence = .95, trunc_const = FALSE,
                        Kendall = FALSE,
                        pretest_trend = FALSE,
                        report_correction = FALSE, 
                        warn = TRUE,
                        ...) {
  
  m <- length(A_data)
  n <- length(B_data)
  session_A <- 1:m
  session_B <- (m + 1) : (m + n)
  
  if (pretest_trend > 0 & pretest_trend < 1) {
    
    if (!requireNamespace("Kendall", quietly = TRUE)) {
      stop("The baseline trend test requires the Kendall package. Please install it.", call. = FALSE)
    }
    
    pval_slope_A <- Kendall::Kendall(A_data, session_A)$sl
    
    if (pval_slope_A > pretest_trend) {
      
      if (warn) message("The baseline trend is not statistically significant. Tau is calculated without trend correction.")
      adjust <- FALSE
      
    } else {
      adjust <- TRUE
    }
    
  } else if (pretest_trend != FALSE) {
    
    stop("The pretest_trend argument must be FALSE or a number between 0 and 1.")
    
  } else {
    adjust <- TRUE
  }

  # Calculate baseline trend coefficients  
  slopes <- apply(combn(m, 2), 2, function(x) diff(A_data[x]) / diff(session_A[x]))
  slope <- median(slopes)
  intercept <- median(A_data - session_A * slope)
  
  # Adjust if necessary
  if (adjust) {
    A_data <- A_data - intercept - slope * session_A
    B_data <- B_data - intercept - slope * session_B
    
  }
  
  if (Kendall) {
    
    if (improvement == "decrease") {
      A_data <- -1 * A_data
      B_data <- -1 * B_data
    }
    
    # Tarlow (2017) approach to calculating Tau
    
    if (sd(c(A_data, B_data)) == 0) {
      tau <- 0
    } else {
      res_Kendall <- Kendall::Kendall(c(A_data, B_data), 
                                      c(rep(0L, m), rep(1L, n)))
      
      tau <- as.numeric(res_Kendall$tau)
    }
    
    res <- data.frame(ES = "Tau-BC", Est = tau, stringsAsFactors = FALSE)
    res$SE <- sqrt((2/(m+n)) * (1-(tau^2)))
    
    if (!is.null(confidence)) {
      CI <- tau + c(-1, 1) * qnorm(1 - (1 - confidence) / 2) * res$SE
      res$CI_lower <- CI[1]
      res$CI_upper <- CI[2]
    } 
    
  } else {
    
    # Calculate basic Tau (no adjustment for ties)
    
    res <- calc_Tau(A_data = A_data, B_data = B_data,
                    improvement = improvement, SE = SE, 
                    CI = CI, confidence = confidence, trunc_const = trunc_const)
    res$ES <- "Tau-BC"
    
  } 
  
  if (report_correction) {
    res$slope <- slope
    res$intercept <- intercept
    if (pretest_trend != FALSE) res$pval_slope_A <- pval_slope_A
  }
    
  return(res)
  
}

#' @title Percentage of non-overlapping data
#'   
#' @description Calculates the percentage of non-overlapping data index 
#'   (Scruggs, Mastropieri, & Castro, 1987).
#'   
#' @inheritParams NAP
#'   
#' @details For an outcome where increase is desirable, PND is calculated as the
#'   proportion of observations in the B phase that exceed the highest 
#'   observation from the A phase. For an outcome where decrease is desirable, 
#'   PND is the proportion of observations in the B phase that are less than the
#'   lowest observation from the A phase. The range of PND is [0,1].
#'   
#' @references Scruggs, T. E., Mastropieri, M. A., & Casto, G. (1987). The 
#'   quantitative synthesis of single-subject research: Methodology and 
#'   validation. \emph{Remedial and Special Education, 8}(2), 24--43. 
#'   doi:\doi{10.1177/074193258700800206}
#'   
#'   
#'   
#' @export
#' 
#' @return Numeric value
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' PND(A_data = A, B_data = B)
#' 

PND <- function(A_data, B_data, condition, outcome,
                baseline_phase = NULL,
                intervention_phase = NULL,
                improvement = "increase") {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = "PND", improvement = improvement)
  
}
  
calc_PND <- function(A_data, B_data, improvement = "increase", ...) {
  
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  
  PND <- mean(B_data > max(A_data, na.rm = TRUE), na.rm = TRUE)
  data.frame(ES = "PND", Est = PND, stringsAsFactors = FALSE)
  
}

#' @title Percentage exceeding the median
#'   
#' @description Calculates the percentage exceeding the median (PEM) index (Ma, 
#'   2006).
#'   
#' @inheritParams NAP
#'   
#' @details For an outcome where increase is desirable, PEM is calculated as the
#'   proportion of observations in the B phase that exceed the median 
#'   observation from the A phase. For an outcome where decrease is desirable, 
#'   PEM is calculated as the proportion of observations in the B phase that are
#'   less than the median observation from the A phase. Ties are counted with a
#'   weight of 0.5. The range of PEM is [0,1].
#'   
#' @references Ma, H.-H. (2006). An alternative method for quantitative 
#'   synthesis of single-subject researches: Percentage of data points exceeding
#'   the median. \emph{Behavior Modification, 30}(5), 598--617. 
#'   doi:\doi{10.1177/0145445504272974}
#'   
#'   
#'   
#' @export
#' 
#' @return Numeric value
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' PEM(A_data = A, B_data = B)
#' 

PEM <- function(A_data, B_data, condition, outcome,
                baseline_phase = NULL,
                intervention_phase = NULL,
                improvement = "increase") {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = "PEM", improvement = improvement)
  
}

calc_PEM <- function(A_data, B_data, improvement = "increase", ...) {
  
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  
  med <- median(A_data, na.rm = TRUE)
  
  PEM <- mean((B_data > med) + 0.5 * (B_data == med), na.rm = TRUE)
  
  data.frame(ES = "PEM", Est = PEM, stringsAsFactors = FALSE)
  
}

#' @title Percentage of all non-overlapping data (PAND)
#'   
#' @description Calculates the percentage of all non-overlapping data index 
#'   (Parker, Hagan-Burke, & Vannest, 2007; Parker, Vannest, & Davis, 2011).
#'   
#' @inheritParams NAP
#'   
#' @details For an outcome where increase is desirable, PAND is calculated as
#'   the proportion of observations remaining after removing the fewest possible
#'   number of observations from either phase so that the highest remaining
#'   point from the baseline phase is less than the lowest remaining point from
#'   the treatment phase. For an outcome where decrease is desirable, PAND is calculated as
#'   the proportion of observations remaining after removing the fewest possible
#'   number of observations from either phase so that the lowest remaining
#'   point from the baseline phase is greater than the highest remaining point from
#'   the treatment phase. The range of PAND depends on the number of
#'   observations in each phase.
#'   
#' @references Parker, R. I., Hagan-Burke, S., & Vannest, K. J. (2007). 
#'   Percentage of all non-overlapping data (PAND): An alternative to PND. 
#'   \emph{The Journal of Special Education, 40}(4), 194--204. 
#'   doi:\doi{10.1177/00224669070400040101}
#'   
#'   Parker, R. I., Vannest, K. J., & Davis, J. L. (2011). Effect size in 
#'   single-case research: A review of nine nonoverlap techniques. 
#'   \emph{Behavior Modification, 35}(4), 303--22. 
#'   doi:\doi{10.1177/0145445511399147}
#'   
#'   
#'   
#' @export
#' 
#' @return Numeric value
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' PAND(A_data = A, B_data = B)
#' 

PAND <- function(A_data, B_data, condition, outcome,
                 baseline_phase = NULL,
                 intervention_phase = NULL,
                 improvement = "increase") {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = "PAND", improvement = improvement)
  
}

calc_PAND <- function(A_data, B_data, improvement = "increase", ...) {
  
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  
  m <- length(A_data)
  n <- length(B_data)
  
  X <- c(-Inf, sort(A_data))
  Y <- c(sort(B_data), Inf)
  ij <- expand.grid(i = 1:(m + 1), j = 1:(n + 1))
  ij$no_overlap <- mapply(function(i, j) X[i] < Y[j], i = ij$i, j = ij$j)
  ij$overlap <- with(ij, i + n - j)
  overlaps <- with(ij, max(overlap * no_overlap))
  
  PAND <- overlaps / (m + n)
  data.frame(ES = "PAND", Est = PAND, stringsAsFactors = FALSE)
  
}

#' @title Robust improvement rate difference
#'   
#' @description Calculates the robust improvement rate difference index (Parker,
#'   Vannest, & Brown, 2009). The range of IRD depends on the number of 
#'   observations in each phase.
#'   
#' @inheritParams NAP
#'   
#' @seealso \code{\link{PAND}}
#'   
#' @references Parker, R. I., Vannest, K. J., & Brown, L. (2009). The
#'   improvement rate difference for single-case research. \emph{Exceptional Children,
#'   75}(2), 135--150. doi:\doi{10.1177/001440290907500201}
#'   
#' @export
#' 
#' @return Numeric value 
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' IRD(A_data = A, B_data = B)
#' 

IRD <- function(A_data, B_data, condition, outcome,
                baseline_phase = NULL,
                intervention_phase = NULL,
                improvement = "increase") {
  
  calc_ES(A_data = A_data, B_data = B_data, 
          condition = condition, outcome = outcome, 
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = "IRD", improvement = improvement)
  
}

calc_IRD <- function(A_data, B_data, improvement = "increase", ...) {
  
  pand <- calc_PAND(A_data = A_data, B_data = B_data, improvement = improvement)
  
  m <- sum(!is.na(A_data))
  n <- sum(!is.na(B_data))
  
  IRD <- ((m+n)^2 * pand$Est - m^2 - n^2) / (2 * m * n)
  
  data.frame(ES = "IRD", Est = IRD, stringsAsFactors = FALSE)
}
