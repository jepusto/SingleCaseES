summary_stats <- function(A_data, B_data) {
  n <- c(sum(!is.na(A_data)), sum(!is.na(B_data)))
  M <- c(mean(A_data, na.rm = TRUE), mean(B_data, na.rm = TRUE))
  V <- c(var(A_data, na.rm = TRUE), var(B_data, na.rm = TRUE))
  data.frame(n = n, M = M, V = V)
}

#' @title Log-response ratio
#'   
#' @description Calculates the log-response ratio effect size index, with or 
#'   without bias correction (Pustejovsky, 2015)
#'   
#' @inheritParams NAP
#' @param bias_correct  logical value indicating whether to use bias-correction.
#'   Default is \code{TRUE}
#'   
#' @details The response ratio parameter is the ratio of the mean level of the 
#'   outcome during phase B to the mean level of the outcome during phase A. The
#'   log response ratio is the natural logarithm of the response ratio. Without 
#'   bias correction, the log response ratio is estimated as the natural 
#'   logarithm of the phase B sample mean, minus the natural logarithm of the 
#'   phase A sample mean. A delta-method bias correction to the estimator is 
#'   used by default.
#'   
#'   The standard error of LRR is calculated based on a delta-method 
#'   approximation, allowing for the possibility of different degrees of 
#'   dispersion in each phase. The confidence interval for LRR is based on a
#'   large-sample (z) approximation.
#'   
#' @references Pustejovsky, J. E. (2015). Measurement-comparable effect sizes 
#'   for single-case studies of free-operant behavior. \emph{Psychological 
#'   Methods, 20}(3), 342--359. 
#'   doi:\href{http://dx.doi.org/10.1037/met0000019}{10.1037/met0000019}

#' @return A list containing the estimate, standard error, and confidence 
#'   interval.
#'   
#' @examples 
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' LRR(A_data = A, B_data = B, bias_correct = FALSE)
#' LRR(A_data = A, B_data = B)
#' 
#' @export

# Check against calculations in Pustejovsky (2015)!

LRR <- function(A_data, B_data, bias_correct = TRUE, confidence = .95) {
  
  dat <- summary_stats(A_data, B_data)
  
  if (!all(dat$M > 0)) stop('The mean of one or both phases is at the floor of 0.')
  
  if (bias_correct == TRUE) {
    BC <- with(dat, log(M) + V / (2 * n * M^2))
    lRR <- BC[2] - BC[1]
  } else {
    lRR <- diff(log(dat$M))
  }
  
  SE <- with(dat, sqrt(sum(V / (n * M^2))))
  CI <- lRR + c(-1, 1) * qnorm(1 - (1 - confidence) / 2) * SE
  
  list(Est = lRR, SE = SE, CI = CI)
}


#' @title Within-case standardized mean difference
#'   
#' @description Calculates the within-case standardized mean difference effect 
#'   size index
#'   
#' @inheritParams NAP
#' @param std_dev character string controlling how to calculate the standard 
#'   deviation in the denominator of the effect size. Set to \code{"baseline"} 
#'   (the default) to use the baseline standard deviation. Set to \code{"pool"} 
#'   to use the pooled standard deviation.
#' @param bias_correct logical value indicating whether to use bias-correction. 
#'   Default is \code{TRUE}
#'   
#' @details The standardized mean difference parameter is defined as the 
#'   difference between the mean level of the outcome in phase B and the mean 
#'   level of the outcome in phase A, scaled by the within-case standard 
#'   deviation of the outcome in phase A. The parameter is estimated using 
#'   sample means and sample standard deviations and (optionally) making a 
#'   small-sample correction. 
#'   
#'   By default, the scaling factor is estimated using 
#'   the sample standard deviation in phase A (the baseline phase) only. Set
#'   \code{std_dev = "pool"} to use the sample standard deviation pooled across
#'   both phases. Hedges' (1981) small-sample bias correction is applied by default. 
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

SMD <- function(A_data, B_data, std_dev = "baseline", 
                bias_correct = TRUE, confidence = .95) {
  
  dat <- summary_stats(A_data, B_data)
  
  if (std_dev == "baseline") {
    df <- dat$n[1]
    s_sq <- dat$V[1]
    SV1 <- with(dat, 1 / n[1] + V[2] / (n[2] * V[1]))
  } else {
    df <- sum(dat$n - 1)
    s_sq <-  with(dat, sum((n - 1) * V) / df)
    SV1 <- sum(1 / dat$n)
  }
  
  J <- if (bias_correct) 1  - 3 / (4 * df - 1) else 1
  
  d <- J * diff(dat$M) / sqrt(s_sq) 
  SE <- J * sqrt(SV1 + d^2 / (2 * df))
  CI <- d + c(-1, 1) * qnorm(1 - (1 - confidence) / 2) * SE
  
  list(Est = d, SE = SE, CI = CI)
}
