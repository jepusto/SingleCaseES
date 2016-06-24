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
#' @inheritParams PND
#' @param SE logical value indicating whether to report the standard error
#' @param CI logical value indicating whether to report a confidence interval
#' @param confidence confidence level for the reported interval estimate
#' @param bias_correct  logical value indicating whether to use bias-correction.
#'   Default is \code{TRUE}
#'   
#' @details
#' 
#' @references Pustejovsky, J. E. (2015). Measurement-comparable effect sizes 
#'   for single-case studies of free-operant behavior. \emph{Psychological
#'   Methods, 20}(3), 342--359.
#'   doi:\href{http://dx.doi.org/10.1037/met0000019}{10.1037/met0000019}
#'   
#' @return If \code{SE = FALSE} and \code{CI = FALSE}, a numeric value. 
#'   Otherwise, a list containing the estimate, standard error, and/or confidence 
#'   interval.
#'   
#' @examples 
#' 
#' @export

# Check against calculations in Pustejovsky (2015)!

LRR <- function(A_data, B_data, SE = TRUE, CI = TRUE, confidence = .95, bias_correct = TRUE) {
  
  dat <- summary_stats(A_data, B_data)
  
  if (!all(dat$M > 0)) stop('The mean of one or both phases is at the floor of 0.')
  
  if (bias_correct == TRUE) {
    BC <- with(dat, log(M) + V / (2 * n * M^2))
    lRR <- BC[2] - BC[1]
  } else {
    lRR <- diff(log(dat$M))
  }
  
  SE <- with(dat, sqrt(sum(V / (n * M^2))))
  
  res <- list(Est = lRR)
  
  if (SE) res$SE <- SE
  
  if (CI) res$CI <- lRR + c(-1, 1) * qnorm(1-(1-conf_level)/2) * SE

  if (length(res) > 1) res else res[[1]]
}
