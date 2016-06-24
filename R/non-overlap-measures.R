
#' @title Percentage of non-overlapping data
#'   
#' @description Calculates the percentage of non-overlapping data index 
#'   (Scruggs, Mastropieri, & Castro, 1987).
#'   
#' @param A_data vector of numeric data for A phase
#' @param B_data vector of numeric data for B phase
#' @param improvement character string indicating direction of improvement.
#'   Default is "increase"
#'   
#' @details PND is calculated as the proportion of observations in the B phase 
#'   that exceed the highest observation from the A phase.
#'   
#' @references Scruggs, T. E., Mastropieri, M. A., & Casto, G. (1987). The 
#'   quantitative synthesis of single-subject research: Methodology and 
#'   validation. \emph{Remedial and Special Education, 8}(2), 24--43. 
#'   doi:\href{http://dx.doi.org/10.1177/074193258700800206}{10.1177/074193258700800206}
#'   
#' @export
#' 
#' @return Numeric value. The range of PND is [0,1].
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' PND(A_data = A, B_data = B)
#' 

PND <- function(A_data, B_data, improvement = c("increase","decrease")) {
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  mean(B_data > max(A_data, na.rm = TRUE), na.rm = TRUE)
}

#' @title Percentage exceeding the median
#'   
#' @description Calculates the percentage exceeding the median (PEM) index (Ma, 
#'   2006).
#'   
#' @inheritParams PND
#' 
#' @details PEM is calculated as the proportion of observations in the B phase 
#'   that exceed the median observation from the A phase. Ties are counted with
#'   a weight of 0.5.
#'   
#' @references Ma, H.-H. (2006). An alternative method for quantitative 
#'   synthesis of single-subject researches: Percentage of data points exceeding
#'   the median. \emph{Behavior Modification, 30}(5), 598--617. 
#'   doi:\href{http://dx.doi.org/10.1177/0145445504272974}{10.1177/0145445504272974}
#'   
#' @export
#' 
#' @return Numeric value. The range of PEM is [0,1].
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' PEM(A_data = A, B_data = B)
#' 

PEM <- function(A_data, B_data, improvement = "increase") {
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  med <- median(A_data, na.rm = TRUE)
  mean((B_data > med) + 0.5 * (B_data == med), na.rm = TRUE)
}

#' @title Percentage of all non-overlapping data (PAND)
#'   
#' @description Calculates the percentage of all non-overlapping data index 
#'   (Parker, Hagan-Burke, & Vannest, 2007; Parker, Vannest, & Davis, 2011).
#'   
#' @inheritParams PND
#'   
#' @details PAND is calculated as the proportion of observations remaining after
#'   removing (instead of swapping) the fewest possible number of observations 
#'   from either phase so that the highest remaining point from the baseline 
#'   phase is less than the lowest remaining point from the treatment phase
#'   
#' @references Parker, R. I., Hagan-Burke, S., & Vannest, K. J. (2007). 
#'   Percentage of all non-overlapping data (PAND): An alternative to PND. 
#'   \emph{The Journal of Special Education, 40}(4), 194--204. 
#'   doi:\href{http://dx.doi.org/10.1177/00224669070400040101}{10.1177/00224669070400040101}
#'   
#'   Parker, R. I., Vannest, K. J., & Davis, J. L. (2011). Effect size in 
#'   single-case research: A review of nine nonoverlap techniques. 
#'   \emph{Behavior Modification, 35}(4), 303--22. 
#'   doi:\href{http://dx.doi.org/10.1177/0145445511399147}{10.1177/0145445511399147}
#'   
#' @export
#' 
#' @return Numeric value. The range of PAND depends on the number of 
#'   observations in each phase.
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' PAND(A_data = A, B_data = B)
#' 

PAND <- function(A_data, B_data, improvement = "increase") {
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  A_data <- A_data[!is.na(A_data)]
  B_data <- B_data[!is.na(B_data)]
  
  m <- length(A_data)
  n <- length(B_data)
  X <- sort(A_data)
  Y <- sort(B_data)
  ij <- expand.grid(i = 1:m, j = 1:n)
  ij$no_overlap <- mapply(function(i, j) X[i] < Y[j], i = ij$i, j = ij$j)
  ij$overlap <- with(ij, i + n - j + 1)
  overlaps <- with(ij, max(overlap * no_overlap))
  
  overlaps / (m + n)
}

#' @title Robust improvement rate difference
#'   
#' @description Calculates the robust improvement rate difference index (Parker,
#'   Vannest, & Brown, 2009).
#'   
#' @inheritParams PND
#'   
#' @seealso \code{\link{PAND}}
#'   
#' @references Parker, R. I., Vannest, K. J., & Brown, L. (2009). The
#'   improvement rate difference for single-case research. \emph{Exceptional Children,
#'   75}(2), 135--150. doi:\href{http://dx.doi.org/10.1177/001440290907500201}{10.1177/001440290907500201}
#'   
#' @export
#' 
#' @return Numeric value. The range of IRD depends on the number of 
#'   observations in each phase.
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' IRD(A_data = A, B_data = B)
#' 

IRD <- function(A_data, B_data, improvement = "increase") {
  pand <- PAND(A_data = A_data, B_data = B_data, improvement = improvement)
  m <- sum(!is.na(A_data))
  n <- sum(!is.na(B_data))
  
  ((m+n)^2 * pand - m^2 - n^2) / (2 * m * n)
}

#' @title Non-overlap of all pairs
#'   
#' @description Calculates the non-overlap of all pairs index (Parker & Vannest,
#'   2009).
#'   
#' @inheritParams PND
#' @param SE logical value indicating whether to report the standard error of 
#'   NAP
#' @param confidence confidence level for the reported interval estimate of NAP.
#'   If \code{NULL} then no confidence interval is calculated.
#'   
#' @details NAP is calculated as the proportion of all pairs of one observation
#'   from each phase in which the measurement from the B phase exceeds the
#'   measurement from the A phase, with pairs of data points that are exactly
#'   tied being given a weight of 0.5.
#'   
#' @references Parker, R. I., & Vannest, K. J. (2009). An improved effect size 
#'   for single-case research: Nonoverlap of all pairs. \emph{Behavior Therapy,
#'   40}(4), 357--67.
#'   doi:\href{http://dx.doi.org/10.1016/j.beth.2008.10.006}{10.1016/j.beth.2008.10.006}
#'   
#' @export
#' 
#' @return Numeric value. The range of NAP is [0,1], with a null value of 0.5.
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' NAP(A_data = A, B_data = B)
#' 

NAP <- function(A_data, B_data, improvement = "increase", SE = TRUE, confidence = .95) {
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  XY <- expand.grid(x = A_data, y = B_data)
  mean(with(XY, (y > x) + 0.5 * (y == x)), na.rm=TRUE)
}

#' @title Tau (non-overlap)
#'   
#' @description Calculates the Tau (non-overlap) index (Parker, Vannest, Davis, 
#'   & Sauber 2011).
#'   
#' @inheritParams PND
#' @param SE logical value indicating whether to report the standard error of 
#'   Tau
#' @param confidence confidence level for the reported interval estimate of Tau.
#'   If \code{NULL} then no confidence interval is calculated.
#'   
#' @details Tau (non-overlap) is calculated as the Spearman rank-correlation 
#'   between the outcome observations and a binary variable indicating phase B. Tau
#'   is a linear re-scaling of NAP to the range [-1,1].
#'   
#' @references Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. 
#'   (2011). Combining nonoverlap and trend for single-case research: Tau-U. 
#'   \emph{Behavior Therapy, 42}(2), 284--299. 
#'   doi:\href{http://dx.doi.org/10.1016/j.beth.2010.08.006}{10.1016/j.beth.2010.08.006}
#'   
#' @export
#' 
#' @return Numeric value. The range of Tau is [-1,1], with a null value of 0.
#'   
#' @examples
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' Tau(A_data = A, B_data = B)
#' 

Tau <- function(A_data, B_data, improvement = "increase", SE = TRUE, confidence = .05) {
  nap <- NAP(A_data = A_data, B_data = B_data, improvement = improvement, 
             SE = SE, confidence = confidence)
  2 * nap - 1
}