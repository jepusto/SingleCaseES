#' @title Non-overlap of all pairs
#'   
#' @description Calculates the non-overlap of all pairs index (Parker & Vannest,
#'   2009).
#'   
#' @param A_data vector of numeric data for A phase. Missing values are dropped.
#' @param B_data vector of numeric data for B phase. Missing values are dropped.
#' @param improvement character string indicating direction of improvement.
#'   Default is "increase"
#' @param SE logical value indicating whether to report the standard error
#' @param CI logical value indicating whether to report a confidence interval
#' @param confidence confidence level for the reported interval estimate
#'   
#' @details NAP is calculated as the proportion of all pairs of one observation 
#'   from each phase in which the measurement from the B phase improves upon the 
#'   measurement from the A phase, with pairs of data points that are exactly 
#'   tied being given a weight of 0.5. The range of NAP is [0,1], with a null
#'   value of 0.5.
#'   
#'   The standard error of NAP is calculated based on the method of Hanley and 
#'   McNeil (1982).
#'   
#'   The confidence interval for NAP is calculated based on the symmetrized 
#'   score-inversion method (Method 5) proposed by Newcombe (2006).
#'   
#' @references
#' 
#' Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under 
#' a receiver operating characteristic (ROC) curve. \emph{Radiology, 143}, 
#' 29--36. 
#' doi:\href{http://dx.doi.org/10.1148/radiology.143.1.7063747}{10.1148/radiology.143.1.7063747}
#' 
#' Newcombe, R. G. (2006). Confidence intervals for an effect size measure based
#' on the Mann-Whitney statistic. Part 2: Asymptotic methods and evaluation. 
#' \emph{Statistics in Medicine, 25}(4), 559--573. 
#' doi:\href{http://dx.doi.org/10.1002/sim.2324}{10.1002/sim.2324}
#' 
#' Parker, R. I., & Vannest, K. J. (2009). An improved effect size for 
#' single-case research: Nonoverlap of all pairs. \emph{Behavior Therapy, 
#' 40}(4), 357--67. 
#' doi:\href{http://dx.doi.org/10.1016/j.beth.2008.10.006}{10.1016/j.beth.2008.10.006}
#' 
#' @export
#' 
#' @return A list containing the estimate, standard error, and/or confidence
#'   interval.
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

NAP <- function(A_data, B_data, improvement = "increase", 
                SE = TRUE, CI = TRUE, confidence = .95) {
  
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  A_data <- A_data[!is.na(A_data)]
  B_data <- B_data[!is.na(B_data)]
  m <- length(A_data)
  n <- length(B_data)
  
  Q_mat <- sapply(B_data, function(j) (j > A_data) + 0.5 * (j == A_data))
  NAP <- mean(Q_mat)
  
  res <- list(Est = NAP)
  
  if (SE) {
    Q1 <- sum(rowSums(Q_mat)^2) / (m * n^2)
    Q2 <- sum(colSums(Q_mat)^2) / (m^2 * n)
    V <- (NAP * (1 - NAP) + (n - 1) * (Q1 - NAP^2) + (m - 1) * (Q2 - NAP^2)) / (m * n)  
    res$SE <- sqrt(V)  
  }
  
  if (CI) {
    h <- (m + n) / 2 - 1
    z <- qnorm(1 - (1 - confidence) / 2)
    f <- function(x) m * n * (NAP - x)^2 * (2 - x) * (1 + x) - 
      z^2 * x * (1 - x) * (2 + h + (1 + 2 * h) * x * (1 - x))
    lower <- if (NAP > 0) uniroot(f, c(0, NAP))$root else 0
    upper <- if (NAP < 1) uniroot(f, c(NAP, 1))$root else 1
    res$CI <- c(lower = lower, upper = upper)
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
#' @details Tau (non-overlap) is calculated as the Spearman rank-correlation 
#'   between the outcome observations and a binary variable indicating phase B. 
#'   Tau is a linear re-scaling of \code{\link{NAP}} to the range [-1,1], with a
#'   null value of 0.
#'   
#'   Standard errors and confidence intervals for Tau are based on 
#'   transformations of the corresponding SEs and CIs for \code{\link{NAP}}
#'   
#' @references Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. 
#'   (2011). Combining nonoverlap and trend for single-case research: Tau-U. 
#'   \emph{Behavior Therapy, 42}(2), 284--299. 
#'   doi:\href{http://dx.doi.org/10.1016/j.beth.2010.08.006}{10.1016/j.beth.2010.08.006}
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

Tau <- function(A_data, B_data, improvement = "increase", 
                SE = TRUE, CI = TRUE, confidence = .95) {
  nap <- NAP(A_data = A_data, B_data = B_data, improvement = improvement, 
             SE = SE, CI = CI, confidence = confidence)
  
  res <- list(Est = 2 * nap$Est - 1)
  if (SE) res$SE <- 2 * nap$SE
  if (CI) res$CI <- 2 * nap$CI - 1
  
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
#'   correction for baseline trend. It is calculated as the Kendall 
#'   rank-correlation coefficient between the outcome observations and a binary variable 
#'   indicating phase B, minus \deqn{(m - 1) / (2n)}{(m - 1) / (2n)} times the 
#'   Kendall rank-correlation coefficient between the outcome observations and the session 
#'   numbers within the baseline phase. 
#'   
#'   Note that \code{A_data} must be ordered by session number. 
#'   
#' @references Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. 
#'   (2011). Combining nonoverlap and trend for single-case research: Tau-U. 
#'   \emph{Behavior Therapy, 42}(2), 284--299. 
#'   doi:\href{http://dx.doi.org/10.1016/j.beth.2010.08.006}{10.1016/j.beth.2010.08.006}
#'   
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

Tau_U <- function(A_data, B_data, improvement = "increase") {
  
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  A_data <- A_data[!is.na(A_data)]
  B_data <- B_data[!is.na(B_data)]
  m <- length(A_data)
  n <- length(B_data)
  
  Q_P <- sapply(B_data, function(j) (j > A_data) - (j < A_data))
  Q_B <- sapply(A_data, function(j) (j > A_data) - (j < A_data))
  
  (sum(Q_P) - sum(Q_B[upper.tri(Q_B)])) / (m * n)
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
#'   doi:\href{http://dx.doi.org/10.1177/074193258700800206}{10.1177/074193258700800206}
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

PND <- function(A_data, B_data, improvement = "increase") {
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
#'   doi:\href{http://dx.doi.org/10.1177/0145445504272974}{10.1177/0145445504272974}
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
#'   doi:\href{http://dx.doi.org/10.1177/00224669070400040101}{10.1177/00224669070400040101}
#'   
#'   
#'   
#'   Parker, R. I., Vannest, K. J., & Davis, J. L. (2011). Effect size in 
#'   single-case research: A review of nine nonoverlap techniques. 
#'   \emph{Behavior Modification, 35}(4), 303--22. 
#'   doi:\href{http://dx.doi.org/10.1177/0145445511399147}{10.1177/0145445511399147}
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
#'   Vannest, & Brown, 2009). The range of IRD depends on the number of 
#'   observations in each phase.
#'   
#' @inheritParams NAP
#'   
#' @seealso \code{\link{PAND}}
#'   
#' @references Parker, R. I., Vannest, K. J., & Brown, L. (2009). The
#'   improvement rate difference for single-case research. \emph{Exceptional Children,
#'   75}(2), 135--150. doi:\href{http://dx.doi.org/10.1177/001440290907500201}{10.1177/001440290907500201}
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

IRD <- function(A_data, B_data, improvement = "increase") {
  pand <- PAND(A_data = A_data, B_data = B_data, improvement = improvement)
  m <- sum(!is.na(A_data))
  n <- sum(!is.na(B_data))
  
  ((m+n)^2 * pand - m^2 - n^2) / (2 * m * n)
}
