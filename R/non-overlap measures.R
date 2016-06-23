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
#'   validation. \emph{Remedial and Special Education, 8}(2), 24-43. 
#'   doi:\href{http://dx.doi.org/10.1177/074193258700800206}{10.1177/074193258700800206}
#'   
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
#' 

PND <- function(A_data, B_data, improvement = c("increase","decrease")) {
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  mean(B_data > max(A_data, na.rm = TRUE), na.rm = TRUE)
}


PEM <- function(A_data, B_data, improvement = "increase") {
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  med <- median(A_data, na.rm = TRUE)
  mean((B_data > med) + 0.5 * (B_data == med), na.rm = TRUE)
}


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

IRD <- function(A_data, B_data, improvement = "increase") {
  pand <- PAND(A_data = A_data, B_data = B_data, increase = increase)
  m <- sum(!is.na(A_data))
  n <- sum(!is.na(B_data))
  
  ((m+n)^2 * pand - m^2 - n^2) / (2 * m * n)
}


NAP <- function(A_data, B_data, improvement = "increase", SE = TRUE, alpha = .05) {
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  XY <- expand.grid(x = A_data, y = B_data)
  mean(with(XY, (y > x) + 0.5 * (y == x)), na.omit)
}


Tau <- function(A_data, B_data, improvement = "increase", SE = TRUE, alpha = .05) {
  nap <- NAP(A_data = A_data, B_data = B_data, improvement = improvement, SE = SE, alpha = alpha)
  2 * nap - 1
}