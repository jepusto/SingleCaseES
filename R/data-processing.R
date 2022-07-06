#' @title Calculate phase pair numbers
#'
#' @description Calculates a vector containing a unique phase number for every
#'   sequential occurrence of a phase or treatment condition. This is useful for
#'   creating a grouping variable to be used in calculating effect sizes for
#'   each pair of A-B phases within treatment reversal designs.
#'
#' @param x vector of phase/condition labels.
#' @param session numeric vector of measurement occasions, used to sort \code{x}
#'   before calculating phase pairs.
#'
#' @export
#'
#' @return A vector containing an integer phase number for every observation.
#'
#' @examples
#' # Basic use-case
#' x <- rep(c("A","B","C","B","C","A","C"), c(4:10))
#' calc_phase_pairs(x)
#' 
#' # Using session argument to handle sort order
#' session <- sample(seq_along(x))
#' x_scrambled <- x[session]
#' dat <- data.frame(
#'   x = x_scrambled,
#'   session = session,
#'   phase_pair = calc_phase_pairs(x_scrambled, session = session)
#' )
#' dat_sorted <- dat[order(session),]
#' identical(x, dat_sorted$x)
#' 
#' # With a grouped data.frame
#' library(dplyr)
#' Schmidt2007 %>%
#'   group_by(Behavior_type, Case_pseudonym) %>%
#'   mutate(phase_pair = calc_phase_pairs(Condition, session = Session_number))
#' 

calc_phase_pairs <- function(x, session = seq_along(x)) {
  x <- as.character(x[order(session)])
  n <- length(x)
  y <- rep(1L, n)
  condition_list <- x[1]
  for (i in 2:n) {
    if (x[i] == x[i - 1]) {
      y[i] <- y[i - 1]
    } else if (!(x[i] %in% condition_list)) {
      y[i] <- y[i - 1]
      condition_list <- c(condition_list, x[i])
    } else {
      y[i] <- y[i - 1] + 1L
      condition_list <- x[i]
    }
  }
  y[order(order(session))]
}

