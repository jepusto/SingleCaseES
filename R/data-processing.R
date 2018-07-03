#' @title Calculate phase pair numbers
#'
#' @description Calculates a vector containing a unique phase number for every
#'   sequential occurrence of a phase or treatment condition. This is useful for 
#'   creating a grouping variable to be used in calculating effect sizes for each 
#'   pair of A-B phases within treatment reversal designs.
#'
#' @param x vector of phase/condition labels.
#'
#' @export
#'
#' @return A vector containing an integer phase number for every observation.
#'
#' @examples
#' x <- rep(c("A","B","C","B","C","A","C"), c(4:10))
#' calc_phase_pairs(x)
#' 
#' library(dplyr)
#' Schmidt2007 %>% 
#'   group_by(Behavior_type, Case_pseudonym) %>%
#'   mutate(phase_pair = calc_phase_pairs(Condition))
#' 

calc_phase_pairs <- function(x) {
  phases <- rle(x)
  P <- length(phases$values)
  phase_number <- vector(mode = "integer", length = P)
  for (p in 1:P) {
    phase_number[p] <- sum(phases$values[1:p] == phases$values[p])
  }
  rep(phase_number, times = phases$lengths)
}

