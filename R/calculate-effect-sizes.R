
calc_ES <- function(A_data, B_data, 
                    outcome, phase, 
                    baseline_phase = unique(phase)[1],
                    ES = c("LRRd","LRRi","SMD","NAP","Tau","Tau_U", "PND","IRD","PAND","PEM"), 
                    improvement = "increase", 
                    ..., 
                    confidence = confidence,
                    format = "long") {
  
  if (missing(A_data) | missing(B_data)) {
    if (missing(outcome) | missing(phase)) stop("You must provide the data using the outcome and phase arguments")
    
    phases <- unique(phase)
    if (length(phases) != 2) stop("The phase variable must have exactly two unique values.")
    dat <- split(outcome, phase)
    treatment_phase <- setdiff(phases, baseline_phase)
    A_data <- dat[[baseline_phase]]
    B_data <- dat[[treatment_phase]]
  } 
  
  ES_to_calc <- paste0("calc_", ES)
  
  res <- purrr::map(ES_to_calc, 
                       do.call, 
                       args = list(improvement = improvement, confidence = confidence, ...))
}
