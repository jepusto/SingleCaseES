
if (getRversion() >= "2.15.1") utils::globalVariables(c("n", "ES", "Est", "SE", "CI_upper", "CI_lower", "A", "B", "weights"))

# get ES names
get_ES_names <- function(ES) {
  ES_names <- if (identical(ES, "all")) {
    c("LRRd","LRRi","LOR","SMD","LRM","NAP","IRD","PAND","PND","PEM","PoGO","Tau","Tau_U","Tau_BC")
  } else if (identical(ES, "NOM")) {
    c("NAP","IRD","PAND","PND","PEM","Tau","Tau_U", "Tau_BC")
  } else if (identical(ES, "parametric")) {
    c("LRRd","LRRi","LOR","SMD", "LRM","PoGO")
  } else {
    ES
  }
  return(ES_names)
}

# convert long format to wide format
convert_to_wide <- function(res, ES_names) {
  
  if (any(ES_names == "Tau_U")) ES_names[ES_names == "Tau_U"] <- "Tau-U"
  if (any(ES_names == "Tau_BC")) ES_names[ES_names == "Tau_BC"] <- "Tau-BC"
  
  if (any(c("Pct_Change_d","Pct_Change_i") %in% res$ES)) {
    ES_names <- as.list(ES_names)
    if ("Pct_Change_d" %in% res$ES) ES_names[[which(ES_names == "LRRd")]] <- c("LRRd","Pct_Change_d")
    if ("Pct_Change_i" %in% res$ES) ES_names[[which(ES_names == "LRRi")]] <- c("LRRi","Pct_Change_i")
    ES_names <- unlist(ES_names)
  }
  
  output_names <- c("Est", "SE", "CI_lower", "CI_upper", "baseline_SD", "pooled_SD", "trunc")
  val_names <- intersect(names(res), output_names)
  sym_val_names <- rlang::syms(val_names)
  val <- rlang::sym("val")
  
  res <- 
    res |>
    tidyr::gather("q", !!val, !!!sym_val_names) |>
    tidyr::unite("q", ES, q) |>
    dplyr::filter(!is.na(val)) |>
    tidyr::spread("q", !!val) 
  
  # re-order names
  long_names <- 
    tidyr::expand_grid(ES_names, val_names) |>
    dplyr::mutate(ES_names = paste(ES_names, val_names, sep = "_")) |>
    dplyr::pull(ES_names) |>
    intersect(names(res)) |>
    rlang::syms()
  
  res <- res |> dplyr::relocate(!!!long_names, .after = tidyselect::last_col())
  
  return(res)
}


#' @title Calculate effect sizes
#'
#' @description Calculates one or more effect size estimates, along with
#'   associated standard errors and confidence intervals, if available, for a
#'   single-case data series.
#'
#' @param A_data vector of numeric data for A phase. Missing values are dropped.
#' @param B_data vector of numeric data for B phase. Missing values are dropped.
#' @param condition vector identifying the treatment condition for each
#'   observation in the series.
#' @param outcome vector of outcome data for the entire series.
#' @param baseline_phase character string specifying which value of
#'   \code{condition} corresponds to the baseline phase. Defaults to first
#'   observed value of \code{condition}.
#' @param intervention_phase character string specifying which value of
#'   \code{condition} corresponds to the intervention phase. Defaults to second
#'   unique value of \code{condition}.
#' @param ES character string or character vector specifying which effect size
#'   index or indices to calculate. Available effect sizes are \code{"LRRd"},
#'   \code{"LRRi"}, \code{"LRM"}, \code{"LOR"}, \code{"SMD"}, \code{"NAP"},
#'   \code{"IRD"}, \code{"PND"}, \code{"PEM"}, \code{"PAND"}, \code{"PoGO"},
#'   \code{"Tau"}, \code{"Tau-U"}, and \code{"Tau-BC"}. Set to \code{"all"} for
#'   all available effect sizes. Set to \code{"parametric"} for all parametric
#'   effect sizes. Set to \code{"NOM"} for all non-overlap measures. Defaults to
#'   calculating the LRRd, LRRi, SMD, and Tau indices.
#' @param improvement character string indicating direction of improvement.
#'   Default is "increase".
#' @param ... further arguments used for calculating some of the effect size
#'   indices.
#' @param confidence confidence level for the reported interval estimate. Set to
#'   \code{NULL} to omit confidence interval calculations.
#' @param format character string specifying whether to organize the results in
#'   \code{"long"} format or \code{"wide"} format. Defaults to \code{"long"}.
#'
#' @details Calculates one or more effect size indices
#'
#' @export
#'
#' @return A data.frame containing the estimate, standard error, and/or
#'   confidence interval for each specified effect size.
#'
#' @examples
#' # Using the A_data and B_data arguments
#' A <- c(20, 20, 26, 25, 22, 23)
#' B <- c(28, 25, 24, 27, 30, 30, 29)
#' calc_ES(A_data = A, B_data = B)
#'
#' # Using the condition and outcome arguments
#' phase <- c(rep("A", length(A)), rep("B", length(B)))
#' outcome <- c(A, B)
#' calc_ES(condition = phase, outcome = outcome, baseline_phase = "A")
#'
#' # Example from Parker & Vannest (2009)
#' yA <- c(4, 3, 4, 3, 4, 7, 5, 2, 3, 2)
#' yB <- c(5, 9, 7, 9, 7, 5, 9, 11, 11, 10, 9)
#' calc_ES(yA, yB)
#'
#' @importFrom rlang !!!
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom rlang exec
#' @importFrom dplyr .data
#' @importFrom dplyr vars
#'   


calc_ES <- function(A_data, B_data, 
                    condition, outcome, 
                    baseline_phase = NULL,
                    intervention_phase = NULL,
                    ES = c("LRRd","LRRi","SMD","Tau"), 
                    improvement = "increase", 
                    ..., 
                    confidence = .95,
                    format = "long") {
  
  if (missing(A_data) | missing(B_data)) {
    if (missing(outcome) | missing(condition)) stop("You must provide the data using the arguments 'A_data' and 'B_data' or 'condition' and 'outcome'.")
    if (length(outcome) != length(condition)) stop("The outcome vector must be the same length as the condition vector.")
    
    if (length(condition) == 0) return(data.frame())
    
    conditions <- as.character(unique(condition))
    if (length(conditions) < 2) stop("The 'condition' variable must have two or more unique values.")

    if (is.null(baseline_phase)) baseline_phase <- conditions[1]
    if (!is.character(baseline_phase)) baseline_phase <- as.character(baseline_phase)
    
    if (is.null(intervention_phase)) {
      intervention_phase <- setdiff(conditions, baseline_phase)[1]
      if (length(conditions) > 2) {
        msg <- paste0("The 'condition' variable has more than two unique values. Treating '", intervention_phase, "' as the intervention phase.")
        warning(msg)
      }
    }
    
    if (!is.character(intervention_phase)) intervention_phase <- as.character(intervention_phase)
    if (!(baseline_phase %in% conditions)) stop(paste0("The value of 'baseline_phase' must be one of the following: ", paste(paste0('"',conditions,'"'), collapse = ", "), "."))
    if (!(intervention_phase %in% conditions)) stop(paste0("The value of 'intervention_phase' must be one of the following: ", paste(paste0('"',conditions,'"'), collapse = ", "), "."))
    
    dat <- split(outcome, condition)
    
    A_data <- dat[[baseline_phase]]
    B_data <- dat[[intervention_phase]]
  } 
  
  if (length(improvement) > 1L) improvement <- names(sort(table(improvement), decreasing = TRUE)[1])
  
  if (!is.null(improvement) && !is.na(improvement)) {
    improvement <- tolower(improvement)
    
    improvement <- tryCatch(
      match.arg(improvement, c("increase", "decrease")),
      error = function(e) stop("The `improvement` argument must be a variable name or a string specifying 'increase' or 'decrease'.")
    ) 
  }
  
  A_data <- A_data[!is.na(A_data)]
  B_data <- B_data[!is.na(B_data)]
  
  ES_names <- get_ES_names(ES)

  # Allow for Tau-U variant names
  Tau_U_names <- c("Tau_U","Tau-U","TauU")
  if (any(Tau_U_names %in% ES)) ES_names <- union(setdiff(ES_names, Tau_U_names), "Tau_U") 
  
  Tau_BC_names <- c("Tau_BC", "Tau-BC", "TauBC")
  if (any(Tau_BC_names %in% ES)) ES_names <- union(setdiff(ES_names, Tau_BC_names), "Tau_BC")
  
  ES_to_calc <- paste0("calc_", ES_names)
  
  args <- list(A_data = A_data, B_data = B_data, improvement = improvement, confidence = confidence, ...)
  res <- purrr::map_dfr(ES_to_calc, function(fn) exec(fn, !!!args))
  
  if (format != "long") res <- convert_to_wide(res, ES_names)
  
  return(res)
}


#' @title Calculate effect sizes from a dataset for multiple series
#'
#' @description Calculates one or more effect size estimates, along with
#'   associated standard errors and confidence intervals, if available, for a
#'   single-case data series.
#' @param dat data frame containing SCD series for which effect sizes will be
#'   calculated.
#' @param grouping A variable name or list of (unquoted) variable names that
#'   uniquely identify each data series.
#' @param aggregate A variable name of list of (unquoted) variable names that
#'   identify additional grouping variables. Effect sizes will be calculated
#'   separately for each unique value of these variables, after which the effect
#'   size estimates will be averaged across values of these variables (but not
#'   across the values of the \code{grouping} variables).
#' @param weighting character string specifying the weighting scheme for use
#'   when variables are specified in \code{aggregate}. Available is
#'   \code{"1/V"}, \code{"equal"} (the default), \code{"Equal"}, \code{"nA"},
#'   \code{"n_A"}, \code{"nB"}, \code{"n_B"}, \code{"nAnB"}, \code{"nA*nB"},
#'   \code{"nA*nB"}, \code{"n_A*n_B"}, \code{"n_A * n_B"}, \code{"1/nA+1/nB"},
#'   \code{"1/nA + 1/nB"}, \code{"1/n_A+1/n_B"}, or \code{"1/n_A + 1/n_B"}. Note
#'   that \code{"1/V"} can only be used for effect sizes with known standard
#'   errors.
#' @param condition A variable name that identifies the treatment condition for
#'   each observation in the series.
#' @param outcome A variable name for the outcome data. Default is
#' @param session_number A variable name used to order the data within each
#'   series.
#' @param baseline_phase character string specifying which value of
#'   \code{condition} corresponds to the baseline phase. If \code{NULL} (the
#'   default), the first observed value of \code{condition} within the series
#'   will be used.
#' @param intervention_phase character string specifying which value of
#'   \code{condition} corresponds to the intervention phase. If \code{NULL} (the
#'   default), the second unique value of \code{condition} within the series
#'   will be used.
#' @param ES character string or character vector specifying which effect size
#'   index or indices to calculate. Available effect sizes are \code{"LRRd"},
#'   \code{"LRRi"}, \code{"LRM"}, \code{"LOR"}, \code{"SMD"}, \code{"NAP"},
#'   \code{"IRD"}, \code{"PND"}, \code{"PEM"}, \code{"PAND"}, \code{"PoGO"},
#'   \code{"Tau"}, \code{"Tau-U"}, and \code{"Tau-BC"}. Set to \code{"all"} for
#'   all available effect sizes. Set to \code{"parametric"} for all parametric
#'   effect sizes. Set to \code{"NOM"} for all non-overlap measures. Defaults to
#'   calculating the LRRd, LRRi, SMD, and Tau indices.
#' @param improvement character string either indicating the direction of
#'   uniform improvement ("increase" or "decrease") or the variable name of a
#'   variable identifying the direction of improvement for each series. Default
#'   is "increase".
#' @param scale character string indicating the common scale of the outcome
#'   variable across all of the series in the data set or the name of a variable
#'   within the dataset that identifies the outcome scale within each series.
#'   Possible values for the scale are \code{"percentage"} for a percentage with
#'   range 0-100, \code{"proportion"} for a proportion with range 0-1,
#'   \code{"count"} for a frequency count (0 or positive integers),
#'   \code{"rate"} for a standardized rate per minute. If a vector, the most
#'   frequent unique value will be used and missing values will be ignored.
#'   Defaults to \code{NA}.
#' @param intervals for interval recording procedures. Either the total number
#'   of intervals per observation session common to all series in the dataset,
#'   or the name of a variable within the dataset that identifies the number of
#'   intervals for each observation. If a variable name, the mean number of
#'   intervals within each series will be used. Missing values will be ignored.
#'   Defaults to \code{NA}.
#' @param observation_length Used for the log-response ratio. Either the common
#'   observation session length (in minutes) across all series in the dataset or
#'   a variable name containing the observation session length for each
#'   observation. If a variable name, the mean observation session length within
#'   each series will be used. Missing values will be ignored. Defaults to
#'   \code{NA}.
#' @param goal Used for the percent of goal obtained. Either a numeric common
#'   goal level across all series in the dataset or a variable name containing
#'   the goal level for each series. If a variable name, the mean goal level
#'   within each series will be used. Missing values will be ignored. Defaults
#'   to \code{NULL}.
#' @param warn logical indicating whether warnings should be displayed. Default
#'   is \code{TRUE}.
#' @inheritParams calc_ES
#'
#' @details Calculates one or more effect size indices for each series in a
#'   dataset
#'
#' @export
#'
#' @return A tibble containing the estimate, standard error, and/or confidence
#'   interval for each specified effect size.
#'
#' @examples
#'
#' data(McKissick)
#' batch_calc_ES(McKissick,
#'               grouping = Case_pseudonym,
#'               condition = Condition,
#'               outcome = Outcome,
#'               ES = c("LRRd","LRRi"),
#'               improvement = "decrease",
#'               scale = "count",
#'               observation_length = 20,
#'               format = "long")
#'
#' data(Schmidt2007)
#' batch_calc_ES(dat = Schmidt2007,
#'               grouping = c(Behavior_type, Case_pseudonym, Phase_num),
#'               condition = Condition,
#'               outcome = Outcome,
#'               ES = c("LRRi","LRRd"),
#'               improvement = direction,
#'               scale = Metric,
#'               bias_correct = TRUE,
#'               confidence = NULL,
#'               format = "wide")
#'
#' # Aggregate across phase-pairs
#'batch_calc_ES(dat = Schmidt2007,
#'            grouping = c(Behavior_type, Case_pseudonym),
#'            aggregate = Phase_num,
#'            weighting = "1/V",
#'            condition = Condition,
#'            outcome = Outcome,
#'            ES = c("LRRi", "LRRd", "SMD", "Tau"),
#'            improvement = direction,
#'            scale = "count",
#'            bias_correct = TRUE,
#'            confidence = NULL,
#'            format = "long")
#' 


batch_calc_ES <- function(dat, 
                          grouping, 
                          condition, 
                          outcome,
                          aggregate = NULL,
                          weighting = "equal",
                          session_number = NULL,
                          baseline_phase = NULL,
                          intervention_phase = NULL,
                          ES = c("LRRd","LRRi","SMD","Tau"), 
                          improvement = "increase",
                          scale = "other",
                          intervals = NA,
                          observation_length = NA,
                          goal = NULL,
                          confidence = .95,
                          format = "long",
                          warn = TRUE,
                          ...
) {
  
  grouping <- rlang::enquos(grouping)
  grouping <- tryCatch(tidyselect::vars_select(names(dat), !!!grouping), 
                       error = function(e) stop("The `grouping` variables are not in the dataset."))
  
  condition <- tryCatch(tidyselect::vars_pull(names(dat), !! rlang::enquo(condition)), 
                        error = function(e) stop("The `condition` variable is not in the dataset."))
  
  outcome <- tryCatch(tidyselect::vars_pull(names(dat), !! rlang::enquo(outcome)), 
                      error = function(e) stop("The `outcome` variable is not in the dataset."))
  
  if (tryCatch(!is.null(aggregate), error = function(e) TRUE)) {
    aggregate <- tryCatch(tidyselect::vars_select(names(dat), !!!rlang::enquos(aggregate)), 
                          error = function(e) stop("The `aggregate` variables are not in the dataset."))
  }
  
  weights_args <- c("1/V", "equal", "Equal", "nA", "n_A", "nB", "n_B", 
                    "nAnB", "nA*nB", "nA * nB", "n_A*n_B", "n_A * n_B", 
                    "1/nA+1/nB", "1/nA + 1/nB", "1/n_A+1/n_B", "1/n_A + 1/n_B")
  weighting <- tryCatch(match.arg(weighting, choices = weights_args),
                        error = function(e) stop(paste("The `weighting` argument must be one of the following:", paste0("'",weights_args,"'", collapse = ", "))))

  if (tryCatch(!is.null(session_number), error = function(e) TRUE)) {
    session_number <- tryCatch(tidyselect::vars_pull(names(dat), !! rlang::enquo(session_number)), 
                               error = function(e) stop("The `session_number` variable is not in the dataset."))
  }
  
  improvement_msg <- "The `improvement` argument must be a variable name or a string specifying 'increase' or 'decrease'."

  improvement_quo <- rlang::enquo(improvement)
  
  if (tryCatch(is.character(improvement), error = function(e) FALSE)) {
    improvement <- tolower(improvement)
    improvement <- tryCatch(
      match.arg(improvement, c("increase","decrease")),
      error = function(e) stop(improvement_msg)
    )
    dat$improvement <- improvement
    improvement <- "improvement"
  } else {
    improvement <- tryCatch(
      tidyselect::vars_pull(names(dat), !!improvement_quo), 
      error = function(e) stop(improvement_msg)
    )
  }
  
  scale_msg <- "The `scale` argument must be a variable name or one of the accepted scale types. See ?batch_calc_ES for more details."
  
  scale_quo <- rlang::enquo(scale)
  
  if (tryCatch(is.character(scale), error = function(e) FALSE)) {
    scale <- tryCatch(
      match.arg(tolower(scale), c("count", "rate", "proportion", "percentage", "other")),
      error = function(e) stop(scale_msg)
    )
    dat$scale <- scale
    scale <- "scale"
  } else {
    scale <- tryCatch(
      tidyselect::vars_pull(names(dat), !!scale_quo), 
      error = function(e) stop(scale_msg)
    )
  }
  
  intervals_quo <- rlang::enquo(intervals)
  
  if (tryCatch(typeof(intervals) %in% c("double", "integer") || is.na(intervals), 
               warning = function(w) FALSE,
               error = function(e) FALSE)) {
    dat$intervals <- intervals
    intervals <- "intervals"
  } else {
    intervals <- tryCatch(tidyselect::vars_pull(names(dat), !!intervals_quo), 
                          error = function(e) stop("The `intervals` variable is not in the dataset."))
  }
  
  observation_length_quo <- rlang::enquo(observation_length)
  
  if (tryCatch(typeof(observation_length) %in% c("double", "integer") || is.na(observation_length), 
               warning = function(w) FALSE,
               error = function (e) FALSE)) {
    dat$observation_length <- observation_length
    observation_length <- "observation_length"
  } else {
    observation_length <- tryCatch(tidyselect::vars_pull(names(dat), !!observation_length_quo), 
                                   error = function(e) stop("The `observation_length` variable is not in the dataset."))
  }
  
  if (warn & "LOR" %in% ES & !all(dat[[scale]] %in% c("proportion","percentage"))) {
    warning("LOR can only be calculated for proportions or percentages. Will return NAs for other outcome scales.", call. = FALSE)
  }
  
  ES_names <- get_ES_names(ES)
  
  if ("PoGO" %in% ES_names) {
    
    if (tryCatch(is.null(goal), error = function(e) FALSE)) {
      stop("You must provide the goal level of the behavior to calculate the PoGO effect size.")
    }
    if (tryCatch(typeof(goal) %in% c("double", "integer"), 
                 warning = function(w) FALSE,
                 error = function(e) FALSE)) {
      dat$goal <- goal
      goal <- "goal"
    } else {
      goal <- tryCatch(tidyselect::vars_select(names(dat), !!rlang::enquo(goal)), 
                       error = function(e) stop("The `goal` level is missing. Please either indicate a numerical common goal level or add a `goal` variable in the dataset."))
    } 
  }
  
  if (!is.null(session_number)) dat <- dplyr::arrange(dat, !!rlang::sym(session_number))
  
  ES_ests_long <-
    if (utils::packageVersion("dplyr") >= '1.1.0') {
      dat |>
      dplyr::group_by(!!!rlang::syms(c(grouping, aggregate))) |>
      dplyr::reframe(
        calc_ES(
          condition = .data[[condition]],
          outcome = .data[[outcome]],
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = ES_names,
          improvement = .data[[improvement]],
          scale =  .data[[scale]],
          intervals = .data[[intervals]],
          observation_length = .data[[observation_length]],
          goal = if ("PoGO" %in% ES_names) .data[[goal]] else NULL,
          confidence = confidence,
          format = "long",
          ...,
          warn = warn
        )
      ) 
    } else {
      dat |>
      dplyr::group_by(!!!rlang::syms(c(grouping, aggregate))) |>
      dplyr::summarise(
        calc_ES(
          condition = .data[[condition]],
          outcome = .data[[outcome]],
          baseline_phase = baseline_phase,
          intervention_phase = intervention_phase,
          ES = ES_names,
          improvement = .data[[improvement]],
          scale =  .data[[scale]],
          intervals = .data[[intervals]],
          observation_length = .data[[observation_length]],
          goal = if ("PoGO" %in% ES_names) .data[[goal]] else NULL,
          confidence = confidence,
          format = "long",
          ...,
          warn = warn
        ),
        .groups = "drop"
      )
    }
  
  ES_long_names <- names(ES_ests_long)
  
  # Aggregation
  
  if (is.null(aggregate)) {
    
    res <- ES_ests_long
    
  } else {
    
    # determine unique condition labels
    
    conditions <- dat |> dplyr::pull({{condition}}) |> unique()
    
    if (is.null(baseline_phase)) baseline_phase <- conditions[1]
    
    if (is.null(intervention_phase)) {
      intervention_phase <- setdiff(conditions, baseline_phase)[1]
      if (length(conditions) > 2) {
        msg <- paste0("The 'condition' variable has more than two unique values. Treating '", intervention_phase, "' as the intervention phase.")
        warning(msg)
      }
    }

    # calculate weights
    
    if (weighting %in% c("1/V")) {
      if (!("SE" %in% ES_long_names)) {
        stop("Using `weighting = '1/V'` will not work because none of the chosen effect sizes have known standard errors. Consider specifying 'weighting' as 'equal' (the default), 'nA', 'nB', 'nA*nB', or '1/nA+1/nB' instead.")
      }
      
      ES_weights <- 
        ES_ests_long |>
        dplyr::mutate(
          weights = 1 / SE^2
        )
      
    } else if (weighting %in% c("equal","Equal")) {
      
      ES_weights <- 
        ES_ests_long |>
        dplyr::mutate(
          weights = 1L
        )
      
    } else if (weighting %in% weights_args[-(1:3)]) {
      
      n_weights <- 
        dat |> 
        dplyr::filter(!!rlang::sym(condition) %in% c(baseline_phase, intervention_phase)) |> 
        dplyr::mutate(!!rlang::sym(condition) := ifelse(!!rlang::sym(condition) == baseline_phase, "A", "B")) |> 
        dplyr::group_by(!!!rlang::syms(c(grouping, aggregate, condition))) |> 
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |> 
        tidyr::pivot_wider(names_from = !!rlang::sym(condition), values_from = n) |>
        dplyr::ungroup() |> 
        dplyr::mutate(
          weights = dplyr::case_when(
            weighting %in% c("nA", "n_A") ~ as.numeric(A),
            weighting %in% c("nB", "n_B") ~ as.numeric(B),
            weighting %in% c("nAnB", "nA*nB", "nA * nB", "n_A*n_B", "n_A * n_B") ~ as.numeric(A*B),
            weighting %in% c("1/nA+1/nB", "1/nA + 1/nB", "1/n_A+1/n_B", "1/n_A + 1/n_B") ~ as.numeric(1/A + 1/B)
          )
        ) |> 
        dplyr::select(-c(A,B))
      
      ES_weights <- 
        ES_ests_long |> 
        dplyr::left_join(n_weights, by = c(grouping, aggregate))
      
    }
    
    # Aggregate
    
    if ("SE" %in% ES_long_names) {
      
      res <- 
        ES_weights |> 
        dplyr::group_by(!!!rlang::syms(grouping), ES) |>
        dplyr::summarise(
          Est = sum(Est * weights) / sum(weights),
          SE = sqrt(sum(weights^2 * SE^2) / (sum(weights)^2)),
          .groups = "drop"
        )
      
      if (!is.null(confidence)) {
        res <-
          res |>
          dplyr::mutate(
            CI_lower = Est - qnorm(1 - (1 - confidence) / 2) * SE,
            CI_upper = Est + qnorm(1 - (1 - confidence) / 2) * SE
          )
      } 
      
    } else {
      
      res <- 
        ES_weights |> 
        dplyr::group_by(!!!rlang::syms(grouping), ES) |>
        dplyr::summarise(
          Est = sum(Est * weights) / sum(weights),
          .groups = "drop"
        )
      
    }
  }
  
  # Wide or long format
  
  if (format == "long") {
    result <- res
  } else {
    result <- convert_to_wide(res, ES_names)
  }
  
  return(result)
  
}

