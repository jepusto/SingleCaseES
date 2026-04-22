library(markdown, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(SingleCaseES, warn.conflicts = FALSE, quietly = TRUE)
library(rlang, warn.conflicts = FALSE, quietly = TRUE)
library(Kendall, warn.conflicts = FALSE, quietly = TRUE)

source("mappings.R")
source("helper-functions.R", local = TRUE)

statistical_indices <- c("NAP", "Tau", "Tau_BC", "SMD", "LRRi", "LRRd", "LOR", "LRM", "PoGO")

full_names <- list(IRD = "Robust Improvement Rate Difference",
                   NAP = "Non-overlap of All Pairs",
                   PAND = "Percentage of All Non-overlapping Data",
                   PEM = "Percent Exceeding the Median",
                   PND = "Percentage of Non-overlapping Data",
                   Tau = "Tau",
                   Tau_BC = "Tau-BC",
                   Tau_U = "Tau-U",
                   LOR = "Log Odds Ratio",
                   LRRd = "Log Response Ratio (decreasing)",
                   LRRi = "Log Response Ratio (increasing)",
                   LRM = "Log Ratio of Medians",
                   PoGO = "Percent of Goal Obtained",
                   SMD = "Standardized Mean Difference (within-case)")


server <- function(input, output, session) {
  
  f <- function(x) {
    y <- as.numeric(unlist(strsplit(x, split = "\\s|\\t|\\n|,")))
    y[!is.na(y)]
  }
  
  dat <- reactive({ 
    dat_strings <- list(A = input$A_dat, B = input$B_dat)
    dat <- lapply(dat_strings, f)
    dat$compute <- all(sapply(dat, function(x) sum(!is.na(x)) >= 3))
    dat
  })
  
  output$SCDplot <- renderPlot({
    
    if (input$toggleSinglePlot & dat()$compute) {
      A_dat <- dat()$A
      B_dat <- dat()$B
      m <- length(A_dat)
      n <- length(B_dat)
      dat <- data.frame(session = 1:(m + n),
                        phase = c(rep("A",m), rep("B",n)),
                        outcome = c(A_dat, B_dat))
      ggplot(dat, aes(session, outcome, color = phase)) + 
        geom_point(size = 2) + geom_line() + 
        geom_vline(xintercept = m + 0.5, linetype = "dashed") + 
        scale_color_brewer(type = "qual", palette = 2) + 
        theme_minimal() + theme(legend.position = "bottom")
    } else {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = single_series_min_data_points_msg), size = 6) + 
        theme_void()
    }
  })
  
  ES <- reactive({
    index <- c("Non-overlap" = input$NOM_ES, "Parametric" = input$parametric_ES)[[input$ES_family]]
    
    if (input$tau_calculation == "Kendall") {
      Kendall <- TRUE
    } else {
      Kendall <- FALSE
    }
    
    if (input$baseline_check == "No") {
      pretest_trend <- FALSE
    } else {
      pretest_trend <- input$significance_level
    }
    
    arg_vals <- list(A_data = dat()$A, 
                     B_data = dat()$B,
                     ES = index,
                     improvement = input$improvement,
                     std_dev = substr(input$SMD_denom, 1, nchar(input$SMD_denom) - 3),
                     confidence = (input$confidence/100),
                     pct_change = input$pct_change,
                     scale = input$outScale,
                     observation_length = input$obslength,
                     intervals = input$intervals,
                     goal = input$goal_level,
                     D_const = input$lrrfloor,
                     # options for Tau_BC
                     Kendall = Kendall,
                     pretest_trend = pretest_trend,
                     warn = FALSE, 
                     report_correction = TRUE
    )
    
    est <- tryCatch(do.call(calc_ES, arg_vals), warning = function(w) w, error = function(e) e)
    
    if (index %in% c("LRRi", "LRRd", "LOR")) {
      validate(
        need(all(c(dat()$A, dat()$B) >= 0), message = "For the log response or log odds ratio, all data must be greater than or equal to zero. ")
      )
    }
    
    if (index == "LRM") {
      validate(
        need(all(c(median(dat()$A), median(dat()$B)) > 0), message = "For the log ratio of medians, medians of phase A or B must be greater than zero. ")
      )
    }
    
    if (input$ES_family == "Parametric" & input$parametric_ES %in% c("LOR", "LRRi", "LRRd") & input$outScale == "percentage") {
      validate(
        need(all(c(dat()$A, dat()$B) >= 0) & all(c(dat()$A, dat()$B) <= 100), message =  "For percentage scale, values must be between 0 and 100.")
      )
    }
    
    if (input$ES_family == "Parametric" & input$parametric_ES %in% c("LOR", "LRRi", "LRRd") & input$outScale == "proportion") {
      validate(
        need(all(c(dat()$A, dat()$B) >= 0) & all(c(dat()$A, dat()$B) <= 1), message = "For proportion scale, values must be between 0 and 1.")
      )
    }
    
    if (index %in% c("LOR")) {
      validate(
        need(input$outScale %in% c("proportion", "percentage"), message = "For the log odds ratio, only proportion or percentage scale data is accepted.")
      )
    }
    
    list(index = index, est = est)
  })
  
  output$ES_name <- renderText({
    index <- c("Non-overlap" = input$NOM_ES, "Parametric" = input$parametric_ES)[[input$ES_family]]
    full_names[[index]] 
  })
  
  output$result <- renderText({
    index <- ES()$index[1]
    
    if (dat()$compute) {
      fmt <- function(x) formatC(x, digits = input$digits, format = "f")
      pct <- function(x) formatC(x, digits = input$digits - 2, format = "f")
      est <- ES()$est
      
      if (ES()$index %in% statistical_indices) {
        
        Est_txt <- paste("Effect size estimate:", fmt(est$Est[1]))
        SE_txt <- paste("Standard error:", fmt(est$SE[1]))
        CI_txt <- paste0(input$confidence,"% CI: [", fmt(est$CI_lower[1]), ", ", fmt(est$CI_upper[1]), "]")
        
        note_SMD_no_variation <- NULL
        pct_txt <- NULL
        
        if (ES()$index == "SMD" & input$SMD_denom == "baseline SD") {
          if (sd(dat()$A) == 0) {
            Est_txt <- SE_txt <- CI_txt <- SD_txt <- NULL
            note_SMD_no_variation <- strong(style="color:red", "The baseline SD is zero. Please try the pooled SD or use other effect size metrics.")
          } else {
            SD_txt <- paste("Baseline SD:", fmt(est$`baseline_SD`[1]))
          }
          
        } else if (ES()$index == "SMD" & input$SMD_denom == "pooled SD") {
          if (all(c(sd(dat()$A), sd(dat()$B)) == 0)) {
            Est_txt <- SE_txt <- CI_txt <- SD_txt <- NULL
            note_SMD_no_variation <- strong(style ="color:red", "The pooled SD is zero. The SMD is not an appropriate effect size metric for these data.")
          } else {
            SD_txt <- paste("Pooled SD:", fmt(est$`pooled_SD`[1]))
          }
          
        } else {
          SD_txt <- NULL
          
          if (nrow(est) > 1) {
            pct_est <- paste("<br/>Percentage change:", pct(est$Est[2]))
            pct_CI <- paste0(input$confidence,"% CI: [", pct(est$CI_lower[2]), ", ", pct(est$CI_upper[2]), "]")
            pct_txt <- paste(pct_est, pct_CI, sep = "<br/>")
          } 
        }
        
        note_txt_TauBC <- NULL
        if (ES()$index == "Tau_BC" & input$baseline_check == "Yes") {
          
          if (ES()$est$pval_slope_A > input$significance_level) {
            if (input$tau_calculation == "Kendall") {
              note_txt_TauBC <- strong(style="color:red", "The baseline trend is not statistically significant. Kendall rank correlation is calculated without trend correction.")
            } else if (input$tau_calculation == "Nlap") {
              note_txt_TauBC <- strong(style="color:red", "The baseline trend is not statistically significant. Tau (non-overlap) is calculated without trend correction.")
            }
          }
          
        }
        
        note_txt <- "<br/>Note: SE and CI are based on the assumption that measurements are mutually independent (i.e., not auto-correlated)." 
        
        HTML(paste(Est_txt, SE_txt, CI_txt, SD_txt, pct_txt, note_txt_TauBC, note_SMD_no_variation, note_txt, sep = "<br/>"))
        
      } else {
        Est_txt <- paste("Effect size estimate:", fmt(est$Est))
        HTML(Est_txt)
      }
    } else {
      HTML(single_series_min_data_points_msg)
    }
  })
  
  sheetname <- reactive({
    if (input$dat_type == "xlsx") {
      inFile <- input$xlsx
      if (is.null(inFile)) return(NULL)
      sheetnames <- readxl::excel_sheets(inFile$datapath)
    }
  })
  
  observe({
    sheets <- sheetname()
    updateSelectInput(session, "inSelect", label = "Select a sheet",
                      choices = sheets,
                      selected = sheets[1])
  })
  
  datFile <- reactive({
    
    if (input$dat_type == "example") {
      dat <- get(input$example)
      
      dat
      
    } else if (input$dat_type == "dat") {
      
      inFile <- input$dat
      
      if (is.null(inFile)) return(NULL)
      
      read.table(inFile$datapath, header=input$header,
                 sep=input$sep, quote=input$quote,
                 stringsAsFactors = FALSE, check.names = FALSE) |>
        janitor::clean_names(case = "parsed")
      
    } else if (input$dat_type == "xlsx") {
      
      inFile <- input$xlsx
      
      if (is.null(inFile) || is.null(input$inSelect) || nchar(input$inSelect) == 0) return(NULL)
      
      readxl::read_xlsx(inFile$datapath, col_names = input$col_names, sheet = input$inSelect) |>
        janitor::clean_names(case = "parsed") |>
        as.data.frame()
      
    }
  })
  
  
  output$filtervarMapping <- renderUI({
    
    var_names <- names(datFile())
    list(
      selectizeInput("filters", label = "Filtering variables", choices = var_names, selected = NULL, multiple = TRUE)
    )
    
  })
  
  output$filterMapping <- renderUI({
    
    filter_vars <- input$filters  
    filter_vals <- lapply(filter_vars, function(x) levels(as.factor(datFile()[,x])))
    names(filter_vals) <- filter_vars
    header <- strong("Please select values for each filtering variable.")
    
    filter_selects <- lapply(filter_vars, function(x) 
      selectizeInput(paste0("filter_",x), label = x, choices = filter_vals[[x]], 
                     selected = filter_vals[[x]][1], multiple = TRUE))
    
    if (length(filter_vars) > 0) {
      filter_selects <- list(header, column(12, br()), filter_selects)
    }
    
    filter_selects
    
  })
  
  datClean <- reactive({
    
    if (input$dat_type == "example") {
      data(list = input$example)
      dat <- get(input$example)
    } else {
      dat <- datFile()
    }
    
    if (!is.null(input$filters)) {
      subset_vals <- sapply(input$filters, function(x) datFile()[[x]] %in% input[[paste0("filter_",x)]])
      dat <- dat[apply(subset_vals, 1, all),]
      dat <- droplevels(dat)
    }
    
    return(dat)
    
  })
  
  output$datview <- renderTable(datClean())
  
  
  # variables tab
  output$clusterPhase <- renderUI({
    
    var_names <- names(datClean())
    if (input$dat_type == "dat" || input$dat_type == "xlsx") {
      
      b_clu_agg_choices <- if (input$calcPhasePair) {
        c(var_names, "phase_pair_calculated")
      } else {
        var_names
      }
      
      list(
        selectizeInput("b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", 
                       choices = b_clu_agg_choices, selected = NULL, multiple = TRUE),
        selectizeInput("b_aggregate", label = "Select all variables to average across after calculating effect size estimates.", 
                       choices = b_clu_agg_choices, selected = NULL, multiple = TRUE),
        selectInput("b_phase", label = "Phase indicator", choices = var_names, selected = var_names[3])
      )
      
    } else {
      
      curMap <- exampleMapping[[input$example]]
      
      list(
        selectizeInput("b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", 
                       choices = var_names, 
                       selected = curMap$cluster_vars, multiple = TRUE),
        selectizeInput("b_aggregate", label = "Select all variables to average across after calculating effect size estimates.", 
                       choices = var_names,
                       selected = curMap$aggregate_vars, multiple = TRUE),
        selectInput("b_phase", label = "Phase indicator", choices = var_names, selected = curMap$condition)
      )
      
    }
  })
  
  
  output$baseDefine <- renderUI({
    
    phase_choices <- if (!is.null(input$b_phase)) unique(datClean()[[input$b_phase]]) else c("A","B")
    
    if (input$dat_type == "dat" || input$dat_type == "xlsx") {
      selectInput("b_base", label = "Baseline phase value", choices = phase_choices)
    } else {
      curMap <- exampleMapping[[input$example]]
      selectInput("b_base", label = "Baseline phase value", choices = phase_choices, selected = curMap$phase_vals[1])
    }
  })
  
  output$treatDefine <- renderUI({
    
    phase_choices <- if (!is.null(input$b_phase)) unique(datClean()[[input$b_phase]]) else c("A","B")
    trt_choices <- setdiff(phase_choices, input$b_base)
    
    if (input$dat_type == "dat" || input$dat_type == "xlsx") {
      selectInput("b_treat", label = "Treatment phase value", choices = trt_choices)
    } else {
      curMap <- exampleMapping[[input$example]]
      selectInput("b_treat", label = "Treatment phase value", choices = trt_choices, selected = curMap$phase_vals[2])  
    }
  })
  
  
  output$outOrderImp <- renderUI({
    
    var_names <- names(datClean())
    
    if (input$dat_type == "dat" || input$dat_type == "xlsx") {
      list(
        selectInput("session_number", label = "Session number", choices = var_names, selected = var_names[5]),
        selectInput("b_out", label = "Outcome", choices = var_names, selected = var_names[4]),
        selectInput("bimprovement", label = "Direction of improvement", choices = c("all increase" = "increase", "all decrease" = "decrease", "by series" = "series"))
      )
    } else {
      curMap <- exampleMapping[[input$example]]
      list(
        selectInput("session_number", label = "Session number", choices = var_names, selected = curMap$session_num),
        selectInput("b_out", label = "Outcome", choices = var_names, selected = curMap$outcome),
        selectInput("bimprovement", label = "Direction of improvement", choices = c("all increase" = "increase", "all decrease" = "decrease", "by series" = "series"),
                    selected = curMap$direction)
      )
    }
    
  })
  
  output$improvementVar <- renderUI({
    var_names <- names(datClean())
    
    if (input$dat_type == "dat" || input$dat_type == "xlsx") {
      list(selectInput("bseldir", label = "Select variable identifying improvement direction", choices = var_names, selected = var_names[5]))
    } else {
      curMap <- exampleMapping[[input$example]]
      list(selectInput("bseldir", label = "Select variable identifying improvement direction", choices = var_names, selected = curMap$direction_var))
    }
  })
  
  # Measurement details taken out of the following function - might be useful later
  # selectInput("bmeasurementProcedure", label = "Measurement Procedure",
  #                  choices = c("all continuous recording", "all interval recording", "all event counting", "all other", "by series" = "series")),
  #      conditionalPanel(condition = "input.bmeasurementProcedure == 'series'",
  #                       selectInput("bmeasurementvar", "Select variable identifying measurement procedure",
  #                                   choices = var_names)),
  #      conditionalPanel(condition = "input.bmeasurementProcedure == 'series' | input.bmeasurementProcedure == 'all continuous recording'",
  #                       textInput("bfloor", label = "Optional floor for log-prevalence odds-ratio", value = NA)),
  
  output$measurementProc <- renderUI({
    
    var_names <- names(datClean())
    
    if (input$dat_type == "dat" || input$dat_type == "xlsx") {  
      
      list(
        selectInput("boutScale", label = "Outcome Scale",
                    choices = c("all percentage" = "percentage", "all proportion" = "proportion", "all count" = "count", "all rate" = "rate", "all other" = "other", "by series" = "series")),
        conditionalPanel(condition = "input.boutScale == 'series'",
                         selectInput("bscalevar", "Select variable identifying outcome scale",
                                     choices = var_names)),
        selectInput("bintervals", label = "Optionally, a variable identifying the number of intervals per observation session.",
                    choices = c(NA, var_names), selected = NA),
        selectInput("bobslength", label = "Optionally, a variable identifying the length of each observation session.",
                    choices = c(NA, var_names), selected = NA),
        numericInput("blrrfloor", label = "Optionally, provide a floor for the log-response or log-odds ratio? Must be greater than or equal to 0.", 
                     value = NA, min = 0)
      )
      
    } else {
      
      curMap <- exampleMapping[[input$example]]
      list(
        selectInput("boutScale", label = "Outcome Scale",
                    choices = c("all percentage" = "percentage", "all proportion" = "proportion", "all count" = "count", "all rate" = "rate", "all other" = "other", "by series" = "series"),
                    selected = curMap$scale),
        conditionalPanel(condition = "input.boutScale == 'series'",
                         selectInput("bscalevar", "Select variable identifying outcome scale",
                                     choices = var_names,
                                     selected = (if(!is.null(curMap$scale_var)){curMap$scale_var}else{NA}))),
        selectInput("bintervals", label = "Optionally, a variable identifying the number of intervals per observation session.",
                    choices = c(NA, var_names), selected = curMap$intervals),
        selectInput("bobslength", label = "Optionally, a variable identifying the length of each observation session.",
                    choices = c(NA, var_names), selected = curMap$observation_length),
        numericInput("blrrfloor", label = "Optionally, provide a floor for the log-response or log-odds ratio? Must be greater than or equal to 0.", 
                     value = NA, min = 0)
      )
      
    }
  })
  
  output$goalLevel <- renderUI({
    
    var_names <- names(datClean())
    
    if (input$dat_type == "dat" || input$dat_type == "xlsx") {  
      
      list(
        selectInput("bgoalLevel", "Set the goal level for PoGO.",
                    choices = c("common goal" = "commonGoal", "different goals across series" = "goals")),
        conditionalPanel(condition = "input.bgoalLevel == 'commonGoal'",
                         numericInput("bcomgoal", label = "Provide a common goal level across series.",
                                      value = NULL)),
        conditionalPanel(condition = "input.bgoalLevel == 'goals'",
                         selectInput("bgoalvar", "Select variable identifying goal level",
                                     choices = var_names))
      )
      
    } else {
      
      numericInput("bcomgoal", label = "Provide a common goal level across series.", value = NULL)
      
    }
    
  })
  
  
  datClean2 <- reactive({
    dat <- datClean()
    
    if (input$calcPhasePair) {
      grouping_vars <- setdiff(input$b_clusters, "phase_pair_calculated")
      session_var <- input$session_number
      phase_var <- input$b_phase
      
      dat <- 
        dat |> 
        dplyr::group_by(!!!rlang::syms(grouping_vars)) |> 
        dplyr::mutate(
          phase_pair_calculated = calc_phase_pairs(!!rlang::sym(phase_var), session = !!rlang::sym(session_var))
        ) |> 
        dplyr::ungroup() |> 
        as.data.frame() 
    }
    
    return(dat)
    
  })
  
  output$datview2 <- renderTable(datClean2())
  
  # warning message for outcome scale types
  
  output$outcomeScale <- renderUI({
    
    req(input$boutScale)
    
    if (input$boutScale == "series") {
      scales <- c("count", "rate", "proportion", "percentage", "other")
      scale_type <- tolower(unique(datClean2()[[input$bscalevar]]))
      wrong_scale_type <- setdiff(scale_type, scales)
      
      if (length(wrong_scale_type) > 0) {
        warning <- paste0("The scale variable contains non-acceptable types: ", paste(wrong_scale_type, collapse = ", "), 
                          ". The acceptable scale types are: count, rate, proportion, percentage, or other.")
        list(
          strong(style="color:red", warning),
          br("")
        )
      } else {
        NULL
      }
    }
    
  })
  
  # warning message for improvement direction
  
  output$improvementDir <- renderUI({
    
    req(input$bimprovement)
    req(input$bseldir)
    
    if (input$bimprovement == "series") {
      valence_allowed <- c("increase", "decrease")
      valence_input <- tolower(unique(datClean2()[[input$bseldir]]))
      wrong_valence <- setdiff(valence_input, valence_allowed)
      
      if (length(wrong_valence) > 0) {
        warning <- paste0("The improvement direction variable contains non-acceptable types: ", paste(wrong_valence, collapse = ", "), 
                          ". The acceptable improvement directions are: increase or decrease.")
        list(
          strong(style="color:red", warning),
          br("")
        )
      } else {
        NULL
      }
    }
    
  })
  
  # Plot
  
  output$facetSelector <- renderUI({
    grouping_vars <- input$b_clusters
    aggregating_vars <- input$b_aggregate
    facet_vars <- c("None", grouping_vars, aggregating_vars)
    selectizeInput("bfacetSelector", label = "Display plots for each value of this variable.", 
                   choices = facet_vars, selected = NULL, multiple = FALSE)
  })
  
  grouping_vars <- reactive({
    setdiff(c(input$b_clusters, input$b_aggregate), input$bfacetSelector)
  })
  
  output$graph_filters <- renderUI({
    
    if (length(grouping_vars()) > 0) {
      
      grouping_vals <- lapply(grouping_vars(), function(x) sort(unique(datClean2()[[x]])))
      names(grouping_vals) <- grouping_vars()
      
      header <- strong("Select a value for each grouping variable.")
      
      grouping_selects <- lapply(grouping_vars(), function(x) 
        selectizeInput(paste("grouping",x, sep = "_"), label = x, choices = grouping_vals[[x]], 
                       selected = grouping_vals[[x]][1], multiple = FALSE))
      
      grouping_selects <- list(header, column(12, br()), grouping_selects)
      
      grouping_selects
      
    }
    
  })
  
  # vector of selected filter variables
  grouping_selected <- reactive({
    if (length(grouping_vars()) > 0) {
      lapply(grouping_vars(), \(x) input[[paste("grouping", x, sep = "_")]])
    }
  })
  
  # update the filter variable inputs
  observeEvent(grouping_selected(), {
    n_grouping <- length(grouping_vars())
    if (n_grouping > 0) {
      dat <- datClean2()
      for (i in 1:n_grouping) {
        grp_var <- grouping_vars()[i]
        inputId <- paste("grouping", grp_var, sep = "_")
        grp_val <- grouping_selected()[[i]]
        grouping_vals <- sort(unique(dat[[grp_var]]))
        selected_val <- if (!is.null(grp_val) && grp_val %in% grouping_vals) grp_val else grouping_vals[1]
        subset_vals <- dat[[grp_var]] == selected_val
        dat <- dat[subset_vals,]
        if (inputId %in% names(input)) {
          updateSelectizeInput(session, inputId = inputId, choices = grouping_vals, selected = selected_val)
        }
      }
    }
  })
  
  datGraph <- reactive({
    
    dat <- datClean2()
    
    if (!is.null(input$b_clusters) | !is.null(input$b_aggregate)) {
      
      if (length(grouping_vars()) > 0) {
        subset_vals <- sapply(grouping_vars(), function(x) datClean2()[[x]] %in% input[[paste0("grouping_",x)]])
        dat <- dat[apply(subset_vals, 1, all),]
      } 
      
    }
    
    return(dat)
    
  })
  
  
  heightPlot <- reactive({
    if (is.null(input$bfacetSelector) || input$bfacetSelector == "None") {
      height <- 300
    } else {
      height <- 180 * (length(unique(datGraph()[[input$bfacetSelector]])))
    }
  })
  
  output$batchPlot <- renderPlot({
    
    dat <- datGraph()
    session_dat <- dat[[input$session_number]]
    outcome_dat <- dat[[input$b_out]]
    phase_dat <- dat[[input$b_phase]]
    phase_code <- if (!is.null(input$b_phase)) unique(phase_dat) else c("A","B")
    
    if (is.null(input$bfacetSelector) || input$bfacetSelector == "None"){
      
      dat_graph <-
        data.frame(session = session_dat, outcome = outcome_dat, phase = as.factor(phase_dat)) |>
        dplyr::filter(phase %in% c(input$b_base, input$b_treat))
      
      phase_change <-
        dat_graph |>
        dplyr::filter(phase == phase_code[2]) |>
        dplyr::mutate(treat_change = suppressWarnings(min(session)) - 0.5) |>
        dplyr::select(treat_change) |>
        unique()
      
    } else {
      
      facet_dat <- dat[[input$bfacetSelector]]
      
      dat_graph <-
        data.frame(facet = facet_dat, session = session_dat, outcome = outcome_dat, phase = as.factor(phase_dat)) |>
        dplyr::filter(phase %in% c(input$b_base, input$b_treat))
      
      phase_change <-
        dat_graph |>
        group_by(facet) |>
        dplyr::filter(phase == phase_code[2]) |>
        dplyr::mutate(treat_change = suppressWarnings(min(session)) - 0.5) |>
        dplyr::select(facet, treat_change) |>
        unique()
      
    }
    
    ggplot(dat_graph, aes(session, outcome, color = phase)) +
      geom_point(size = 2) + geom_line() +
      {if ("facet" %in% names(dat_graph)) facet_grid(facet ~ .)} +
      geom_vline(data = phase_change, aes(xintercept = treat_change), linetype = "dashed") +
      scale_color_brewer(type = "qual", palette = 2) +
      theme_bw() + theme(legend.position = "bottom")
    
  }, height = function() heightPlot(), width = function() 700)
  
  
  batchModel <- eventReactive(input$batchest, {
    
    if (any(input$bESpar %in% c("LRRi", "LRRd", "LOR"))) {
      if (input$boutScale == "series") {
        scale_val <- as.symbol(input$bscalevar)
      } else{
        scale_val <- input$boutScale
      }
      
      intervals <- if (input$bintervals == "NA") NA else input$bintervals
      obslength <- if (input$bobslength == "NA") NA else input$bobslength
      D_const <- if (is.null(input$blrrfloor)) NA else input$blrrfloor
      
    } else {
      scale_val <- "other"
      intervals <- obslength <- D_const <- NA
    }
    
    
    if ("PoGO" %in% input$bESpar) {
      
      if (input$dat_type %in% c("dat", "xlsx")) {
        if (input$bgoalLevel == "goals") goal_val <- as.symbol(input$bgoalvar) else goal_val <- input$bcomgoal
      } else {
        goal_val <- input$bcomgoal
      }
      
    } else {
      goal_val <- NULL
    }
    
    
    if(input$bimprovement == "series") {
      improvement <- as.symbol(input$bseldir)
    } else {
      improvement <- input$bimprovement
    }
    
    if (input$btau_calculation == "Kendall") {
      Kendall <- TRUE
    } else {
      Kendall <- FALSE
    }
    
    if (input$bbaseline_check == "No") {
      pretest_trend <- FALSE
    } else {
      pretest_trend <- input$bsignificance_level
    }
    
    if (is.null(input$b_aggregate)) {
      
      batch_calc_ES(dat = datClean2(),
                    grouping = input$b_clusters,
                    condition = input$b_phase,
                    outcome = input$b_out,
                    session_number = input$session_number,
                    baseline_phase = input$b_base,
                    intervention_phase = input$b_treat,
                    ES = c(input$bESno, input$bESpar),
                    improvement = improvement,
                    pct_change = input$b_pct_change,
                    scale = scale_val,
                    intervals = intervals,
                    observation_length = obslength,
                    D_const = D_const,
                    goal = goal_val,
                    std_dev = input$bSMD_denom,
                    confidence = input$bconfidence / 100,
                    Kendall = Kendall, 
                    pretest_trend = pretest_trend, 
                    warn = FALSE, 
                    format = input$resultsformat)
      
    } else{
      
      batch_calc_ES(dat = datClean2(),
                    grouping = input$b_clusters,
                    condition = input$b_phase,
                    outcome = input$b_out,
                    aggregate = input$b_aggregate,
                    weighting = input$weighting_scheme,
                    session_number = input$session_number,
                    baseline_phase = input$b_base,
                    intervention_phase = input$b_treat,
                    ES = c(input$bESno, input$bESpar),
                    improvement = improvement,
                    pct_change = input$b_pct_change,
                    scale = scale_val,
                    intervals = intervals,
                    observation_length = obslength,
                    D_const = D_const,
                    goal = goal_val,
                    std_dev = input$bSMD_denom,
                    confidence = input$bconfidence / 100,
                    Kendall = Kendall, 
                    pretest_trend = pretest_trend, 
                    warn = FALSE,
                    format = input$resultsformat)
      
    }
    
    
    
    
  })
  
  output$batchTable <- renderTable(batchModel(), na = "-", digits = function() input$bdigits)
  
  output$downloadES <- downloadHandler(
    filename = "SCD effect size estimates.csv",
    content = function(file) {
      dat <- batchModel()
      write.csv(dat, file, row.names=FALSE, na = "")
    },
    contentType = "text/csv"
  )
  
  #------------------------------
  # Syntax for replication in R
  #------------------------------
  
  batch_syntax <- reactive({
    header_res <- c(
      '# Load packages',
      'library(SingleCaseES)',
      ''
    )
    
    # read in data
    if (input$dat_type == "example") {
      read_res <- c(
        parse_code_chunk("load-example", args = list(example_name = input$example)),
        ''
      )
    } else if (input$dat_type == "dat") {
      inFile <- input$dat
      read_res <- c(
        parse_code_chunk("load-data", args = list(user_path = inFile$name, user_header = input$header,
                                                  user_sep = input$sep, user_quote = input$quote)),
        ''
      )
    } else if (input$dat_type == "xlsx") {
      inFile <- input$xlsx
      read_res <- c(
        parse_code_chunk("load-excel", args = list(user_path = inFile$name, user_sheet = input$inSelect)),
        ''
      )
    }
    
    # filter the data
    
    filter_vars <- input$filters
    filter_vals <- if(length(filter_vars) > 0) lapply(paste0("filter_", filter_vars), 
                                                      function(x) paste0('"', input[[x]], '"', collapse = ",")) else NULL
    filter_vals <- paste0("%in% c(", filter_vals, ")")
    filter_string <- paste(input$filters, filter_vals, collapse = " & ")
    
    if (!is.null(input$filters)) {
      clean_dat <- c(
        parse_code_chunk("dat-filter", args = list(user_filterString = filter_string)),
        ''
      )
    } else {
      clean_dat <- c()
    } 
    
    # clean the data
    if (input$calcPhasePair) {
      grouping_vars <- setdiff(input$b_clusters, "phase_pair_calculated")
      grouping <- paste(grouping_vars, collapse=', ')
      
      condition <- input$b_phase
      session_number <- input$session_number
      
      clean_dat <- c(clean_dat,
                     parse_code_chunk("dat-clean", 
                                      args = list(user_grouping = grouping,
                                                  user_condition = condition,
                                                  user_session_number = session_number)),
                     ''
      )
    }
    
    
    # batch calculation
    if (any(input$bESpar %in% c("LRRi", "LRRd", "LOR"))) {
      
      scale_val <- switch(input$boutScale,
                          "series" = paste0("\n  scale = ", as.symbol(input$bscalevar), ","),
                          "percentage" = '\n  scale = "percentage",',
                          "proportion" = '\n  scale = "proportion",',
                          "count" = '\n  scale = "count",',
                          "rate" = '\n  scale = "rate",',
                          "other" = '\n  scale = "other",',
                          c()
      )  
      
      intervals <- if (input$bintervals == "NA") NA else input$bintervals
      obslength <- if (input$bobslength == "NA") NA else input$bobslength
      D_const <- if (is.null(input$blrrfloor)) NA else input$blrrfloor
      
    } else {
      # scale_val <- "other"
      scale_val <- '\n  scale = "other",'
      intervals <- obslength <- D_const <- NA
    }
    
    
    if ("PoGO" %in% input$bESpar) {
      if (input$dat_type %in% c("dat", "xlsx")) {
        goal_val <- switch(input$bgoalLevel,
                           "goals" = paste0(as.symbol(input$bgoalvar)),
                           "commonGoal" = input$bcomgoal,
                           c())
      } else {
        goal_val <- input$bcomgoal
      }
      
    } else {
      goal_val <- "NULL"
      
    }
    
    
    improvement <- switch(input$bimprovement,
                          "series" = paste0("\n  improvement = ", as.symbol(input$bseldir), ","),
                          "increase" = '\n  improvement = "increase",',
                          "decrease" = '\n  improvement = "decrease",',
                          c()
    )
    
    if (input$btau_calculation == "Kendall") {
      Kendall <- TRUE
    } else {
      Kendall <- FALSE
    }
    
    if (input$bbaseline_check == "No") {
      pretest_trend <- FALSE
    } else {
      pretest_trend <- input$bsignificance_level
    }
    
    grouping <- paste0('c(', paste(input$b_clusters, collapse=', '), ')')
    condition <- input$b_phase
    outcome <- input$b_out
    aggregate <- paste0('c(', paste(input$b_aggregate, collapse=', '), ')')
    weighting <- input$weighting_scheme
    session_number <- input$session_number
    baseline_phase <- input$b_base
    intervention_phase <- input$b_treat
    ES <- paste0('c("', paste(c(input$bESno, input$bESpar), collapse='", "'), '")')
    pct_change <- input$b_pct_change
    intervals <- intervals
    obslength <- obslength
    D_const <- D_const
    goal <- goal_val
    std_dev <- input$bSMD_denom
    confidence <- input$bconfidence / 100
    Kendall <- Kendall
    pretest_trend <- pretest_trend
    format <- input$resultsformat
    
    if (is.null(input$b_aggregate)) {
      output_res <-
        c(
          parse_code_chunk("bcalc-noaggregate-inputdata",
                           args = list(user_grouping = grouping,
                                       user_condition = condition,
                                       user_outcome = outcome,
                                       user_session_number = session_number,
                                       user_baseline_phase = baseline_phase,
                                       user_intervention_phase = intervention_phase,
                                       user_ES = ES,
                                       user_improvement = improvement,
                                       user_pct_change = pct_change,
                                       user_scale = scale_val,
                                       user_intervals = intervals,
                                       user_obslength = obslength,
                                       user_D_const = D_const,
                                       user_goal = goal,
                                       user_std_dev = std_dev,
                                       user_confidence = confidence,
                                       user_Kendall = Kendall,
                                       user_pretest_trend = pretest_trend,
                                       user_format = format)),
          '')
    } else {
      output_res <-
        c(
          parse_code_chunk("bcalc-aggregate-inputdata",
                           args = list(user_grouping = grouping,
                                       user_condition = condition,
                                       user_outcome = outcome,
                                       user_aggregate = aggregate,
                                       user_weighting = weighting,
                                       user_session_number = session_number,
                                       user_baseline_phase = baseline_phase,
                                       user_intervention_phase = intervention_phase,
                                       user_ES = ES,
                                       user_improvement = improvement,
                                       user_pct_change = pct_change,
                                       user_scale = scale_val,
                                       user_intervals = intervals,
                                       user_obslength = obslength,
                                       user_D_const = D_const,
                                       user_goal = goal,
                                       user_std_dev = std_dev,
                                       user_confidence = confidence,
                                       user_Kendall = Kendall,
                                       user_pretest_trend = pretest_trend,
                                       user_format = format)),
          '')
    }
    
    res <- c(header_res, '', read_res, '', clean_dat, '', output_res)
    paste(res, collapse = "\n")
  })
  
  output$syntax <- renderPrint({
    cat(batch_syntax(), sep = "\n")
  })
  
  output$clip <- renderUI({
    rclipboard::rclipButton("clipbtn", "Copy", batch_syntax(), modal = FALSE, icon = icon("clipboard"),
                            tooltip = "Click me to copy the code to your clipboard!",)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}

ui <- navbarPage(title = "Single-case effect size calculator",
                 id = "SCD_es_calculator",
                 tabPanel("About",
                          navlistPanel(widths = c(3,9),
                                       tabPanel("About", 
                                                includeMarkdown("markdown/About.md")),
                                       tabPanel("Accessing the calculator", 
                                                includeMarkdown("markdown/Accessing.md")),
                                       tabPanel("Using the single-series calculator", 
                                                includeMarkdown("markdown/using_single_series.md")),
                                       tabPanel("Using the multiple-series calculator", 
                                                includeMarkdown("markdown/using_multiple_series.md")),
                                       tabPanel("Example data", 
                                                includeMarkdown("markdown/example-data.md"))
                          )),
                 tabPanel("Single-Series Calculator",
                          fluidRow(column(12,
                                          h3("Data input"),
                                          h5("Enter data values, separated by commas, spaces, or tabs.")
                          )
                          ),
                          fluidRow(
                            column(4, 
                                   textInput("A_dat", label = "Phase A", value = "")
                            ),
                            column(4, 
                                   textInput("B_dat", label = "Phase B", value = "")
                            ),
                            column(4,
                                   checkboxInput("toggleSinglePlot","Show graph", value = FALSE)
                            )
                          ),
                          conditionalPanel(condition = "input.toggleSinglePlot",
                                           fluidRow(
                                             column(12,
                                                    plotOutput('SCDplot', height = "300px")
                                             )
                                           )),
                          fluidRow(
                            hr(),
                            column(4, 
                                   h3("Effect sizes")
                            ),
                            column(8,
                                   h3(textOutput("ES_name"))
                            )
                          ),
                          sidebarLayout(
                            sidebarPanel(width = 4,
                                         tabsetPanel(id = "ES_family", type = "pills",
                                                     tabPanel("Non-overlap", 
                                                              br(),
                                                              selectInput("NOM_ES", 
                                                                          label = "Effect size index",
                                                                          choices = c("IRD",
                                                                                      "NAP",
                                                                                      "PAND",
                                                                                      "PEM",
                                                                                      "PND",
                                                                                      "Tau",
                                                                                      "Tau-BC" = "Tau_BC",
                                                                                      "Tau-U" = "Tau_U"), 
                                                                          selected = "NAP")
                                                     ),
                                                     tabPanel("Parametric", 
                                                              br(),
                                                              selectInput("parametric_ES", 
                                                                          label = "Effect size index", 
                                                                          choices = c("LOR", "LRRd", "LRRi", "LRM", "PoGO", "SMD"), 
                                                                          selected = "LRRd"),
                                                              conditionalPanel(condition = "input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                                               checkboxInput("pct_change","Convert LRR to % change")),
                                                              conditionalPanel(condition = "input.parametric_ES=='PoGO'",
                                                                               numericInput("goal_level", 
                                                                                            label = "Goal level for the behavior", 
                                                                                            value = NULL))
                                                     )
                                         ),
                                         selectInput("improvement",
                                                     label = "Direction of improvement",
                                                     choices = c("increase", "decrease")),
                                         conditionalPanel(condition = "input.ES_family=='Non-overlap' & input.NOM_ES == 'Tau_BC'",
                                                          radioButtons("tau_calculation",
                                                                       label = "Choose a method for calculating Tau index",
                                                                       choices = c("Tau (non-overlap)" = "Nlap",
                                                                                   "Kendall rank correlation" = "Kendall"))
                                         ),
                                         conditionalPanel(condition = "input.ES_family=='Non-overlap' & input.NOM_ES == 'Tau_BC'",
                                                          radioButtons("baseline_check",
                                                                       label = "Test for baseline trend",
                                                                       choices = c("Always adjust for baseline trend" = "No", 
                                                                                   "Pretest for baseline trend, adjust if significant" = "Yes"))
                                         ),
                                         conditionalPanel(condition = "input.ES_family=='Non-overlap' & 
                                               input.NOM_ES == 'Tau_BC' & input.baseline_check == 'Yes'",
                                                          numericInput("significance_level", 
                                                                       label = "Significance level for the initial baseline trend test", 
                                                                       value = 0.05, step = .01,
                                                                       min = 0.01, 
                                                                       max = 0.99)
                                         ),
                                         conditionalPanel("input.ES_family=='Parametric'",
                                                          conditionalPanel(condition = "input.parametric_ES == 'SMD'",
                                                                           radioButtons("SMD_denom", 
                                                                                        label = "Standardized by", 
                                                                                        choices = c("baseline SD","pooled SD"))),
                                                          conditionalPanel(condition = "input.parametric_ES=='LOR'|input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                                           selectInput("outScale", label = "Outcome Scale",
                                                                                       choices = c("percentage", 
                                                                                                   "proportion",
                                                                                                   "count",
                                                                                                   "rate",
                                                                                                   "other")),
                                                                           numericInput("intervals", label = "If observed via interval recording, total intervals per session", value = NULL)),
                                                          conditionalPanel(condition = "input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                                           numericInput("obslength", label = "Session length (in minutes)", value = NULL)),
                                                          conditionalPanel(condition = "input.parametric_ES=='LOR'|input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                                           numericInput("lrrfloor", label = "User-specified floor constant",value = NULL))
                                         ),
                                         conditionalPanel(condition = "input.ES_family=='Parametric'|input.NOM_ES=='NAP'|input.NOM_ES=='Tau'|input.NOM_ES=='Tau_BC'",
                                                          numericInput("confidence", 
                                                                       label = "Confidence level", 
                                                                       value = 95, 
                                                                       min = 0, 
                                                                       max = 100)
                                         ),
                                         numericInput("digits","Digits",
                                                      value = 2, min = 1, 
                                                      max = 16, step = 1)
                            ),
                            mainPanel(width = 8,
                                      conditionalPanel(condition = "input.ES_family=='Non-overlap' & input.NOM_ES == 'PND'", 
                                                       includeMarkdown("markdown/PND-message.md")),
                                      htmlOutput("result"),
                                      checkboxInput("explanation", 
                                                    label = "Show methods and references", 
                                                    value = FALSE),
                                      conditionalPanel("input.explanation==true", 
                                                       conditionalPanel("input.ES_family=='Non-overlap'",
                                                                        conditionalPanel("input.NOM_ES == 'IRD'", withMathJax(includeMarkdown("markdown/IRD.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'NAP'", withMathJax(includeMarkdown("markdown/NAP.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'PAND'", withMathJax(includeMarkdown("markdown/PAND.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'PEM'", withMathJax(includeMarkdown("markdown/PEM.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'PND'", withMathJax(includeMarkdown("markdown/PND.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'Tau'", withMathJax(includeMarkdown("markdown/Tau.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'Tau_BC'", withMathJax(includeMarkdown("markdown/Tau-BC.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'Tau_U'", withMathJax(includeMarkdown("markdown/Tau-U.md")))
                                                       ),
                                                       conditionalPanel("input.ES_family=='Parametric'",
                                                                        conditionalPanel("input.parametric_ES == 'LRRi'|input.parametric_ES == 'LRRd'", withMathJax(includeMarkdown("markdown/LRR.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'LRM'", withMathJax(includeMarkdown("markdown/LRM.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'SMD'", withMathJax(includeMarkdown("markdown/SMD.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'LOR'", withMathJax(includeMarkdown("markdown/LOR.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'PoGO'", withMathJax(includeMarkdown("markdown/PoGO.md")))
                                                       )
                                      )
                            )
                            
                          )
                 ),
                 tabPanel("Multiple-Series Calculator",
                          tabsetPanel(
                            id = "BatchEntryTabs",
                            tabPanel("Data",
                                     sidebarLayout(sidebarPanel(radioButtons('dat_type', 'What data do you want to use?', 
                                                                             c("Use an example" = "example",
                                                                               "Upload data from a .csv or .txt file" = "dat",
                                                                               "Upload data from a .xlsx file" = "xlsx")),
                                                                conditionalPanel(
                                                                  condition = "input.dat_type == 'example'",
                                                                  selectInput("example", label = "Choose an example", 
                                                                              choices = example_list)
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.dat_type == 'dat'",
                                                                  fileInput('dat', 'Upload a .csv or .txt file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.txt')),
                                                                  checkboxInput('header', 'File has a header?', TRUE),
                                                                  radioButtons('sep', 'Data seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' '), inline = TRUE),
                                                                  radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"), inline = TRUE)
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.dat_type == 'xlsx'",
                                                                  fileInput('xlsx', 'Upload a .xlsx file', accept = c('.xlsx')),
                                                                  checkboxInput('col_names', 'File has a header?', TRUE),
                                                                  selectInput("inSelect", "Select a sheet", "")
                                                                ),
                                                                uiOutput("filtervarMapping"),
                                                                uiOutput("filterMapping")
                                     ),
                                     mainPanel(tableOutput("datview")))
                            ),
                            tabPanel("Variables",
                                     sidebarLayout(
                                       sidebarPanel(
                                         style = "max-height: 1200px; overflow-y: auto",
                                         conditionalPanel(condition = "input.dat_type == 'dat' | input.dat_type == 'xlsx'",
                                                          checkboxInput("calcPhasePair", "Calculate phase pair numbers for ABAB designs.", value = FALSE)),
                                         uiOutput("clusterPhase"),
                                         uiOutput("baseDefine"),
                                         uiOutput("treatDefine"),
                                         uiOutput("outOrderImp"),
                                         conditionalPanel(condition = "input.bimprovement == 'series'",
                                                          uiOutput("improvementVar")),
                                         conditionalPanel(condition = "input.bimprovement == 'series'",
                                                          uiOutput("improvementDir")),
                                         br(),
                                         br(),
                                         br()
                                       ),
                                       mainPanel(tableOutput("datview2"))
                                     )
                            ),
                            tabPanel("Plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         style = "max-height: 800px; overflow-y: auto",
                                         uiOutput("facetSelector"),
                                         uiOutput("graph_filters"),
                                         br(),
                                         br(),
                                         br(),
                                         br()
                                       ),
                                       mainPanel(plotOutput('batchPlot', height = "auto"))
                                     )
                                     
                            ),
                            tabPanel("Estimate", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Select Effect Sizes"),
                                         checkboxGroupInput("bESno", "Non-Overlap Effect Sizes", choices = c("IRD","NAP","PAND","PEM","PND","Tau","Tau-BC" = "Tau_BC","Tau-U" = "Tau_U"), inline = TRUE),
                                         checkboxGroupInput("bESpar", "Parametric Effect Sizes", choices = c("LOR", "LRRd", "LRRi", "LRM", "PoGO", "SMD"), inline = TRUE),
                                         conditionalPanel(condition = "input.bESno.includes('Tau_BC')",
                                                          radioButtons("btau_calculation", label = "Choose a method for calculating Tau index",
                                                                       choices = c("Tau (non-overlap)" = "Nlap",
                                                                                   "Kendall rank correlation" = "Kendall"),
                                                                       inline = TRUE)),
                                         conditionalPanel(condition = "input.bESno.includes('Tau_BC')",
                                                          radioButtons("bbaseline_check", label = "Use baseline trend test for Tau-BC?",
                                                                       choices = c("Always adjusting for baseline trend" = "No", 
                                                                                   "Pretest for baseline trend, adjust if significant" = "Yes"), 
                                                                       inline = FALSE)),
                                         conditionalPanel(condition = "input.bESno.includes('Tau_BC') & input.bbaseline_check == 'Yes'",
                                                          numericInput("bsignificance_level",
                                                                       label = "Significance level for the baseline trend test",
                                                                       value = 0.05, step = .01,
                                                                       min = 0.01,
                                                                       max = 0.99)),
                                         conditionalPanel(condition = "input.bESpar.includes('LRRi') | input.bESpar.includes('LRRd') | input.bESpar.includes('LOR')",
                                                          checkboxInput("b_pct_change", "Convert LRR to % change")),
                                         conditionalPanel(condition = "input.bESpar.includes('LOR')", 
                                                          strong(style="color:orange","LOR will only be calculated for outcomes measured as percentages or proportions."),
                                                          br("")),
                                         conditionalPanel(condition = "input.bESpar.includes('SMD')", 
                                                          radioButtons("bSMD_denom", label = "Standardize SMD ", 
                                                                       choices = c("baseline SD" = "baseline", "pooled SD" = "pool"), inline = TRUE)),
                                         conditionalPanel(condition = "input.bESpar.includes('LRRi') | input.bESpar.includes('LRRd') | input.bESpar.includes('LOR')",
                                                          uiOutput("outcomeScale")),
                                         conditionalPanel(condition = "input.bESpar.includes('LRRi') | input.bESpar.includes('LRRd') | input.bESpar.includes('LOR')",
                                                          uiOutput("measurementProc")),
                                         conditionalPanel(condition = "input.bESpar.includes('PoGO')", uiOutput("goalLevel")),
                                         conditionalPanel(condition = "input.b_aggregate != ''", 
                                                          radioButtons('weighting_scheme',
                                                                       label = "Weighting scheme to use for aggregating.",
                                                                       choices = c("equal", "1/V", "nA", "nB", "nA*nB", "1/nA + 1/nB"))
                                         ),
                                         numericInput("bconfidence", label = "Confidence level (for any effect size with standard errors)", value = 95, min = 0, max = 100),
                                         numericInput("bdigits","Digits",
                                                      value = 2, min = 1, 
                                                      max = 16, step = 1),
                                         radioButtons("resultsformat", "Long or wide format?", c("Long" = "long", "Wide" = "wide"), inline = TRUE),
                                         conditionalPanel(condition = "input.bESpar.length > 0 || input.bESno.length > 0", 
                                                          actionButton("batchest", "Estimate"))
                                       ),
                                       
                                       mainPanel(
                                         conditionalPanel(condition = "input.bESno.includes('PND')", 
                                                          includeMarkdown("markdown/PND-message.md")),
                                         tableOutput("batchTable"),
                                         p(),
                                         conditionalPanel(condition = "input.batchest > 0",
                                                          downloadButton("downloadES", label = "Download results"))
                                       )
                                     )
                                     
                            ),
                            tabPanel("Syntax for R",
                                     rclipboard::rclipboardSetup(),
                                     uiOutput("clip"),
                                     verbatimTextOutput("syntax")
                            )
                          )
                          
                 ))

shinyApp(ui = ui, server = server)
