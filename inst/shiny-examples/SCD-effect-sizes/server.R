library(markdown, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)
library(janitor, warn.conflicts = FALSE, quietly = TRUE)
library(SingleCaseES, warn.conflicts = FALSE, quietly = TRUE)
library(rlang, warn.conflicts = FALSE, quietly = TRUE)

source("mappings.R")
source("helper-functions.R", local = TRUE)

statistical_indices <- c("NAP","Tau", "SMD","LRRi", "LRRd", "LOR", "LRM")

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
                   SMD = "Standardized Mean Difference (within-case)")


shinyServer(function(input, output, session) {

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
    
    if (input$plot & dat()$compute) {
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
    }
  })
  
  ES <- reactive({
    index <- c("Non-overlap" = input$NOM_ES, "Parametric" = input$parametric_ES)[[input$ES_family]]
    
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
                     D_const = input$lrrfloor,
                     pretest_trend = pretest_trend
                     )
    
    est <- tryCatch(do.call(calc_ES, arg_vals), warning = function(w) w, error = function(e) e)
    
    if (index %in% c("LRRi", "LRRd", "LOR")){
      validate(
        need(all(c(dat()$A, dat()$B) >= 0), message = "For the log response or log odds ratio, all data must be greater than or equal to zero. ")
      )
    }
    
    if (index == "LRM"){
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
      
      if (ES()$index %in% statistical_indices | ES()$index == "Tau_BC") {
        Est_txt <- paste("Effect size estimate:", fmt(est$Est[1]))
        SE_txt <- paste("Standard error:", fmt(est$SE[1]))
        CI_txt <- paste0(input$confidence,"% CI: [", fmt(est$CI_lower[1]), ", ", fmt(est$CI_upper[1]), "]")
        
        if (nrow(est) > 1) {
          pct_est <- paste("<br/>Percentage change:", pct(est$Est[2]))
          pct_CI <- paste0(input$confidence,"% CI: [", pct(est$CI_lower[2]), ", ", pct(est$CI_upper[2]), "]")
          pct_txt <- paste(pct_est, pct_CI, sep = "<br/>")
        } else {
          pct_txt <- NULL
        }
        
        if (ES()$index == "Tau_BC" & ES()$est$ES == "Tau") {
          note_txt_TauBC <- strong(style="color:red", "The baseline trend is not statistically significant. Tau is calculated without trend correction.")
        } else {
          note_txt_TauBC <- NULL
        }
        
        note_txt <- "<br/>Note: SE and CI are based on the assumption that measurements are mutually independent (i.e., not auto-correlated)." 
        HTML(paste(Est_txt, SE_txt, CI_txt, pct_txt, note_txt_TauBC, note_txt, sep = "<br/>"))
        
      } else {
        Est_txt <- paste("Effect size estimate:", fmt(est$Est))
        HTML(Est_txt)
      }
    }
  })
  
  sheetname <- reactive({
    if (input$dat_type == "xlsx") {
      inFile <- input$xlsx
      if (is.null(inFile)) return(NULL)
      sheetnames <- excel_sheets(inFile$datapath)
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
      
      return(dat)
      
    } else if (input$dat_type == "dat") {
      
      inFile <- input$dat
      
      if (is.null(inFile)) return(NULL)
      
      read.table(inFile$datapath, header=input$header, 
                 sep=input$sep, quote=input$quote,
                 stringsAsFactors = FALSE, check.names = FALSE) %>% 
        clean_names(case = "parsed")
      
    } else if (input$dat_type == "xlsx") {
      
      inFile <- input$xlsx
      
      if (is.null(inFile) || is.null(input$inSelect) || nchar(input$inSelect) == 0) return(NULL)
      
      readxl::read_xlsx(inFile$datapath, col_names = input$col_names, sheet = input$inSelect) %>%
        clean_names(case = "parsed") %>% 
        as.data.frame()
      
    }
    
  })
  
  output$datview <- renderTable(datFile())
  output$datview2 <- renderTable(datFile())
  
  output$clusterPhase <- renderUI({
      var_names <- names(datFile())
      if (input$dat_type == "dat" || input$dat_type == "xlsx") {
        list(
        selectizeInput("b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", choices = var_names, 
                       selected = NULL, multiple = TRUE),
        selectizeInput("b_aggregate", label = "Select all variables to average across after calculating effect size estimates.", choices = var_names, 
                       selected = NULL, multiple = TRUE),
        selectInput("b_phase", label = "Phase indicator", choices = var_names, selected = var_names[3]))
      } else {
        curMap <- exampleMapping[[input$example]]
        list(
          selectizeInput("b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", choices = var_names, 
                         selected = curMap$cluster_vars, multiple = TRUE),
          selectizeInput("b_aggregate", label = "Select all variables to average across after calculating effect size estimates.", choices = var_names, 
                         selected = curMap$aggregate_vars, multiple = TRUE),
          selectInput("b_phase", label = "Phase indicator", choices = var_names, selected = curMap$condition)
        )
        
      }
  })
  
  output$weightingScheme <- renderUI({
    radioButtons('weighting_scheme',
                 label = "Weighting scheme to use for aggregating.",
                 choices = c("1/V", "equal"))
  })
  
  output$baseDefine <- renderUI({
    
    phase_choices <- if (!is.null(input$b_phase)) unique(datFile()[[input$b_phase]]) else c("A","B")
    
    if (input$dat_type == "dat") {
      selectInput("b_base", label = "Baseline phase value", choices = phase_choices)
    } else {
      curMap <- exampleMapping[[input$example]]
      selectInput("b_base", label = "Baseline phase value", choices = phase_choices, selected = curMap$phase_vals[1])
    }
  })
  
  output$treatDefine <- renderUI({
    
    phase_choices <- if (!is.null(input$b_phase)) unique(datFile()[[input$b_phase]]) else c("A","B")
    trt_choices <- setdiff(phase_choices, input$b_base)
    
    if (input$dat_type == "dat") {
      selectInput("b_treat", label = "Treatment phase value", choices = trt_choices)
    } else {
      curMap <- exampleMapping[[input$example]]
      selectInput("b_treat", label = "Treatment phase value", choices = trt_choices, selected = curMap$phase_vals[2])  
    }
  })

    
  output$outOrderImp <- renderUI({

    var_names <- names(datFile())

    if (input$dat_type == "dat") {
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
    var_names <- names(datFile())
    
    if (input$dat_type == "dat") {
      list(selectInput("bseldir", label = "Select variable identifying improvement direction", choices = var_names))
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
    
    var_names <- names(datFile())
    
    if (input$dat_type == "dat") {  
      list(selectInput("boutScale", label = "Outcome Scale",
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
      list(selectInput("boutScale", label = "Outcome Scale",
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


  batchModel <- eventReactive(input$batchest, {
    
    if (any(input$bESpar %in% c("LRRi", "LRRd", "LOR"))) {
      if (input$boutScale == "series") {
        scale_val <- input$bscalevar
      } else{
        scale_val <- input$boutScale
      }
    } else {
      scale_val <- "other"
    }
    
    if(input$bimprovement == "series") {
      improvement <- input$bseldir
    } else {
      improvement <- input$bimprovement
    }
    
    if (input$bbaseline_check == "No") {
      pretest_trend <- FALSE
    } else {
      pretest_trend <- input$bsignificance_level
    }
    
    if(is.null(input$b_aggregate)) {
      output <- batch_calc_ES(dat = datFile(), 
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
                              std_dev = input$bSMD_denom,
                              confidence = input$bconfidence / 100,
                              pretest_trend = pretest_trend,
                              format = input$resultsformat)
    } else{
      output <- batch_calc_ES(dat = datFile(), 
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
                              std_dev = input$bSMD_denom,
                              confidence = input$bconfidence / 100,
                              pretest_trend = pretest_trend,
                              format = input$resultsformat)
    }
    
    return(output)

  })
  
  output$downloadES <- downloadHandler(
    filename = "SCD effect size estimates.csv",
    content = function(file) {
      dat <- batchModel()
      write.csv(dat, file, row.names=FALSE)
    },
    contentType = "text/csv"
  )
  
  output$batchTable <- renderTable(batchModel(), na = "-")
  
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
    
    # batch calculation
      if (any(input$bESpar %in% c("LRRi", "LRRd", "LOR"))) {
        if (input$boutScale == "series") {
          scale_val <- input$bscalevar
        } else{
          scale_val <- input$boutScale
        }
      } else {
        scale_val <- "other"
      }
      
      if(input$bimprovement == "series") {
        improvement <- input$bseldir
      } else {
        improvement <- input$bimprovement
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
      improvement <- improvement
      pct_change <- input$b_pct_change
      scale <- scale_val
      std_dev <- input$bSMD_denom
      confidence <- input$bconfidence / 100
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
                                       user_scale = scale,
                                       user_std_dev = std_dev,
                                       user_confidence = confidence,
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
                                       user_scale = scale,
                                       user_std_dev = std_dev,
                                       user_confidence = confidence,
                                       user_pretest_trend = pretest_trend,
                                       user_format = format)),
          '')
      }
    
    res <- c(header_res, '', read_res, '', output_res)
    paste(res, collapse = "\n")
  })
  
  output$syntax <- renderPrint({
    cat(batch_syntax(), sep = "\n")
  })
  
  output$clip <- renderUI({
    rclipButton("clipbtn", "Copy", batch_syntax(), icon("clipboard"))
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
})
