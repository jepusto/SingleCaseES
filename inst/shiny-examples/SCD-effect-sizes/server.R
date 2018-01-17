library(ggplot2)
library(tidyr)
library(dplyr)
library(SingleCaseES)
library(rlang)

statistical_indices <- c("NAP","Tau","SMD","LRR")

full_names <- list(IRD = "Robust Improvement Rate Difference",
                   NAP = "Non-overlap of All Pairs",
                   PAND = "Percentage of All Non-overlapping Data",
                   PEM = "Percent Exceeding the Median",
                   PND = "Percentage of Non-overlapping Data",
                   Tau = "Tau",
                   Tau_U = "Tau-U",
                   LRR = "Log Response Ratio",
                   SMD = "Standardized Mean Difference (within-case)")


shinyServer(function(input, output, session) {
  
  dat <- reactive({ 
    dat_strings <- list(A = input$A_dat, B = input$B_dat)
    dat <- lapply(dat_strings, function(x) as.numeric(unlist(strsplit(x, split = "\\s|\\t|\\n|,"))))
    dat$compute <- all(sapply(dat, function(x) sum(!is.na(x)) >= 3))
    dat
    })
  
  output$SCDplot <- renderPlot({
    if (input$plot & dat()$compute) {
      A_dat <- dat()$A[!is.na(dat()$A)]
      B_dat <- dat()$B[!is.na(dat()$B)]
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
  }, height = function() 300 * dat()$compute * input$plot)
  
  ES <- reactive({
    index <- c("Non-overlap" = input$NOM_ES, "Parametric" = input$parametric_ES)[[input$ES_family]]
    arg_vals <- list(A_data = dat()$A, B_data = dat()$B)
    if (input$ES_family == "Non-overlap") {
      arg_vals$improvement <- input$improvement
    }
    if (index == "SMD") {
      arg_vals$std_dev <- substr(input$SMD_denom, 1, nchar(input$SMD_denom) - 3)
    }
    if (index %in% statistical_indices) {
      arg_vals$confidence <- input$confidence / 100
    }
    
    est <- tryCatch(do.call(index, arg_vals), warning = function(w) w, error = function(e) e)
    
    list(index = index, est = est)
  })
  
  output$ES_name <- renderText({
    full_names[[ES()$index]] 
  })
  
  output$result <- renderUI({
    if (dat()$compute) {
      fmt <- function(x) format(x, digits = input$digits, nsmall = input$digits)
      est <- ES()$est
      if (ES()$index %in% statistical_indices) {
        Est_txt <- paste("Effect size estimate:", fmt(est$Est))
        SE_txt <- paste("Standard error:", fmt(est$SE))
        CI_txt <- paste0(input$confidence,"% CI: [", fmt(est$CI[1]), ", ", fmt(est$CI[2]), "]")
        note_txt <- "<br/>Note: SE and CI are based on the assumption that measurements are mutually independent (i.e., not auto-correlated)." 
        HTML(paste(Est_txt, SE_txt, CI_txt, note_txt, sep = "<br/>"))
      } else {
        HTML(paste("Effect size estimate:", fmt(est)))
      }
    }
  })
  
  datFile <- reactive({
    
    if(input$dat_type == "example"){
      dat <- get(input$example)
      
      return(dat)
    }
    
    inFile <- input$dat
    
    if (is.null(inFile)) return(NULL)
    
    read.table(inFile$datapath, header=input$header, 
               sep=input$sep, quote=input$quote,
               stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  output$datview <- renderTable(datFile())
  
  output$indexMapping <- renderUI({
    
    var_names <- names(datFile())
    list(conditionalPanel(condition = "input.dat_type == 'example'",
                          actionButton("bupdateselections", label = "Automatically populate example choices")),
    selectizeInput("b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", choices = var_names, selected = NULL, multiple = TRUE),
    selectInput("b_phase", label = "Phase Indicator", choices = var_names, selected = var_names[3]),
    selectInput("b_base", label = "Baseline Phase Value", 
                choices = if(input$dat_type == "example"){exampleMapping[[input$example]]$phase_vals}else{unique(datFile()[input$b_phase])}, 
                selected = if(input$dat_type == "example"){exampleMapping[[input$example]]$phase_vals[1]}else{NULL}),
    selectInput("b_treat", label = "Treatment Phase Value", 
                choices = if(input$dat_type == "example"){exampleMapping[[input$example]]$phase_vals}else{unique(datFile()[input$b_phase])}, 
                selected = if(input$dat_type == "example"){exampleMapping[[input$example]]$phase_vals[2]}else{NULL}),
    selectInput("b_out", label = "Outcome", choices = var_names, selected = var_names[4]),
    hr(),
    h4("Select Effect Sizes"),
    checkboxGroupInput("bESno", "Non-Overlap Effect Sizes", choices = c("IRD","NAP","PAND","PEM","PND","Tau","Tau-U" = "Tau_U"), inline = TRUE),
                      selectInput("bimprovement", label = "Direction of improvement", 
                                     choices = c("all increase" = "increase", "all decrease" = "decrease", "by series" = "series")),
                      conditionalPanel(condition = "input.bimprovement == 'series'",
                                       selectInput("bseldir", label = "Select variable identifying improvement direction",
                                       choices = var_names, selected = if(input$dat_type == "example"){exampleMapping[[input$example]]$direction_var}else{NULL})),
    checkboxGroupInput("bESpar", "Parametric Effect Sizes", choices = c("LRR","SMD"),inline = TRUE),
    radioButtons("bSMD_denom", label = "If calculating SMD, standardized by", 
                                                       choices = c("baseline SD","pooled SD"), inline = TRUE),
    numericInput("bconfidence", label = "Confidence level (for any effect size with standard errors)", value = 95, min = 0, max = 100)
    )
  })
  
  observeEvent(input$bupdateselections,{
    var_names <- names(datFile())
    b_var_names <- exampleMapping[[input$example]]$varnames
    n_var <- length(b_var_names)
    improvement <- exampleMapping[[input$example]]$direction
    if(improvement == "series") dirname <- exampleMapping[[input$example]]$direction_var
    updateSelectizeInput(session, "b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", choices = var_names, selected = b_var_names[3:n_var])
    updateSelectInput(session, "b_phase", label = "Phase Indicator", choices = var_names, selected = b_var_names[1])
    updateSelectInput(session, "b_out",label = "Outcome", choices = var_names, selected = b_var_names[2])
    updateSelectInput(session, "bimprovement", label = "Direction of improvement", 
                      choices = c("all increase" = "increase", "all decrease" = "decrease", "by series" = "series"), selected = improvement)
    })
  
  batchModel <- eventReactive(input$batchest, {
    
    call_index <- function(all_args, dat){
      arg_vals <- list()
      arg_vals$A_data <- dat$Outcome[dat$phase == input$b_base]
      arg_vals$B_data <- dat$Outcome[dat$phase == input$b_treat]
      index <- dat$index[1]
      
      if (index %in% c("IDR", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_U") & input$bimprovement != "series") {
        arg_vals$improvement <- input$bimprovement
      }
      if (index %in% c("IDR", "NAP", "PAND", "PEM", "PND", "Tau", "Tau_U") & input$bimprovement == "series") {
        arg_vals$improvement <- dat[1,input$bseldir]
      }
      if (index == "SMD") {
        arg_vals$std_dev <- substr(input$bSMD_denom, 1, nchar(input$bSMD_denom) - 3)
      }
      if (index %in% statistical_indices) {
        arg_vals$confidence <- input$bconfidence / 100
      }
      
      
      if(index %in% statistical_indices){
       es <- unlist(do.call(index, arg_vals))
       return(data.frame(est = es[1], SE = es[2], lower = es[3], upper = es[4]))
      }else{
       es <-  do.call(index, arg_vals)
       return(data.frame(est = es, SE = NA, lower = NA, upper = NA))
      }
    }
    
    if(input$bimprovement == "series"){
      dat <- datFile()[c(input$b_out, input$b_phase, input$b_clusters, input$bseldir)]
    }else{
    dat <- datFile()[c(input$b_out, input$b_phase, input$b_clusters)]
    }
    names(dat)[1:2] <- c("Outcome", "phase")
    
    index <- data.frame(index = c(input$bESno, input$bESpar), stringsAsFactors = FALSE)
    
    dat <- merge(dat, index, stringsAsFactors = FALSE)
    
    all_args <- list()
    all_args$improvement <- input$bimprovement
    all_args$std_dev <- substr(input$bSMD_denom, 1, nchar(input$bSMD_denom) - 3)
    all_args$confidence <- input$bconfidence / 100
    
    grouping_sym <- syms(c(input$b_clusters, "index"))
    
    dat <- dat %>%  
      group_by(!!!grouping_sym) %>%
      do(call_index(all_args = all_args, dat = .))
    
    if(input$convertWide){
      dat <- gather(dat, key = "key", value = "value", est:upper, na.rm = TRUE) %>%
        unite(col = "est", index, key) %>%
        spread(key = "est", value = "value")
    }
    
    return(dat)

  })
  
  output$batchTable <- renderTable(batchModel(), na = "-")
  
})
