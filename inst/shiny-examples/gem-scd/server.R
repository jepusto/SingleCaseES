library(ggplot2)
library(tidyr)
library(dplyr)
library(purrrlyr)
library(SingleCaseES)

model_params <- function(dat, fam, m){
  lm1 <- gem_scd(outcome = dat$Outcome, Trt = dat$Trt, m = m, fam = fam)
  
  c(lm1$coefficients[1],diag(lm1$varCov)[1],lm1$coefficients[2],diag(lm1$varCov)[2], lm1$omega, diag(lm1$varCov)[3], summary(lm1)$dispersion)
}


shinyServer(function(input, output) {
  
  Trt <- reactive(as.numeric(unlist(strsplit(input$trt, split = "\\s|\\t|\\n|,"))))
  Out <- reactive(as.numeric(unlist(strsplit(input$outcome, split = "\\s|\\t|\\n|,"))))
  
  observeEvent(input$Estimate, {
    
   Trt <- Trt()
   Out <- Out()
   
   if(input$perctoprop) Out <- pmax(0, pmin(1, Out/100))
     
   model <- gem_scd(outcome = Out, 
                    Trt = Trt, 
                    m = as.numeric(input$m), 
                    fam = do.call(input$family, args = list(link = input$link))
                    )
   phase_lines <- cumsum(rle(Trt)$lengths) + .5
   phase_lines <- phase_lines[1:(length(phase_lines)-1)]
   phase_lengths <- rle(Trt)$lengths
   
   plot_dat <- 
     data.frame(
       fitted = model$fitted.values, 
       observed = Out, 
       phase = ifelse(Trt == Trt[1], "A", "B"), 
       time = 1:length(Out)
     ) %>%
     mutate(phase_number = rep(1:length(phase_lengths), times = phase_lengths)) %>%
     gather(key = type, value = Outcome, fitted, observed) %>%
     mutate(typephase = paste0(type, phase_number))
   
  output$modelplot <- renderPlot(
    ggplot(plot_dat, aes(y = Outcome, x = time, color = type, group = typephase)) + 
      geom_point() +
      geom_line() +
      expand_limits(y = 0) +
      geom_vline(xintercept = phase_lines, linetype = "dashed") +
      scale_color_brewer(type = "qual", palette = 6) + 
      labs(x = "Session Number", y = "Outcome") +
      theme_bw() + 
      theme(legend.position = "bottom")
  )
  
  output$single_table <- renderTable(
    data.frame(
      "Effect Size" =  model$coefficients[2],
      "Standard Error" = sqrt(diag(model$varCov)[2]), 
      "Delay parameter (omega)" =  model$omega, 
      "Dispersion" = summary(model)$dispersion, 
    check.names = FALSE)
  )
  })
    
    # Read in data
    
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
 

    
output$variableMapping <- renderUI({
      
      if(input$dat_type == "example"){
        var_names <- exampleMapping[[input$example]]$varnames
        n_var <- length(var_names)
        return(list(
          selectizeInput("b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", choices = var_names, selected = var_names[4:n_var], multiple = TRUE),
          selectInput("b_treat", label = "Treatment Assignment", choices = var_names, selected = var_names[1]),
          selectInput("b_outcome", label = "Outcome", choices = var_names, selected = var_names[2]),
          checkboxInput("b_perctoprop", label = "Transform percent to proportion? (Necessary for binomial or quasi-binomial link)", value = exampleMapping[[input$example]]$transform),
          selectInput("session_number", label = "Within-Case Session Number", choices = var_names, selected = var_names[3]),
          helpText("Data will be ordered by session number", "within each case."),
          textInput("m_length", label = "m", value = exampleMapping[[input$example]]$m),
          helpText("This is the number of treatment sessions the treatment effect is pegged to."),
          selectInput("b_family", label = "Variance Function",
                      choices = c("quasi-binomial" = "quasibinomial","quasi-Poisson" = "quasipoisson", "binomial", "gaussian", "poisson"), 
                      selected = exampleMapping[[input$example]]$family),
          selectInput("b_link", label = "Link Function",
                      choices = c("log", "logit", "identity"),
                      selected = exampleMapping[[input$example]]$link),
          actionButton("batchest", "Estimate Models")
        ))
      }
      
      if(input$dat_type == "dat"){
      var_names <- names(datFile())
      n_var <- length(var_names)
      return(list(
        selectizeInput("b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", choices = var_names, selected = NULL, multiple = TRUE),
        selectInput("b_treat", label = "Treatment Assignment", choices = var_names, selected = var_names[n_var - 4]),
        selectInput("b_outcome", label = "Outcome", choices = var_names, selected = var_names[n_var - 3]),
        checkboxInput("b_perctoprop", label = "Transform percent to proportion? (Necessary for binomial or quasi-binomial link)"),
        selectInput("session_number", label = "Session Number", choices = var_names, selected = var_names[n_var - 2]),
        textInput("m_length", label = "m"),
        helpText("This is the number of treatment sessions at which to estimate the treatment effect."),
        selectInput("b_family", label = "Variance Function",
                    choices = c("quasi-binomial" = "quasibinomial","quasi-Poisson" = "quasipoisson", "binomial", "gaussian", "poisson"), 
                    selected = "quasipoisson"),
        selectInput("b_link", label = "Link Function",
                    choices = c("log", "logit", "identity"),selected = "log"),
       actionButton("batchest", "Estimate Models")
      ))}
    })
  
batchModel <- eventReactive(input$batchest, {
    
    dat <- datFile()[c(input$b_outcome, input$b_treat, input$session_number, input$b_clusters)]
    names(dat)[1:3] <- c("Outcome", "Trt", "session_number")
    
  
    if(input$b_perctoprop) dat$Outcome <- pmax(0, pmin(1, dat$Outcome/100))
  
    dat <- dat %>%
      mutate(session_number = as.numeric(session_number)) %>%
      arrange_(.dots = c(input$b_clusters, "session_number"))
      
    dat %>%  slice_rows(input$b_clusters) %>%
      by_slice(~model_params(dat = ., fam = do.call(input$b_family, args = list(link = input$b_link)), m = as.numeric(input$m_length)), .collate = "cols") %>%
      rename(Intercept = .out1, `Intercept SE` = .out2, `Treatment Effect` = .out3,
             `Treatment Effect SE` = .out4, Omega = .out5, `Omega SE` = .out6,
             Dispersion = .out7)
    
  })

output$batchTable <- renderTable(batchModel())

output$downcsv <- downloadHandler(
  filename = function( ){
    fname <- if (input$dat_type == "example") input$example else input$dat
    paste(fname, '- GEM estimates.csv')
    },
  content = function(file){
    writeDat <- batchModel()
    write.csv(writeDat, file, row.names = FALSE)
    },
  contentType = "text/csv")
})
