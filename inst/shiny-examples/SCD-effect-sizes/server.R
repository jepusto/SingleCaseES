library(ggplot2)
library(SingleCaseES)

statistical_indices <- c("NAP","Tau","SMD","LRR")

full_names <- list(IRD = "Robust Improvement Rate Difference",
                   NAP = "Non-overlap of All Pairs",
                   PAND = "Percentage of All Non-overlapping Data",
                   PEM = "Percent Exceeding the Median",
                   PND = "Percentage of Non-overlapping Data",
                   Tau = "Tau",
                   LRR = "Log Response Ratio",
                   SMD = "Standardized Mean Difference (within-case)")

shinyServer(function(input, output) {
  
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
    if (dat()$compute) {
      full_names[[ES()$index]] 
    }
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

})