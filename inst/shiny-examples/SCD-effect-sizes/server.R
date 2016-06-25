library(ggplot2)
devtools::load_all()

statistical_indices <- c("NAP","Tau","SMD","LRR")

shinyServer(function(input, output) {
  
  dat <- reactive({ 
    dat_strings <- list(A = input$A_dat, B = input$B_dat)
    dat <- lapply(dat_strings, function(x) as.numeric(unlist(strsplit(x, split = "\\s|\\t|\\n|,"))))
    dat$compute <- all(sapply(dat, function(x) sum(!is.na(x)) >= 3))
    dat
    })
  
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
  
  output$ES <- renderPrint({
    if (dat()$compute) {
      ES()$est
    }
  })

})