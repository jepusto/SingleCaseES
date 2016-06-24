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
    arg_vals <- list(A_data = dat()$A, B_data = dat()$B, improvement = input$improvement)
    if (index %in% statistical_indices) {
      conf <- c("Non-overlap" = input$conf_NOM, "Parametric" = input$conf_parametric)[[input$ES_family]]
      arg_vals$confidence <- conf
    }
    
    est <- tryCatch(do.call(index, arg_vals), warning = function(w) w, error = function(e) e)
    
    list(index = index, est = est)
  })
  
  output$confidence <- renderUI({
    if (ES()$index %in% statistical_indices) {
      numericInput("confidence", label = "Confidence level", value = 95, min = 0, max = 100)  
    } else {
      NULL
    }
  })
  
  output$ES <- renderPrint({
    if (dat()$compute) {
      ES()$est
    }
  })

})