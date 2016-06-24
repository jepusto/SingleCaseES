shinyServer(function(input, output) {
  
  output$value <- renderPrint({ input$A_dat })
  
})