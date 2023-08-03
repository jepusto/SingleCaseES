library(shiny)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = 'inst/shiny-examples/SCD-effect-sizes',
  host = '0.0.0.0',
  port = as.numeric(port)
)