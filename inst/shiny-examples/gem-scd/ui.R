library(shiny)
library(markdown)
source("mappings.R")

ui <- navbarPage(title = "Gradual Effects Model Calculator",
                 tabPanel("GEM SCD",
                          navlistPanel(widths = c(3,9),
                                       tabPanel("About", includeMarkdown("markdown/gem_scd.md")),
                                       tabPanel("Accessing gem_scd", includeMarkdown("markdown/Accessing_gem_scd.md")),
                                       tabPanel("Using the single-entry calculator", includeMarkdown("markdown/using_gem_scd_single.md")),
                                       tabPanel("Using the batch-entry calculator", includeMarkdown("markdown/using_gem_scd_batch.md")),
                                       tabPanel("Example data", includeMarkdown("markdown/example-data.md"))
                          )
                 ),
                tabPanel("Single-entry Calculator", fluidRow(column(12,
                                          h3("Data input"),
                                          h5("Enter data values, separated by commas, spaces, or tabs."),
                                          helpText("The default values are for Participant 1 from Thorne and Kamps (2008). Click \"Estimate Model\" without entering your own values to see example output."))),
                          
                          fluidRow(
                                column(4, 
                                   textInput("trt", label = "Treatment Assignment", value = "0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1")),
                                column(4, 
                                       textInput("outcome", label = "Outcome", value = "16.93 18.47 24.03 6.84 10.47 4.70 5.77 7.69 1.92 1.71 2.78 1.71 4.49 0.85 3.28 8.30 13.97 21.62 35.59 18.39 11.01 3.85 4.26 5.98 3.58 3.35 5.93 3.31 5.67 3.28 3.69"),
                                       checkboxInput("perctoprop", label = "Transform percent to proportion? (Necessary for binomial or quasi-binomial link)")),
                                column(4,
                                       textInput("m", label = "m (# of treatment sessions)", value = "10"))),
                         hr(),
                          sidebarLayout(
                            sidebarPanel(width = 4,
                                        h4("Modeling"),
                                        selectInput("family", label = "Variance Function",
                                                              choices = c("quasi-binomial" = "quasibinomial","quasi-Poisson" = "quasipoisson", "binomial", "gaussian", "poisson"), 
                                                              selected = "quasipoisson"),
                                        selectInput("link", label = "Link Function",
                                                            choices = c("log", "logit", "identity"),selected = "log"),
                                        actionButton("Estimate", "Estimate Model")
  
                                         ),
                            mainPanel(width = 8,
                                      plotOutput("modelplot"),
                                      br(),
                                      tableOutput("single_table")
                                      )
                            )),
                tabPanel("Batch Entry",
                         tabsetPanel(
                         tabPanel("Data",
                         sidebarLayout(sidebarPanel(
                                              radioButtons('dat_type', 'What data do you want to use?', 
                                                             c("Use an example" = "example", "Upload data from a file" = "dat")),
                                                conditionalPanel(
                                                  condition = "input.dat_type == 'example'",
                                                  selectInput("example", label = "Choose an example", 
                                                              choices = example_list)
                                                ),
                                                conditionalPanel(
                                                  condition = "input.dat_type == 'dat'",
                                                  fileInput('dat', 'Upload a .csv or .txt file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.txt')),
                                                  checkboxInput('header', 'File has a header?', TRUE),
                                                  radioButtons('sep', 'Data seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' ')),
                                                  radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"))
                                                  )),
                                       mainPanel(tableOutput("datview"))
                                         
                           )),
                tabPanel("Estimate", 
                         sidebarLayout(
                           sidebarPanel(
                              uiOutput("variableMapping"),
                              downloadButton("downcsv", "Download displayed results")
                           ),
                           mainPanel(tableOutput("batchTable"))))
               
)
)
)