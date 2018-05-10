library(shiny)
source("mappings.R")

ui <- navbarPage(title = "Single-case effect size calculator",
                 tabPanel("About",
                          navlistPanel(widths = c(3,9),
                                       tabPanel("About", includeMarkdown("markdown/About.md")),
                                       tabPanel("Accessing the calculator", includeMarkdown("markdown/Accessing.md")),
                                       tabPanel("Using the single-entry calculator", includeMarkdown("markdown/using_single.md")),
                                       tabPanel("Using the batch-entry calculator", includeMarkdown("markdown/using_batch.md")),
                                       tabPanel("Example data", includeMarkdown("markdown/example-data.md"))
                 )),
                 tabPanel("Calculator",
                          fluidRow(column(12,
                                          h3("Data input"),
                                          h5("Enter data values, separated by commas, spaces, or tabs.")
                          )
                          ),
                          fluidRow(
                            column(4, 
                                   textInput("A_dat", label = "Phase A", value = ""),
                                   br(),
                                   selectInput("improvement", label = "Direction of improvement",
                                               choices = c("increase", "decrease"))
                            ),
                            column(4, 
                                   textInput("B_dat", label = "Phase B", value = ""),
                                   br(),
                                   selectInput("measurementProcedure", label = "Measurement Procedure",
                                               choices = c("continuous recording", "interval recording", "event counting", "other"))
                            ),
                            column(4,
                                   checkboxInput("plot","Show graph", value = FALSE),
                                   br(),
                                   br(),
                                   br(),
                                   selectInput("outScale", label = "Outcome Scale",
                                               choices = c("percentage", "proportion", "count", "rate", "other"))
                            )
                          ),
                          fluidRow(
                            column(12,
                                   plotOutput('SCDplot', height = "auto")
                            )
                          ),
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
                                                              selectInput("NOM_ES", label = "Effect size index",
                                                                          choices = c("IRD","NAP","PAND","PEM","PND","Tau","Tau-U" = "Tau_U"), 
                                                                          selected = "NAP")
                                                                          
                                                     ),
                                                     tabPanel("Parametric", 
                                                              br(),
                                                              selectInput("parametric_ES", label = "Effect size index", 
                                                                          choices = c("LRR","SMD"), selected = "LRR"),
                                                              conditionalPanel(condition = "input.parametric_ES == 'SMD'",
                                                                               radioButtons("SMD_denom", label = "Standardized by", 
                                                                                            choices = c("baseline SD","pooled SD"))
                                                              )
                                                     )
                                         ),
                                         conditionalPanel(condition = "input.ES_family=='Parametric'|input.NOM_ES=='NAP'|input.NOM_ES=='Tau'",
                                                          numericInput("confidence", label = "Confidence level", value = 95, min = 0, max = 100)
                                         ),
                                         numericInput("digits","Digits",value = 3, min = 1, max = 16, step = 1)
                            ),
                            mainPanel(width = 8,
                                      htmlOutput("result"),
                                      checkboxInput("explanation", label = "Show methods and references", value = FALSE),
                                      conditionalPanel("input.explanation==true", 
                                                       conditionalPanel("input.ES_family=='Non-overlap'",
                                                                        conditionalPanel("input.NOM_ES == 'IRD'", withMathJax(includeMarkdown("markdown/IRD.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'NAP'", withMathJax(includeMarkdown("markdown/NAP.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'PAND'", withMathJax(includeMarkdown("markdown/PAND.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'PEM'", withMathJax(includeMarkdown("markdown/PEM.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'PND'", withMathJax(includeMarkdown("markdown/PND.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'Tau'", withMathJax(includeMarkdown("markdown/Tau.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'Tau_U'", withMathJax(includeMarkdown("markdown/Tau-U.md")))
                                                       ),
                                                       conditionalPanel("input.ES_family=='Parametric'",
                                                                        conditionalPanel("input.parametric_ES == 'LRR'", withMathJax(includeMarkdown("markdown/LRR.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'SMD'", withMathJax(includeMarkdown("markdown/SMD.md")))
                                                       )
                                      )
                            )
                            
                          )
                 ),
                 tabPanel("Batch Entry",
                          tabsetPanel(
                            tabPanel("Data",
                                     sidebarLayout(sidebarPanel(radioButtons('dat_type', 'What data do you want to use?', 
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
                                     mainPanel(tableOutput("datview")))),
                            tabPanel("Estimate", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         style = "max-height: 800px; overflow-y: auto",
                                         # uiOutput("indexMapping"),
                                         selectizeInput("b_clusters", label = "Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior).", choices = names(datFile()), 
                                                        selected = NULL, multiple = TRUE),
                                         selectInput("b_phase", label = "Phase Indicator", choices = names(datFile()), selected = names(datFile())[3]),
                                         selectInput("b_base", label = "Baseline Phase Value", choices = unique(datFile()[input$b_phase])),
                                         selectInput("b_treat", label = "Treatment Phase Value", choices = unique(datFile()[input$b_phase])),
                                         selectInput("b_out", label = "Outcome", choices = names(datFile()), selected = names(datFile())[4]),
                                         selectInput("bimprovement", label = "Direction of improvement", choices = c("all increase" = "increase", "all decrease" = "decrease", "by series" = "series")),
                                         conditionalPanel(condition = "input.bimprovement == 'series'",
                                                          selectInput("bseldir", label = "Select variable identifying improvement direction",
                                                                      choices = names(datFile()))),
                                         selectInput("session_number", label = "Within-Case Session Number", choices = names(datFile())),
                                         selectInput("bmeasurementProcedure", label = "Measurement Procedure",
                                                     choices = c("continuous recording", "interval recording", "event counting", "other")),
                                         selectInput("boutScale", label = "Outcome Scale",
                                                     choices = c("percentage", "proportion", "count", "rate", "other")),
                                         hr(),
                                         h4("Select Effect Sizes"),
                                         checkboxGroupInput("bESno", "Non-Overlap Effect Sizes", choices = c("IRD","NAP","PAND","PEM","PND","Tau","Tau-U" = "Tau_U"), inline = TRUE),
                                         checkboxGroupInput("bESpar", "Parametric Effect Sizes", choices = c("LRR","SMD"),inline = TRUE),
                                         conditionalPanel(condition = "input.bESpar.includes('SMD')", 
                                                          radioButtons("bSMD_denom", label = "Standardize SMD ", 
                                                                       choices = c("baseline SD","pooled SD"), inline = TRUE)),
                                         numericInput("bconfidence", label = "Confidence level (for any effect size with standard errors)", value = 95, min = 0, max = 100),
                                         radioButtons("convertWide", "Long or wide format?", c("Long" = FALSE, "Wide" = TRUE), inline = TRUE),
                                         conditionalPanel(condition = "input.bESpar.length > 0 || input.bESno.length > 0", 
                                                          actionButton("batchest", "Calculate"),
                                                          downloadButton("downcsv", "Download displayed results"))
                                         ),
                                         
                                       mainPanel(tableOutput("batchTable")))
                                     
                            )
                          )
                          
                 ))
