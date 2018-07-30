library(shiny)
source("mappings.R")

ui <- navbarPage(title = "Single-case effect size calculator",
                 tabPanel("About",
                          navlistPanel(widths = c(3,9),
                                       tabPanel("About", 
                                                includeMarkdown("markdown/About.md")),
                                       tabPanel("Accessing the calculator", 
                                                includeMarkdown("markdown/Accessing.md")),
                                       tabPanel("Using the single-entry calculator", 
                                                includeMarkdown("markdown/using_single.md")),
                                       tabPanel("Using the batch-entry calculator", 
                                                includeMarkdown("markdown/using_batch.md")),
                                       tabPanel("Example data", 
                                                includeMarkdown("markdown/example-data.md"))
                          )),
                 tabPanel("Calculator",
                          fluidRow(column(12,
                                          h3("Data input"),
                                          h5("Enter data values, separated by commas, spaces, or tabs.")
                          )
                          ),
                          fluidRow(
                            column(4, 
                                   textInput("A_dat", label = "Phase A", value = "")
                            ),
                            column(4, 
                                   textInput("B_dat", label = "Phase B", value = "")
                            ),
                            column(4,
                                   checkboxInput("plot","Show graph", value = FALSE)
                            )
                          ),
                          conditionalPanel(condition = "input.plot",
                            fluidRow(
                              column(12,
                                     plotOutput('SCDplot', height = "300px")
                              )
                          )),
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
                                        selectInput("NOM_ES", 
                                                    label = "Effect size index",
                                                    choices = c("IRD",
                                                                "NAP",
                                                                "PAND",
                                                                "PEM",
                                                                "PND",
                                                                "Tau",
                                                                "Tau-U" = "Tau_U"), 
                                                    selected = "NAP"),
                                        selectInput("improvement",
                                                    label = "Direction of improvement",
                                                    choices = c("increase", "decrease"))),
                                        tabPanel("Parametric", 
                                        br(),
                                        selectInput("parametric_ES", 
                                                    label = "Effect size index", 
                                                    choices = c("LOR", "LRRd", "LRRi","SMD"), 
                                                    selected = "LRRd"),
                                        selectInput("improvement",
                                                    label = "Direction of improvement",
                                                    choices = c("increase", "decrease")),
                                        conditionalPanel(condition = "input.parametric_ES == 'SMD'",
                                                        radioButtons("SMD_denom", 
                                                                     label = "Standardized by", 
                                                                     choices = c("baseline SD","pooled SD"))),
                                        conditionalPanel(condition = "input.parametric_ES=='LOR'|input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                         selectInput("outScale", label = "Outcome Scale",
                                                                     choices = c("percentage", 
                                                                                 "proportion",
                                                                                 "count",
                                                                                 "rate",
                                                                                 "other")),
                                                         numericInput("intervals", label = "If observed via interval recording, total intervals per session", value = NULL)),
                                        conditionalPanel(condition = "input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                        numericInput("obslength", label = "Session length (in minutes)", value = NULL)),
                                        conditionalPanel(condition = "input.parametric_ES=='LOR'|input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                        numericInput("lrrfloor", label = "User-specified floor constant",value = NULL))),
                                        conditionalPanel(condition = "input.ES_family=='Parametric'|input.NOM_ES=='NAP'|input.NOM_ES=='Tau'",
                                                        numericInput("confidence", 
                                                                     label = "Confidence level", 
                                                                     value = 95, 
                                                                     min = 0, 
                                                                     max = 100)),
                                         numericInput("digits","Digits",
                                                      value = 3, min = 1, 
                                                      max = 16, step = 1))),
                            mainPanel(width = 8,
                                      htmlOutput("result"),
                                      checkboxInput("explanation", 
                                                    label = "Show methods and references", 
                                                    value = FALSE),
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
                                                                        conditionalPanel("input.parametric_ES == 'LRRi'|input.parametric_ES == 'LRRd'", withMathJax(includeMarkdown("markdown/LRR.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'SMD'", withMathJax(includeMarkdown("markdown/SMD.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'LOR'", withMathJax(includeMarkdown("markdown/LOR.md")))
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
                                         uiOutput("clusterPhase"),
                                         uiOutput("phaseDefine"),
                                         uiOutput("outOrderImp"),
                                         conditionalPanel(condition = "input.bimprovement == 'series'",
                                                          uiOutput("improvementVar")),
                                         hr(),
                                         h4("Select Effect Sizes"),
                                         checkboxGroupInput("bESno", "Non-Overlap Effect Sizes", choices = c("IRD","NAP","PAND","PEM","PND","Tau","Tau-U" = "Tau_U"), inline = TRUE),
                                         checkboxGroupInput("bESpar", "Parametric Effect Sizes", choices = c("LOR", "LRRd", "LRRi" ,"SMD"),inline = TRUE),
                                         conditionalPanel(condition = "input.bESpar.includes('LOR')", HTML("<font color=\"#FF0000\">LOR will only be calculated for outcomes measured as percentages or proportions.</font>")),
                                         conditionalPanel(condition = "input.bESpar.includes('SMD')", 
                                                          radioButtons("bSMD_denom", label = "Standardize SMD ", 
                                                                       choices = c("baseline SD","pooled SD"), inline = TRUE)),
                                         conditionalPanel(condition = "input.bESpar.includes('LRRi') | input.bESpar.includes('LRRd') | input.bESpar.includes('LOR')",
                                                          uiOutput("measurementProc")),
                                         numericInput("bconfidence", label = "Confidence level (for any effect size with standard errors)", value = 95, min = 0, max = 100),
                                         radioButtons("resultsformat", "Long or wide format?", c("Long" = "long", "Wide" = "wide"), inline = TRUE),
                                         conditionalPanel(condition = "input.bESpar.length > 0 || input.bESno.length > 0", 
                                                          actionButton("batchest", "Estimate"),
                                                          p(),
                                                          conditionalPanel(condition = "input.batchest > 0",
                                                                           downloadButton("downloadES", label = "Download results")))
                                       ),
                                       
                                       mainPanel(tableOutput("batchTable")))
                                     
                            )
                          )
                          
                 ))
