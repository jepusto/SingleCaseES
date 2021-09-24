library(shiny)
library(rclipboard)
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
                                                                    "Tau-BC" = "Tau_BC",
                                                                    "Tau-U" = "Tau_U"), 
                                                        selected = "NAP")
                                          ),
                                          tabPanel("Parametric", 
                                            br(),
                                            selectInput("parametric_ES", 
                                                        label = "Effect size index", 
                                                        choices = c("LOR", "LRRd", "LRRi", "LRM", "SMD"), 
                                                        selected = "LRRd"),
                                            conditionalPanel(condition = "input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                             checkboxInput("pct_change","Convert LRR to % change"))
                                          )
                              ),
                              selectInput("improvement",
                                          label = "Direction of improvement",
                                          choices = c("increase", "decrease")),
                              conditionalPanel(condition = "input.ES_family=='Non-overlap' & input.NOM_ES == 'Tau_BC'",
                                               radioButtons("baseline_check",
                                                            label = "Test for baseline trend",
                                                            choices = c("No", "Yes"))
                              ),
                              conditionalPanel(condition = "input.ES_family=='Non-overlap' & 
                                               input.NOM_ES == 'Tau_BC' & input.baseline_check == 'Yes'",
                                               numericInput("significance_level", 
                                                            label = "Significance level for the initial baseline trend test", 
                                                            value = 0.05, 
                                                            min = 0.0000001, 
                                                            max = 0.9999999)
                              ),
                              conditionalPanel("input.ES_family=='Parametric'",
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
                                                  numericInput("lrrfloor", label = "User-specified floor constant",value = NULL))
                              ),
                              conditionalPanel(condition = "input.ES_family=='Parametric'|input.NOM_ES=='NAP'|input.NOM_ES=='Tau'|input.NOM_ES=='Tau_BC'",
                                  numericInput("confidence", 
                                               label = "Confidence level", 
                                               value = 95, 
                                               min = 0, 
                                               max = 100)
                              ),
                              numericInput("digits","Digits",
                                          value = 2, min = 1, 
                                          max = 16, step = 1)
                            ),
                            mainPanel(width = 8,
                                      conditionalPanel(condition = "input.ES_family=='Non-overlap' & input.NOM_ES == 'PND'", 
                                                       includeMarkdown("markdown/PND-message.md")),
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
                                                                        conditionalPanel("input.NOM_ES == 'Tau_BC'", withMathJax(includeMarkdown("markdown/Tau-BC.md"))),
                                                                        conditionalPanel("input.NOM_ES == 'Tau_U'", withMathJax(includeMarkdown("markdown/Tau-U.md")))
                                                       ),
                                                       conditionalPanel("input.ES_family=='Parametric'",
                                                                        conditionalPanel("input.parametric_ES == 'LRRi'|input.parametric_ES == 'LRRd'", withMathJax(includeMarkdown("markdown/LRR.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'LRM'", withMathJax(includeMarkdown("markdown/LRM.md"))),
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
                                                                             c("Use an example" = "example", 
                                                                               "Upload data from a .csv or .txt file" = "dat",
                                                                               "Upload data from a .xlsx file" = "xlsx")),
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
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.dat_type == 'xlsx'",
                                                                  fileInput('xlsx', 'Upload a .xlsx file', accept = c('.xlsx')),
                                                                  checkboxInput('col_names', 'File has a header?', TRUE),
                                                                  selectInput("inSelect", "Select a sheet", "")
                                                                )),
                                                   mainPanel(tableOutput("datview")))
                                     ),
                            tabPanel("Variables",
                                     sidebarLayout(
                                       sidebarPanel(
                                         style = "max-height: 800px; overflow-y: auto",
                                         uiOutput("clusterPhase"),
                                         uiOutput("baseDefine"),
                                         uiOutput("treatDefine"),
                                         uiOutput("outOrderImp"),
                                         conditionalPanel(condition = "input.bimprovement == 'series'",
                                                          uiOutput("improvementVar"))
                                       ),
                                       mainPanel(tableOutput("datview2"))
                                     )
                                     ),
                            tabPanel("Estimate", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Select Effect Sizes"),
                                         checkboxGroupInput("bESno", "Non-Overlap Effect Sizes", choices = c("IRD","NAP","PAND","PEM","PND","Tau","Tau-BC" = "Tau_BC","Tau-U" = "Tau_U"), inline = TRUE),
                                         checkboxGroupInput("bESpar", "Parametric Effect Sizes", choices = c("LOR", "LRRd", "LRRi", "LRM", "SMD"),inline = TRUE),
                                         conditionalPanel(condition = "input.bESno.includes('Tau_BC')", 
                                                          radioButtons("bbaseline_check", label = "Use baseline trend test for Tau-BC?", 
                                                                       choices = c("No", "Yes"), inline = TRUE)),
                                         conditionalPanel(condition = "input.bESno.includes('Tau_BC') & input.bbaseline_check == 'Yes'",
                                                          numericInput("bsignificance_level", 
                                                                       label = "Significance level for the baseline trend test", 
                                                                       value = 0.05, 
                                                                       min = 0.0000001, 
                                                                       max = 0.9999999)),
                                         conditionalPanel(condition = "input.bESpar.includes('LRRi') | input.bESpar.includes('LRRd') | input.bESpar.includes('LOR')",
                                                          checkboxInput("b_pct_change", "Convert LRR to % change")),
                                         conditionalPanel(condition = "input.bESpar.includes('LOR')", 
                                                          HTML("<font color=\"#FF0000\">LOR will only be calculated for outcomes measured as percentages or proportions.</font>")),
                                         conditionalPanel(condition = "input.bESpar.includes('SMD')", 
                                                          radioButtons("bSMD_denom", label = "Standardize SMD ", 
                                                                       choices = c("baseline SD" = "baseline", "pooled SD" = "pool"), inline = TRUE)),
                                         conditionalPanel(condition = "input.bESpar.includes('LRRi') | input.bESpar.includes('LRRd') | input.bESpar.includes('LOR')",
                                                          uiOutput("measurementProc")),
                                         conditionalPanel(condition = "input.b_aggregate.length > 0", uiOutput("weightingScheme")),
                                         numericInput("bconfidence", label = "Confidence level (for any effect size with standard errors)", value = 95, min = 0, max = 100),
                                         radioButtons("resultsformat", "Long or wide format?", c("Long" = "long", "Wide" = "wide"), inline = TRUE),
                                         conditionalPanel(condition = "input.bESpar.length > 0 || input.bESno.length > 0", 
                                                          actionButton("batchest", "Estimate"))
                                       ),
                                       
                                       mainPanel(
                                         conditionalPanel(condition = "input.bESno.includes('PND')", 
                                                          includeMarkdown("markdown/PND-message.md")),
                                         tableOutput("batchTable"),
                                         p(),
                                         conditionalPanel(condition = "input.batchest > 0",
                                                          downloadButton("downloadES", label = "Download results"))
                                       )
                                    )
                                     
                            ),
                            
                            tabPanel("Syntax for R",
                                     rclipboardSetup(),
                                     uiOutput("clip"),
                                     verbatimTextOutput("syntax")
                          )
                          
                      )
                 ))
