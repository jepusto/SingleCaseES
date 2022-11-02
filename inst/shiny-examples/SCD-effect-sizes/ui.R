library(shiny)
source("mappings.R")

ui <- navbarPage(title = "Single-case effect size calculator",
                 id = "SCD_es_calculator",
                 tabPanel("About",
                          navlistPanel(widths = c(3,9),
                                       tabPanel("About", 
                                                includeMarkdown("markdown/About.md")),
                                       tabPanel("Accessing the calculator", 
                                                includeMarkdown("markdown/Accessing.md")),
                                       tabPanel("Using the single-series calculator", 
                                                includeMarkdown("markdown/using_single_series.md")),
                                       tabPanel("Using the multiple-series calculator", 
                                                includeMarkdown("markdown/using_multiple_series.md")),
                                       tabPanel("Example data", 
                                                includeMarkdown("markdown/example-data.md"))
                          )),
                 tabPanel("Single-Series Calculator",
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
                                                        choices = c("LOR", "LRRd", "LRRi", "LRM", "PoGO", "SMD"), 
                                                        selected = "LRRd"),
                                            conditionalPanel(condition = "input.parametric_ES=='LRRi'|input.parametric_ES=='LRRd'",
                                                             checkboxInput("pct_change","Convert LRR to % change")),
                                            conditionalPanel(condition = "input.parametric_ES=='PoGO'",
                                                             numericInput("goal_level", 
                                                                          label = "Goal level for the behavior", 
                                                                          value = NULL))
                                          )
                              ),
                              selectInput("improvement",
                                          label = "Direction of improvement",
                                          choices = c("increase", "decrease")),
                              conditionalPanel(condition = "input.ES_family=='Non-overlap' & input.NOM_ES == 'Tau_BC'",
                                               radioButtons("tau_calculation",
                                                            label = "Choose a method for calculating Tau index",
                                                            choices = c("Tau (non-overlap)" = "Nlap",
                                                                        "Kendall rank correlation" = "Kendall"))
                              ),
                              conditionalPanel(condition = "input.ES_family=='Non-overlap' & input.NOM_ES == 'Tau_BC'",
                                               radioButtons("baseline_check",
                                                            label = "Test for baseline trend",
                                                            choices = c("Always adjust for baseline trend" = "No", 
                                                                        "Pretest for baseline trend, adjust if significant" = "Yes"))
                              ),
                              conditionalPanel(condition = "input.ES_family=='Non-overlap' & 
                                               input.NOM_ES == 'Tau_BC' & input.baseline_check == 'Yes'",
                                               numericInput("significance_level", 
                                                            label = "Significance level for the initial baseline trend test", 
                                                            value = 0.05, step = .01,
                                                            min = 0.01, 
                                                            max = 0.99)
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
                                                                        conditionalPanel("input.parametric_ES == 'LOR'", withMathJax(includeMarkdown("markdown/LOR.md"))),
                                                                        conditionalPanel("input.parametric_ES == 'PoGO'", withMathJax(includeMarkdown("markdown/PoGO.md")))
                                                       )
                                      )
                            )
                            
                          )
                 ),
                 tabPanel("Multiple-Series Calculator",
                          tabsetPanel(
                            id = "BatchEntryTabs",
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
                                                                  radioButtons('sep', 'Data seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' '), inline = TRUE),
                                                                  radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"), inline = TRUE)
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.dat_type == 'xlsx'",
                                                                  fileInput('xlsx', 'Upload a .xlsx file', accept = c('.xlsx')),
                                                                  checkboxInput('col_names', 'File has a header?', TRUE),
                                                                  selectInput("inSelect", "Select a sheet", "")
                                                                ),
                                                                uiOutput("filtervarMapping"),
                                                                uiOutput("filterMapping")
                                                                ),
                                                   mainPanel(tableOutput("datview")))
                                     ),
                            tabPanel("Variables",
                                     sidebarLayout(
                                       sidebarPanel(
                                         style = "max-height: 800px; overflow-y: auto",
                                         conditionalPanel(condition = "input.dat_type == 'dat' | input.dat_type == 'xlsx'",
                                                          checkboxInput("calcPhasePair", "Calculate phase pair numbers for ABAB designs.", value = FALSE)),
                                         uiOutput("clusterPhase"),
                                         uiOutput("baseDefine"),
                                         uiOutput("treatDefine"),
                                         uiOutput("outOrderImp"),
                                         conditionalPanel(condition = "input.bimprovement == 'series'",
                                                          uiOutput("improvementVar")),
                                         br(),
                                         br(),
                                         br()
                                       ),
                                       mainPanel(tableOutput("datview2"))
                                     )
                                     ),
                            tabPanel("Plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         style = "max-height: 800px; overflow-y: auto",
                                         uiOutput("facetSelector"),
                                         uiOutput("graph_filters"),
                                         br(),
                                         br(),
                                         br(),
                                         br()
                                       ),
                                       mainPanel(plotOutput('batchPlot', height = "auto"))
                                     )
                                     
                            ),
                            tabPanel("Estimate", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Select Effect Sizes"),
                                         checkboxGroupInput("bESno", "Non-Overlap Effect Sizes", choices = c("IRD","NAP","PAND","PEM","PND","Tau","Tau-BC" = "Tau_BC","Tau-U" = "Tau_U"), inline = TRUE),
                                         checkboxGroupInput("bESpar", "Parametric Effect Sizes", choices = c("LOR", "LRRd", "LRRi", "LRM", "PoGO", "SMD"), inline = TRUE),
                                         conditionalPanel(condition = "input.bESno.includes('Tau_BC')",
                                                          radioButtons("btau_calculation", label = "Choose a method for calculating Tau index",
                                                                       choices = c("Tau (non-overlap)" = "Nlap",
                                                                                   "Kendall rank correlation" = "Kendall"),
                                                                       inline = TRUE)),
                                         conditionalPanel(condition = "input.bESno.includes('Tau_BC')",
                                                          radioButtons("bbaseline_check", label = "Use baseline trend test for Tau-BC?",
                                                                       choices = c("Always adjusting for baseline trend" = "No", 
                                                                                   "Pretest for baseline trend, adjust if significant" = "Yes"), 
                                                                       inline = FALSE)),
                                         conditionalPanel(condition = "input.bESno.includes('Tau_BC') & input.bbaseline_check == 'Yes'",
                                                          numericInput("bsignificance_level",
                                                                       label = "Significance level for the baseline trend test",
                                                                       value = 0.05, step = .01,
                                                                       min = 0.01,
                                                                       max = 0.99)),
                                         conditionalPanel(condition = "input.bESpar.includes('LRRi') | input.bESpar.includes('LRRd') | input.bESpar.includes('LOR')",
                                                          checkboxInput("b_pct_change", "Convert LRR to % change")),
                                         conditionalPanel(condition = "input.bESpar.includes('LOR')", 
                                                          HTML("<font color=\"#FF0000\">LOR will only be calculated for outcomes measured as percentages or proportions.</font>")),
                                         conditionalPanel(condition = "input.bESpar.includes('SMD')", 
                                                          radioButtons("bSMD_denom", label = "Standardize SMD ", 
                                                                       choices = c("baseline SD" = "baseline", "pooled SD" = "pool"), inline = TRUE)),
                                         conditionalPanel(condition = "input.bESpar.includes('LRRi') | input.bESpar.includes('LRRd') | input.bESpar.includes('LOR')",
                                                          uiOutput("measurementProc")),
                                         conditionalPanel(condition = "input.bESpar.includes('PoGO')", uiOutput("goalLevel")),
                                         conditionalPanel(condition = "input.b_aggregate != ''", 
                                                          radioButtons('weighting_scheme',
                                                                       label = "Weighting scheme to use for aggregating.",
                                                                       choices = c("equal", "1/V", "nA", "nB", "nA*nB", "1/nA + 1/nB"))
                                                          ),
                                         numericInput("bconfidence", label = "Confidence level (for any effect size with standard errors)", value = 95, min = 0, max = 100),
                                         numericInput("bdigits","Digits",
                                                      value = 2, min = 1, 
                                                      max = 16, step = 1),
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
                                     rclipboard::rclipboardSetup(),
                                     uiOutput("clip"),
                                     verbatimTextOutput("syntax")
                            )
                          )
                          
                 ))
