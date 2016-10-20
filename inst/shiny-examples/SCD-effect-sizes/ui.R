library(shiny)

ui <- navbarPage(title = "Single-case effect size calculator",
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
                                                                          selected = "NAP"),
                                                              selectInput("improvement", label = "Direction of improvement", 
                                                                          choices = c("increase", "decrease"))
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
                 tabPanel("About",
                          includeMarkdown("markdown/About.md")
                 )
)
