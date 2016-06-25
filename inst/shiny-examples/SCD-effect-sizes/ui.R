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
                            )
                          ),
                          fluidRow(column(12, 
                            hr(),
                            h3("Effect sizes"))),
                          sidebarLayout(
                            sidebarPanel(width = 4,
                                         tabsetPanel(id = "ES_family", type = "pills",
                                                     tabPanel("Non-overlap", 
                                                              hr(),
                                                              selectInput("NOM_ES", label = "Effect size index",
                                                                          choices = c("IRD","NAP","PAND","PEM","PND","Tau"), 
                                                                          selected = "NAP"),
                                                              selectInput("improvement", label = "Direction of improvement", 
                                                                          choices = c("increase", "decrease"))
                                                     ),
                                                     tabPanel("Parametric", 
                                                              hr(),
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
                                                          )
                            ),
                            mainPanel(width = 8,
                                      verbatimTextOutput("ES")
                            )
                          )
                          
                 ),
                 tabPanel("About",
                          includeMarkdown("markdown/About.md")
                 )
)
