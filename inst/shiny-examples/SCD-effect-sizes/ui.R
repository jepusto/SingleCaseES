library(shiny)

ui <- navbarPage(title = "Single-case effect size calculator",
                 tabPanel("Calculator",
                          
                          fluidRow(
                            column(2),
                            column(4, 
                                   textInput("A_dat", label = h3("Phase A"), value = "Enter data values...")
                            ),
                            column(4, 
                                   textInput("B_dat", label = h3("Phase B"), value = "Enter data values...")
                            ),
                            column(2)
                          ),
                          fluidRow(
                            column(4),
                            column(4,
                                   selectInput("improvement", label = "Direction of improvement", 
                                               choices = c("increase", "decrease"))
                                   ),
                            column(4)
                            
                          ),
                          tabsetPanel(id = "ES_family", type = "tabs",
                                      tabPanel("Non-overlap",
                                               tabsetPanel(id = "ES_panel", type = "tabs",
                                                           tabPanel("IRD"),
                                                           tabPanel("NAP"),
                                                           tabPanel("PND"),
                                                           tabPanel("PEM"),
                                                           tabPanel("PAND"),
                                                           tabPanel("Tau")
                                                          )
                                               ),
                                      tabPanel("Parametric",
                                               tabsetPanel(id = "ES_panel", type = "tabs",
                                                           tabPanel("SMD"),
                                                           tabPanel("LRR")
                                                          )
                                               )
                          )
                 ),
                 tabPanel("About",
                          includeMarkdown("markdown/About.md")
                 )
)
