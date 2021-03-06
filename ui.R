# To-do:
#   - Selectize (or otherwise improve) commitItemGroup checkboxes
#   - Fix stacked area graph for directorate expenditures
#   - 

# Define UI for application that draws a histogram
shinyUI(navbarPage("SOCFWD-NWA Expenditures Analysis Tool",
                   ## Directorate Expenditures Tab ##
                   tabPanel(title = "Directorate Expenditures",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "dir",
                                            label = "Select Directorate:",
                                            choices = c("SOCFWD-NWA", sort(wbsList$dir))),
                                radioButtons(inputId = "aggregate",
                                             label = "Table Aggregation Timeframe:",
                                             choices = c("Quarterly" = "fiscalQtr", "Monthly" = "fiscalMonth")),
                                uiOutput(outputId = "commitmentItemCheckBoxes"),
                                radioButtons(inputId = "fy_deTab",
                                             label   = "Select Fiscal Year for Stacked Area Chart:",
                                             choices = c("2017", "2018")
                                ),
                                downloadButton(outputId = "downloadTable",
                                               label    = "Download Data")
                                ),
                              mainPanel(plotOutput(outputId = "burnRatePlot"),
                                        dataTableOutput(outputId = "burnRateTable"),
                                        plotOutput(outputId = "stackedBurnRatePlot"),
                                        dataTableOutput(outputId = "stackData_DataTable") ### TROUBLESHOOTING
                                        
                                        )
                              )
                            ),
                   ## Commitment Item Group Analysis Tab ##
                   tabPanel(title = "Commitment Item Group Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons(inputId = "fy_cigaTab",
                                                   label   = "Select Fiscal Year:",
                                                   choices = c("2017", "2018")
                                                   ),
                                uiOutput(outputId = "commitItemGroup_cigaTab_ui"),
                                downloadButton(outputId = "downloadCommitItemByDir",
                                               label    = "Download Data")
                                ),
                              
                              mainPanel(plotOutput(outputId = "commitItemGroupStack_ciga"),
                                        dataTableOutput(outputId = "commitItemDataTable"))
                            )
                      )
            )
)