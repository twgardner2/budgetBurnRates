

# Define UI for application that draws a histogram
shinyUI(navbarPage("SOCFWD-NWA Expenditures Analysis Tool",
                   tabPanel(title = "Directorate Expenditures",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "dir",
                                            label = "Select Directorate:",
                                            choices = c("SOCFWD-NWA", sort(wbsList$dir))),
                                radioButtons(inputId = "aggregate",
                                             label = "Table Aggregation Timeframe:",
                                             choices = c("Monthly" = "fiscalMonth", "Quarterly" = "fiscalQtr")),
                                uiOutput(outputId = "commitmentItemCheckBoxes"),
                                downloadButton(outputId = "downloadTable",
                                               label    = "Download Data")
                                ),
                              mainPanel(plotOutput(outputId = "burnRatePlot"),
                                        #textOutput(outputId = "aggregateText"), ### TROUBLESHOOTING
                                        dataTableOutput(outputId = "burnRateTable"),
                                        #dataTableOutput(outputId = "tableDataTable") ### TROUBLESHOOTING
                                        plotOutput(outputId = "stackedBurnRatePlot")
                                        )
                              )
                            ),
                   tabPanel(title = "Commitment Item Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "commitItemGroup",
                                            label = "Select Commitment Item Group:",
                                            choices = c("item1","item2","item3" ) ##############################################
                                )
                              ),
                              mainPanel(

                                )
                              )
                            )
                   )
        )