

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Directorate Budget Burn Rates"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput(inputId = "dir",
                   label = "Select Directorate:",
                   choices = c("SOCFWD-NWA", sort(wbsList$dir)) ),
       radioButtons(inputId = "aggregate",
                    label = "Table Aggregation Timeframe:",
                    choices = c("Monthly" = "fiscalMonth", "Quarterly" = "fiscalQtr")),
       uiOutput(outputId = "commitmentItemCheckBoxes"),
       downloadButton(outputId = "downloadTable",
                      label    = "Download Data")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput(outputId = "burnRatePlot"),
       #textOutput(outputId = "aggregateText"), ### TROUBLESHOOTING
       dataTableOutput(outputId = "burnRateTable"),
       #dataTableOutput(outputId = "tableDataTable") ### TROUBLESHOOTING
       plotOutput(outputId = "stackedBurnRatePlot")
    )
  )
))
