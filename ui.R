

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Directorate Budget Burn Rates"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput(inputId = "wbs",
                   label = "Select Directorate:",
                   choices = c("SOCFWD-NWA", sort(wbsList$dir)) ),
       radioButtons(inputId = "aggregate",
                    label = "Table Aggregation Timeframe:",
                    choices = c("Quarterly", "Monthly"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("burnRatePlot"),
       dataTableOutput("burnRateTable"),
       dataTableOutput("plotDataTable")
    )
  )
))
