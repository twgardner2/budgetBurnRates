library(ggthemes)
library(readxl)
library(lubridate)
library(DT)
library(stringr)
library(tidyverse)
library(shiny)

remove(list = ls())

### Define functions ####
dateToFY <- function(x) {ifelse(lubridate::month(x) %in% c(10, 11, 12), lubridate::year(x) + 1, lubridate::year(x))}
dateToFQ <- function(x) {
  y <- lubridate::quarter(x)
  z <- ifelse(y==1, 2, ifelse(y==2, 3, ifelse(y==3, 4, 1)))
  return(z)
}
dateToFM <- function(x) {
  y <- lubridate::month(x)
  switch(y,
         4,5,6,7,8,9,10,11,12,1,2,3)
}
dateToFD <- function(x) {
  y <- lubridate::yday(x)
  
  if(lubridate::leap_year(x)){
    if(y >= 275) {
      return(y-274)
    } else {
      return(y+92)
    }
} else {
    if(y >= 274) {
      return(y-273)
    } else {
      return(y+92)
    }
  }
}

prepFYExp <- function(x) {
  # Rename columns
  colnames(x) <- c("wbs", "commitmentItem", "commitmentItemCat", "postDate", "obligation")
  
  # Remove line items with missing obligation
  x <- x %>% filter(!is.na(obligation))
  
  # Convert dates to date
  x$postDate <- lubridate::as_date(x$postDate)
  
  # Filter out duplicate ('Result') rows
  x <- x %>% filter(commitmentItem != "Result")
  
  # Filter out transactions from previous years
  x <- x %>% filter(dateToFY(postDate) %in% 2017:2018)
  
  # Calculate fiscal periods (year, quarter, month, day)
  x <- x %>% mutate(fiscalYear = dateToFY(postDate),
                    fiscalQtr  = dateToFQ(postDate))
  x$fiscalDay   <- unlist(map(x$postDate,dateToFD))
  x$fiscalMonth <- unlist(map(x$postDate,dateToFM))
  
  # Rename wbs column
  str_FY <- x[1,]$fiscalYear
  colnames(x)[1] <- str_c("wbs", str_FY)
  
  return(x)
}

roundToHundred <- function(x) {round(x/100)*100}
#####

### Plot helper variables ####
fiscalQtr_lines <- unlist(map(ymd(c("2018-01-01", "2018-04-01", "2018-07-01")), dateToFD))
fiscalMonth_lines <- unlist(map(ymd(c("2017-11-01", "2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01","2018-04-01", 
                                      "2018-05-01", "2018-06-01","2018-07-01", "2018-08-01", "2018-09-01")), dateToFD))
#####

data_file <- "NWA_Directorates_Obligations.xlsx"
#data_file <- "//10.6.100.235/tacnasa1$/Public/J8/Gardner/FY18 Budget Forecasting/app/NWA_Directorates_Obligations.xlsx"
#data_file <- "//10.6.100.235/tacnasa1$/Public/J8/Gardner/FY18 Budget Forecasting/app/data.xlsx"

### Read in data
exp17Data <- read_xlsx(data_file, sheet = 1, trim_ws = TRUE)
exp18Data <- read_xlsx(data_file, sheet = 2, trim_ws = TRUE)
wbsList <- read_xlsx(data_file, sheet = 3, trim_ws = TRUE)

### Prepare expData
exp17Data <- prepFYExp(exp17Data)
exp18Data <- prepFYExp(exp18Data)

### Clean wbsList
colnames(wbsList) <- tolower(colnames(wbsList))

### Join expenses to dir label
# 2017
wbsList2017 <- wbsList %>% select(wbs2017, dir, dirfy17)
exp17Data <- dplyr::left_join(exp17Data, wbsList2017, by = "wbs2017")
# 2018
wbsList2018 <- wbsList %>% select(wbs2018, dir)
exp18Data <- dplyr::left_join(exp18Data, wbsList2018, by = "wbs2018")

### Combine FY17 and FY18 data into single tibble
expData <- dplyr::bind_rows(exp18Data, exp17Data)

shinyServer(function(input, output) {
  
  # Reactive data for plot
  plotData <- reactive({
    
    plotData <- expData
    
    if(input$dir != "SOCFWD-NWA") {plotData <- plotData %>% filter(dir==input$dir)}
    
    plotData <- plotData %>% filter(commitmentItemCat %in% input$commitmentItems) %>% 
                             group_by(fiscalYear) %>% 
                             arrange(fiscalDay) %>% 
                             mutate(cum_amount = cumsum(obligation))
  })

  # Reactive data for table
  tableData <- reactive({
    
    tableData <- expData
    
    if(input$dir != "SOCFWD-NWA") {tableData <- tableData %>% filter(dir == input$dir)}
    
    tableData %>% filter(commitmentItemCat %in% input$commitmentItems) %>% 
                  arrange(fiscalDay) %>% 
                  group_by_(input$aggregate) %>%
                  summarize(FY17 = sum(obligation[fiscalYear==2017]),
                            FY18 = sum(obligation[fiscalYear==2018])) %>% 
                  mutate(FY17cum = cumsum(FY17),
                         FY18cum = cumsum(FY18)) %>% 
                  select(1, FY17, FY17cum, FY18, FY18cum)
    
    # if(input$simplify) {
    #   tableData <- tableData %>% mutate(FY17 = map(tableData$FY17,roundToHundred))
    # }
    
  })
  
  # Reactive UI for Commitment Item drop-down
  output$commitmentItemDropDown <- shiny::renderUI({
    
    choicesData <- expData %>% select(dir, commitmentItemCat)
    if(input$dir != "SOCFWD-NWA") {choicesData <- choicesData %>% filter(dir == input$dir)}
    choices <- sort(unique(choicesData$commitmentItemCat))
    
    checkboxGroupInput(inputId  = "commitmentItems",
                       label    = "Select Commitment Items to Include:",
                       choices  = choices,
                       selected = choices)
  })
  
  # Create plot
  output$burnRatePlot <- renderPlot({
    
    plot_vlines <- switch(input$aggregate,
                          "fiscalQtr"   = fiscalQtr_lines,
                          "fiscalMonth" = fiscalMonth_lines)
    
    plot <- ggplot(plotData(), aes(x=fiscalDay, y=cum_amount, group=fiscalYear, color=factor(fiscalYear))) + 
            scale_colour_manual(values = c("red", "blue")) + 
            geom_step(size=1.25) + 
            theme_minimal() +
            theme(legend.position = c(0.95, 0.50)) +
            ylab(label = "Obligations") + 
            xlab(label = "Day of Fiscal Year") + 
            labs(color = "Fiscal Year") +
            scale_y_continuous(label=scales::dollar) +
            geom_vline(xintercept = plot_vlines) 
            
    print(plot)
  })
  
  # Create summary table
  periodLabel <- reactive({
    switch(input$aggregate, 
           "fiscalQtr" = "Fiscal Quarter", 
           "fiscalMonth" = "Fiscal Month")
  })
  output$burnRateTable <- DT::renderDataTable(DT::datatable(tableData(),
                                                            colnames = c(periodLabel(), 
                                                                         "FY17", 
                                                                         "Cumulative FY17", 
                                                                         "FY18", 
                                                                         "Cumulative FY18"),
                                                            rownames = FALSE,
                                                            options = list(dom='t',
                                                                           paging   = FALSE
                                                                           )
                                                            ) %>% 
                                                formatCurrency(columns = c('FY17', 
                                                                           'FY18',
                                                                           'FY17cum', 
                                                                           'FY18cum'),
                                                               digits = 0)
                                              )
                                              

# ### TROUBLESHOOTING OUTPUTS ####  
#   
#   output$plotDataTable <- renderDataTable({
#     plotData()
#   })
#   
#   output$aggregateText <- renderText(input$simplify)
# ######
  
})
