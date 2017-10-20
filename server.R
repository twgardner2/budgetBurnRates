library(ggthemes)
library(readxl)
library(lubridate)
library(DT)
library(stringr)
library(tidyverse)
library(shiny)

remove(list = ls())

### Define functions ####
dateToFY   <- function(x) {ifelse(lubridate::month(x) %in% c(10, 11, 12), lubridate::year(x) + 1, lubridate::year(x))}
# dateToFQ   <- function(x) {
#                   switch(lubridate::quarter(x),
#                     2,
#                     3,
#                     4,
#                     1) 
#                   }
dateToFQ <- function(x) {
  y <- lubridate::quarter(x)
  z <- ifelse(y==1, 2, ifelse(y==2, 3, ifelse(y==3, 4, 1)))
  return(z)
}

###


data_file <- "NWA_Directorates_Obligations_FY17.xlsx"
#data_file <- "//10.6.100.235/tacnasa1$/Public/J8/Gardner/FY18 Budget Forecasting/app/NWA_Directorates_Obligations_FY17.xlsx"
#data_file <- "//10.6.100.235/tacnasa1$/Public/J8/Gardner/FY18 Budget Forecasting/app/data.xlsx"

### Read in data
expData <- read_xlsx(data_file, sheet = 1, trim_ws = TRUE)
wbsList <- read_xlsx(data_file, sheet = 2, trim_ws = TRUE)

### Clean column names
colnames(expData) <- c("wbs", "refDoc", "commitmentItem", "commitmentItemCat", "postDate", "updatedDate", "obligation")
colnames(wbsList) <- tolower(colnames(wbsList))

### Clean data types
expData$postDate    <- lubridate::as_date(expData$postDate)
expData$updatedDate <- lubridate::as_date(expData$updatedDate)

### Join wbs with directorate, calculate additional columns
expData <- dplyr::left_join(expData, wbsList, by = c("wbs" = "wbs"))
expData <- expData %>% filter(commitmentItem != "Result")
expData <- expData %>% mutate(fiscalYear = dateToFY(postDate),
                              fiscalQtr  = dateToFQ(postDate))

### Table of expenses by Quarter
expDataByQtr <- expData %>% mutate(period = lubridate::quarter(.$postDate, with_year = TRUE)) %>% 
                            group_by(directorate, period) %>% 
                            summarize(periodObligation = sum(obligation))
### Table of expenses by Month
expDataByMonth <- expData %>% mutate(period = lubridate::month(.$postDate)) %>% 
                              group_by(directorate, period) %>% 
                              summarize(periodObligation = sum(obligation))


shinyServer(function(input, output) {
   
  
  wbsExpenses <- reactive({
    expData %>% filter(directorate==input$wbs) %>% arrange(postDate) %>% mutate(cum_amount = cumsum(obligation))
  })

  output$burnRatePlot <- renderPlot({
    plot <- ggplot(wbsExpenses(), aes(x=postDate, y=cum_amount)) + 
            geom_step() + 
            theme_fivethirtyeight() +
            scale_y_continuous(label=scales::dollar)
    print(plot)
  })
  
  
  tableData <- reactive({
    x <- switch (input$aggregate,
            "Monthly"   = expDataByMonth,
            "Quarterly" = expDataByQtr
          )    
    x %>% filter(directorate == input$wbs)
  })
  
  output$burnRateTable <- renderDataTable({
    tableData()
    }, options = list(dom='t')  )
    
    # if (input$aggregate == "Quarterly") {
    #   tableData <- displayData() %>% mutate(quarter = lubridate::quarter(.$date, with_year = TRUE)) %>%
    #                                  group_by(quarter) %>%
    #                                  summarize(quarterly_amount = sum(amount))  %>%
    #                                  
    #   tableData
    # } else {
    #   tableData <- displayData() %>% mutate(month = lubridate::month(.$date)) %>%
    #                                  group_by(month) %>%
    #                                  summarize(monthly_amount = sum(amount)) %>%
    #                                  datatable(rownames = FALSE,
    #                                            colnames = c('Month', 'Expenses'),
    #                                            filter = 'none',
    #                                            options = list(pageLength = 12,
    #                                                           dom = 't')) %>%
    #                                  formatCurrency("monthly_amount")
    #   tableData
    # }

  
  
})
