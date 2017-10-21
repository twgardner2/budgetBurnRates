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

### Clean wbs 
colnames(wbsList) <- tolower(colnames(wbsList))


### Join expenses to directorate label
wbsList2017 <- wbsList %>% select(wbs2017, dir, dirfy17)
exp17Data <- dplyr::left_join(exp17Data, wbsList2017, by = "wbs2017")

wbsList2018 <- wbsList %>% select(wbs2018, dir)
exp18Data <- dplyr::left_join(exp18Data, wbsList2018, by = "wbs2018")

### Combine FY17 and FY18 data into single tibble
expData <- dplyr::bind_rows(exp18Data, exp17Data)

shinyServer(function(input, output) {
  
  wbsExpenses <- reactive({
    
    if(input$wbs == "SOCFWD-NWA") {
      expData %>% group_by(fiscalYear) %>% 
                  arrange(fiscalDay) %>% 
                  mutate(cum_amount = cumsum(obligation))
    } else {
      expData %>% filter(directorate==input$wbs) %>% 
        group_by(fiscalYear) %>% 
        arrange(fiscalDay) %>% 
        mutate(cum_amount = cumsum(obligation))
    }
  })

  tableData <- reactive({
    groupByCol <- switch(input$aggregate,
                         "Monthly"   = "fiscalMonth",
                         "Quarterly" = "fiscalQtr")
    wbsExpenses() %>% group_by_(groupByCol) %>% 
                      summarize(periodObligation = sum(obligation))
  })
  
  output$burnRatePlot <- renderPlot({
    plot <- ggplot(wbsExpenses(), aes(x=fiscalDay, y=cum_amount, group=fiscalYear, color=factor(fiscalYear))) + 
            geom_step(size=1.25) + 
            theme_economist() +
            scale_y_continuous(label=scales::dollar)
    print(plot)
  })
  
  
  # tableData <- reactive({
  #   x <- switch (input$aggregate,
  #           "Monthly"   = expDataByMonth,
  #           "Quarterly" = expDataByQtr
  #         )    
  #   x %>% filter(directorate == input$wbs)
  # })
  
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
