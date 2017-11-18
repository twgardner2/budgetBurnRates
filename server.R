library(ggthemes)
library(readxl)
library(lubridate)
library(stringr)
library(tidyverse)
#library(shinyWidgets)
library(shiny)
library(DT)
remove(list = ls())

### Define functions ####
dateToFY <- function(x) {
  ifelse(lubridate::month(x) %in% c(10, 11, 12), lubridate::year(x) + 1, lubridate::year(x))
  }
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
  colnames(x) <- c("wbs", "commitmentItem", "commitmentItemText", "postDate", "obligation")
  
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
  
  # create Commitment Item Group
  x <- x %>% mutate(commitItemGroup = ifelse(str_sub(.$commitmentItem, 1, 1) == "F", "FCM", str_sub(.$commitmentItem, 1, 2)))
  x <- x %>% mutate(commitItemGroup = ifelse(.$commitItemGroup == "21", "21 - Travel", .$commitItemGroup))
  x <- x %>% mutate(commitItemGroup = ifelse(.$commitItemGroup == "22", "22 - Transportation", .$commitItemGroup))
  x <- x %>% mutate(commitItemGroup = ifelse(.$commitItemGroup == "23", "23 - Rental Payments", .$commitItemGroup))
  x <- x %>% mutate(commitItemGroup = ifelse(.$commitItemGroup == "25", "25 - Contracts", .$commitItemGroup))
  x <- x %>% mutate(commitItemGroup = ifelse(.$commitItemGroup == "26", "26 - Supplies/Bulk Fuel", .$commitItemGroup))
  x <- x %>% mutate(commitItemGroup = ifelse(.$commitItemGroup == "31", "31 - Equipment", .$commitItemGroup))
  x <- x %>% mutate(commitItemGroup = ifelse(.$commitItemGroup == "32", "32 - Building Repairs", .$commitItemGroup))
  
  # Rename wbs column
  str_FY <- x[1,]$fiscalYear
  colnames(x)[1] <- str_c("wbs", str_FY)
  
  return(x)
}

roundToHundred <- function(x) {round(x/100)*100}
#####

### Plot helper variables ###
fiscalQtr_lines <- unlist(map(ymd(c("2018-01-01", "2018-04-01", "2018-07-01")), dateToFD))
fiscalMonth_lines <- unlist(map(ymd(c("2017-11-01", "2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01","2018-04-01", 
                                      "2018-05-01", "2018-06-01","2018-07-01", "2018-08-01", "2018-09-01")), dateToFD))


data_file <- "NWA_Directorates_Obligations.xlsx"
#data_file <- "//10.6.100.235/tacnasa1$/Public/J8/Gardner J8/FY18 Budget Forecasting/app/NWA_Directorates_Obligations.xlsx"
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

### Output cleaned data
saveRDS(expData, file="expData.RDS")


shinyServer(function(input, output) {
  
  # Reactive data for plot
  plotData <- reactive({
    
    plotData <- expData
    
    if(input$dir != "SOCFWD-NWA") {plotData <- plotData %>% filter(dir==input$dir)}
    
    #plotData <- plotData %>% filter(commitmentItemText %in% input$commitmentItems) %>%
    plotData <- plotData %>% filter(commitItemGroup %in% input$commitmentItems) %>% 
                             group_by(fiscalYear) %>% 
                             arrange(fiscalDay) %>% 
                             mutate(cum_amount = cumsum(obligation))
  })

  # Reactive data for table
  tableData <- reactive({
    tableData <- expData
    
    if(input$dir != "SOCFWD-NWA") {tableData <- tableData %>% filter(dir == input$dir)}
    
    #tableData %>% filter(commitmentItemText %in% input$commitmentItems) %>%
    tableData %>% filter(commitItemGroup %in% input$commitmentItems) %>% 
                  arrange(fiscalDay) %>% 
                  group_by_(input$aggregate) %>%
                  summarize(FY17     = sum(obligation[fiscalYear==2017]),
                            FY17trav = sum(obligation[fiscalYear==2017 & commitmentItemText == "O/E-TDY Travel"]),
                            FY18     = sum(obligation[fiscalYear==2018]),
                            FY18trav = sum(obligation[fiscalYear==2018 & commitmentItemText == "O/E-TDY Travel"])) %>% 
                  mutate(FY17cum     = cumsum(FY17),
                         FY17travcum = cumsum(FY17trav),
                         FY18cum     = cumsum(FY18),
                         FY18travcum = cumsum(FY18trav)) %>% 
                  select(1, FY17, FY17cum, FY17trav, FY17travcum, FY17travcum, FY18, FY18cum, FY18trav, FY18travcum)
  })
  
  
  # Reactive data for download table (table of all commitItemGroups)
  downloadTableData <- reactive({
    downloadTableData <- expData
    
    if(input$dir != "SOCFWD-NWA") {downloadTableData <- downloadTableData %>% filter(dir == input$dir)}
    
    downloadTableData %>% arrange(fiscalDay) %>% 
      group_by_("fiscalMonth") %>%
      summarize(FY17              = sum(obligation[fiscalYear==2017]),
                FY17_21_trav      = sum(obligation[fiscalYear==2017 & str_sub(commitItemGroup,1,2) == "21"]),
                FY17_22_trans     = sum(obligation[fiscalYear==2017 & str_sub(commitItemGroup,1,2) == "22"]),
                FY17_23_rent      = sum(obligation[fiscalYear==2017 & str_sub(commitItemGroup,1,2) == "23"]),
                FY17_25_contr     = sum(obligation[fiscalYear==2017 & str_sub(commitItemGroup,1,2) == "25"]),
                FY17_26_supp_fuel = sum(obligation[fiscalYear==2017 & str_sub(commitItemGroup,1,2) == "26"]),
                FY17_31_equip     = sum(obligation[fiscalYear==2017 & str_sub(commitItemGroup,1,2) == "31"]),
                FY17_32_bldg_rep  = sum(obligation[fiscalYear==2017 & str_sub(commitItemGroup,1,2) == "32"]),
                FY17_31_fcm       = sum(obligation[fiscalYear==2017 & str_sub(commitItemGroup,1,2) == "FC"]),
                FY18              = sum(obligation[fiscalYear==2018]),
                FY18_21_trav      = sum(obligation[fiscalYear==2018 & str_sub(commitItemGroup,1,2) == "21"]),
                FY18_22_trans     = sum(obligation[fiscalYear==2018 & str_sub(commitItemGroup,1,2) == "22"]),
                FY18_23_rent      = sum(obligation[fiscalYear==2018 & str_sub(commitItemGroup,1,2) == "23"]),
                FY18_25_contr     = sum(obligation[fiscalYear==2018 & str_sub(commitItemGroup,1,2) == "25"]),
                FY18_26_supp_fuel = sum(obligation[fiscalYear==2018 & str_sub(commitItemGroup,1,2) == "26"]),
                FY18_31_equip     = sum(obligation[fiscalYear==2018 & str_sub(commitItemGroup,1,2) == "31"]),
                FY18_32_bldg_rep  = sum(obligation[fiscalYear==2018 & str_sub(commitItemGroup,1,2) == "32"]),
                FY18_31_fcm       = sum(obligation[fiscalYear==2018 & str_sub(commitItemGroup,1,2) == "FC"])) %>% 
      mutate(FY17cum     = cumsum(FY17),
             FY17travcum = cumsum(FY17_21_trav),
             FY18cum     = cumsum(FY18),
             FY18travcum = cumsum(FY18_21_trav)) #%>% 
      #select(1, FY17, FY17cum, FY17trav, FY17travcum, FY17travcum, FY18, FY18cum, FY18trav, FY18travcum)
  })
  
  
  # Reactive UI for Commitment Item drop-down
  output$commitmentItemCheckBoxes <- shiny::renderUI({
    
    #choicesData <- expData %>% select(dir, commitmentItemText)
    choicesData <- expData %>% select(dir, commitItemGroup)
    if(input$dir != "SOCFWD-NWA") {choicesData <- choicesData %>% filter(dir == input$dir)}
    #choices <- sort(unique(choicesData$commitmentItemText))
    choices <- sort(unique(choicesData$commitItemGroup))

    checkboxGroupInput(inputId  = "commitmentItems",
                       label    = "Select Commitment Items to Include:",
                       choices  = choices,
                       selected = choices)
  })
  
  # Create comparative burnrate plot
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
  
  # Create FY17 stacked burnrate plot #########################################################
  # output$stackedBurnRatePlot <- renderPlot({
  #   plot_vlines <- switch(input$aggregate,
  #                         "fiscalQtr"   = fiscalQtr_lines,
  #                         "fiscalMonth" = fiscalMonth_lines)
  #   
  #   plot <- ggplot(downloadTableData(), aes(x = fiscalMonth, y = cumOblig))
  #   plot <- plot + geom_area(aes(color = dirfy17, fill = dirfy17)) +
  #     theme_light() +
  #     xlab("Day of Fiscal Year") + ylab("Obligations") + labs(title = "FY17 Obligations") +
  #     scale_y_continuous(label = dollar_format())
  #   
  #   plot
  #   
  # })
  
  # Create summary table
  periodLabel <- reactive({
    switch(input$aggregate, 
           "fiscalQtr" = "Fiscal Quarter", 
           "fiscalMonth" = "Fiscal Month")
  })

  output$burnRateTable <- DT::renderDataTable(DT::datatable(tableData(),
                                                            class = 'cell-border stripe',
                                                            colnames = c(periodLabel(),
                                                                         "FY17", "Cum. FY17", "FY17 Travel", "Cum. FY17 Travel",
                                                                         "FY18", "Cum. FY18", "FY18 Travel", "Cum. FY18 Travel"),
                                                            rownames = FALSE,
                                                            options = list(dom    ='t',
                                                                           paging = FALSE)
  ) %>%
    formatCurrency(columns = c('FY17', 'FY17cum','FY17trav','FY17travcum',
                               'FY18', 'FY18cum', 'FY18trav', 'FY18travcum'),
                   digits = 0))
                                              
  # Download handler for directorate spending
  output$downloadTable <- downloadHandler(
    filename = function() {
      str_c(input$dir, "_expenditures.csv")
    },
    content = function(file) {
      write.csv(downloadTableData(), file, row.names = FALSE)
    })
  
  
  
  
  
  
  
  
# ### TROUBLESHOOTING OUTPUTS ####  
#   
#   output$plotDataTable <- renderDataTable({
#     plotData()
#   })
#   
  output$tableDataTable <- renderDataTable({
    downloadTableData()
  })
  
#   output$aggregateText <- renderText(input$simplify)
# ######
  
})
