source("setup.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  displayData <- reactive({
    
    data2 <- data %>% group_by(wbs) %>% arrange(date) %>% mutate(cum_amount = cumsum(amount))
    data2 <- data2 %>% arrange(date) %>% filter(wbs==input$wbs)
    
  })
  
  
  
  output$burnRatePlot <- renderPlot({
  
    plot <- ggplot(displayData(), aes(x=date, y=cum_amount)) + 
            geom_line() + 
            theme_fivethirtyeight() +
            scale_y_continuous(label=scales::dollar)
        
    print(plot)
    
  })
  
  output$burnRateTable <- renderDataTable({
    
    if (input$aggregate == "Quarterly") {
      
      tableData <- displayData() %>% mutate(quarter = lubridate::quarter(.$date, with_year = TRUE)) %>% 
                                     group_by(quarter) %>% 
                                     summarize(quarterly_amount = sum(amount))  %>% 
                                     datatable(rownames = FALSE,
                                               colnames = c('Quarter', 'Expenses'),
                                               filter = 'none',
                                               options = list(pageLength = 4,
                                                              dom = 't')) %>%  
                                     formatCurrency("quarterly_amount")
      tableData
      
    } else {
      
      tableData <- displayData() %>% mutate(month = lubridate::month(.$date)) %>% 
                                     group_by(month) %>% 
                                     summarize(monthly_amount = sum(amount)) %>% 
                                     datatable(rownames = FALSE,
                                               colnames = c('Month', 'Expenses'),
                                               filter = 'none',
                                               options = list(pageLength = 12,
                                                              dom = 't')) %>% 
                                     formatCurrency("monthly_amount")
      tableData
      
    }
    
  })
  
})
