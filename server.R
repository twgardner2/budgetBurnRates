source("setup.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  

  
  
  
  output$burnRatePlot <- renderPlot({
    
    data2 <- data %>% group_by(wbs) %>% arrange(date) %>% mutate(cum_amount = cumsum(amount))
    data2 <- data2 %>% arrange(date) %>% filter(wbs==input$wbs)
    
    plot <- ggplot(data2, aes(x=date, y=cum_amount)) + 
            geom_line() + 
            theme_fivethirtyeight() +
            scale_y_continuous(label=scales::dollar)
        
    print(plot)
    
  })
  
})
