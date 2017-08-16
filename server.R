library(shiny)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)


shinyServer(function(input, output) {
   
 
   beerData <- reactive({

    read.csv("bd.csv")
    
    })
   
   
   output$ui <- renderUI({
     
     switch(input$dimension,
            
            "type" = selectInput("typ1", "Select Company", choices = c("***", paste(unique(beerData()$Typ))), selected = "***"),
            
            "sort" =  selectInput("typ2", "Select Company", choices = c("***", paste(unique(beerData()$SortimentText))), selected = "***"))
   }) 
   
   
   
   
   
   
  

output$plot1 <- renderPlot({
  

  if (input$dimension == "type") {
  
  
  
  if (input$typ1 == "***") {
  
  
  beerData() %>% 
    filter(year >= 1990) %>%
    group_by(year, Typ) %>% 
    summarise(Nya = length(Namn)) %>%
    ggplot(aes(x = year, y = Nya, fill = Typ)) + 
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "New Products") +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
          panel.grid.major.x = element_line(colour = "grey", size = 0.1))

  } else { 
  
  
  beerData() %>% 
      filter(year >= 1990) %>%
    filter(Typ == input$typ1) %>%
    group_by(year, Typ) %>% 
    summarise(Nya = length(Namn)) %>%
    ggplot(aes(x = year, y = Nya, fill = Typ)) + 
    geom_bar(stat = "identity") +
      labs(x = "Year", y = "New Products") +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
            panel.grid.major.x = element_line(colour = "grey", size = 0.1))
  
  }
    
    
  } else {
    
    
    if (input$typ2 == "***") {
      
      
      beerData() %>% 
        filter(year >= 1990) %>%
        group_by(year, SortimentText) %>% 
        summarise(Nya = length(Namn)) %>%
        ggplot(aes(x = year, y = Nya, fill = SortimentText)) + 
        geom_bar(stat = "identity") +
        labs(x = "Year", y = "New Products") +
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
              panel.grid.major.x = element_line(colour = "grey", size = 0.1))
      
    } else { 
      
      
      beerData() %>% 
        filter(year >= 1990) %>%
        filter(SortimentText == input$typ2) %>%
        group_by(year, SortimentText) %>% 
        summarise(Nya = length(Namn)) %>%
        ggplot(aes(x = year, y = Nya, fill = SortimentText)) + 
        geom_bar(stat = "identity") +
        labs(x = "Year", y = "New Products") +
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
              panel.grid.major.x = element_line(colour = "grey", size = 0.1))
      
    }
    
    
  }
    
    
    
    
})





})



