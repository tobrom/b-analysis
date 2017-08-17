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
            
            "type" =  selectInput("typ1", "Select Compan", 
                                  choices = c("A", paste(unique(beerData()$Typ))), selected = "A"),
            "sort" =  selectInput("sort1", "Select Compan", 
                                  choices = c("A", paste(unique(beerData()$SortimentText))), selected = "A"))
   }) 
   
   
   
   
   
  

output$plot1 <- renderPlot({


  if (input$dimension == "type") { 
    
    req(input$typ1)
    
   types <- if(input$typ1 == "A") {unique(beerData()$Typ)} else {input$typ1}
   
 # types <- unique(beerData()$SortimentText)
  
   beerData() %>% 
     
     filter(year >= 1990) %>%
     filter(Typ %in% types) %>%
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
    
    req(input$sort1)
    
    types <- if(input$sort1 == "A") {unique(beerData()$SortimentText)} else {input$sort1}
    
    # types <- unique(beerData()$SortimentText)
    
    beerData() %>% 
      
      filter(year >= 1990) %>%
      filter(SortimentText %in% types) %>%
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
  
    
})

})



