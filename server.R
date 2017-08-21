library(shiny)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)

shinyServer(function(input, output) {
   
    beerData <- reactive({

     beerData <- read.csv("bd.csv")
    
     beerData$Alkoholhalt <- as.numeric(str_replace_all(beerData$Alkoholhalt, "%", ""))
     
     beerData 
     
    })
   
  output$ui <- renderUI({
     
     switch(input$dimension,
            
            "type" =  selectInput("typ1", "Select Beer Type", 
                                  choices = c("***", paste(unique(beerData()$Typ))), selected = "***"),
            "sort" =  selectInput("sort1", "Select Assortment Type", 
                                  choices = c("***", paste(unique(beerData()$SortimentText))), selected = "***"))
   }) 
   
   

  output$plot1 <- renderPlot({


    if (input$dimension == "type") { 
    
    req(input$typ1)
    
    types <- if(input$typ1 == "***") {unique(beerData()$Typ)} else {input$typ1}

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
    
    types <- if(input$sort1 == "***") {unique(beerData()$SortimentText)} else {input$sort1}
    
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


  output$plot2 <- renderPlot({
  
  if (is.null(input$assortment_price)) {
    
    return(NULL)
  }
  
    beerData() %>% 
    filter(SortimentText %in% input$assortment_price) %>%
    filter(PrisPerLiter >= min(input$range_price) & PrisPerLiter <= max(input$range_price)) %>%
    ggplot(aes(x = Typ, y = PrisPerLiter, fill = Typ)) + 
    geom_boxplot() +
      labs(x = "", y = "Price per Liter (SEK)") +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            axis.text.x = element_text(size = 12, angle = 90),
            axis.title.y = element_text(size = 14),
            panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
            panel.grid.major.x = element_line(colour = "grey", size = 0.1))

  })


  output$plot3 <- renderPlot({
  
  if (is.null(input$assortment_alcohol)) {
    
    return(NULL)
    }
  
  beerData() %>% 
    filter(SortimentText %in% input$assortment_alcohol) %>%
    ggplot(aes(x = Typ, y = Alkoholhalt, fill = Typ)) + 
    geom_boxplot() +
    labs(x = "", y = "Alcohol (%)") +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_text(size = 12, angle = 90),
          axis.title.y = element_text(size = 14),
          panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
          panel.grid.major.x = element_line(colour = "grey", size = 0.1))
  
  })



})
