library(shiny)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)
library(plotly)

shinyServer(function(input, output) {
  
  beerData <- reactive({
    
    read.csv("beerData.csv")
    #beerData <- read.csv("beerData.csv")
    
  })
  
  
  descriptionData <- reactive({
    
    read.csv("descriptions.csv")
    #descriptionData <- read.csv("descriptions.csv")
  })
  
  
  aggData <- reactive({
    
    read.csv("aggBeerData.csv", encoding = "UTF-8")
    
    aggData <- read.csv("aggBeerData.csv", encoding = "UTF-8")
  })
  
  
  desMatrix <- reactive({
  
  aggData()$cleanText %>%
   iconv(from = "UTF-8", to = "latin1") %>%
    VectorSource() %>%
    Corpus() %>%
    tm_map(content_transformer(tolower)) %>%                            
    tm_map(removePunctuation) %>%                                         
    tm_map(removeNumbers) %>%
    tm_map(removeWords, iconv(stopwords("sv"), from = "UTF-8", to = "latin1")) %>%
    DocumentTermMatrix(control = list(weighting = weightTfIdf)) %>%
    as.matrix() %>%
    dist(diag = TRUE, upper = TRUE) %>%
    as.matrix()
  
  })
 
  
  tasteMatrix <- reactive({ 
  
   aggData() %>%
    select(tasteBeska, tasteFyllighet, tasteSotma) %>%
    dist(diag = TRUE, upper = TRUE) %>%
    as.matrix()
  
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
        summarise(Nya = length(FullNamn)) %>%
        ggplot(aes(x = year, y = Nya, fill = Typ)) + 
        geom_bar(stat = "identity") +
        labs(x = "Year", y = "New Products") +
        scale_fill_manual(values = fall.colors) +
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
              panel.grid.major.x = element_line(colour = "grey", size = 0.1),
              legend.position = "bottom",
              legend.title = element_blank())
      
    } else {
      
      req(input$sort1)
      
      types <- if(input$sort1 == "***") {unique(beerData()$SortimentText)} else {input$sort1}
      
      beerData() %>% 
        filter(year >= 1990) %>%
        filter(SortimentText %in% types) %>%
        group_by(year, SortimentText) %>% 
        summarise(Nya = length(FullNamn)) %>%
        ggplot(aes(x = year, y = Nya, fill = SortimentText)) + 
        geom_bar(stat = "identity") +
        labs(x = "Year", y = "New Products") +
        scale_fill_manual(values = fall.colors) +
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
              panel.grid.major.x = element_line(colour = "grey", size = 0.1),
              legend.position = "bottom",
              legend.title = element_blank())
      
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
      scale_fill_manual(values = fall.colors) +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            axis.text.x = element_text(size = 12, angle = 90),
            axis.title.y = element_text(size = 14),
            panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
            panel.grid.major.x = element_line(colour = "grey", size = 0.1),
            legend.position = "bottom",
            legend.title = element_blank())
    
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
      scale_fill_manual(values = fall.colors) +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            axis.text.x = element_text(size = 12, angle = 90),
            axis.title.y = element_text(size = 14),
            panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
            panel.grid.major.x = element_line(colour = "grey", size = 0.1),
            legend.position = "bottom",
            legend.title = element_blank())
    
  })
  
  
  output$plot4 <- renderPlotly({
    
    descriptionData() %>%
      inner_join(beerData(), by = "nr") %>%
      filter(Typ %in% input$type_taste) %>%
      
      plot_ly(type = "scatter3d", 
              x = ~tasteBeska, 
              y = ~tasteFyllighet,
              z = ~tasteSotma,
              text = ~FullNamn,
              color = ~as.factor(Typ),
              colors = fall.colors,
              mode = "markers") %>%
      layout(scene = list(xaxis = list(title = 'Beskhet'),
                          yaxis = list(title = 'Fyllighet'),
                          zaxis = list(title = 'Sötma'))) 
    
  })
  
  
  output$plot5 <- renderPlot({
    
    if (is.null(input$type_description)) {
      
      return(NULL)
    }
    
    
    bag <- descriptionData() %>%
      inner_join(beerData(), by = "nr") %>%
      filter(Typ %in% input$type_description)
    
    bagWords <- bag$cleanText %>%
      VectorSource() %>%
      Corpus() %>%
      tm_map(content_transformer(tolower)) %>%                            
      tm_map(removePunctuation) %>%                                         
      tm_map(removeNumbers) %>%
      tm_map(removeWords, iconv(stopwords("sv"), from = "UTF-8", to = "latin1")) %>%
      TermDocumentMatrix() %>%
      as.matrix() %>%
      rowSums() %>%
      sort(decreasing = TRUE)
    
    
    wordcloud(names(bagWords), bagWords, scale=c(4,0.5), max.words=input$max_words,
              colors=fall.colors) 
    
  })
  
  output$text_type <- renderText({
  
    df <- aggData() %>%
          subset(FullNamn == input$beer)
    
  
    sprintf("%s är en %s med stilen %s. Ölen
            kommer från %s och har en alkoholhalt 
            på %s procent och kostar %s kronor.", 
            df$FullNamn, 
            df$Typ, 
            df$Stil, 
            df$Ursprunglandnamn, 
            df$Alkoholhalt,
            df$Prisinklmoms)
   
   
  })
  
  output$text_type2 <- renderText({
    
    df <- aggData() %>%
      subset(FullNamn == input$beer)
    
 
    sprintf("På en skala från 1 till 12 har ölen en beskhet på %s,
            en fyllighet på %s och en sötma pa %s.", 
            df$tasteBeska, df$tasteFyllighet,
            df$tasteSotma)
    
    
  })
  
  
  output$text_type3 <- renderText({
    
    df <- aggData() %>%
      subset(FullNamn == input$beer)
    
    
    sprintf("%s", df$description)
    
    
  })

  

  
  output$table <- renderTable({
    
    min.taste <- 1
    max.taste <- 12
    
    max.taste.dist <- sqrt(3*((max.taste-min.taste)^2))
    
    ind <- match(input$beer, aggData()$FullNamn)
    
    aggData() %>%
      dplyr::select(Namn = FullNamn, 
                    Typ = Typ,
                    Sortiment = SortimentText,
                    Alkoholhalt = Alkoholhalt, 
                    Pris = Prisinklmoms) %>%
      mutate(desDist = 1-(desMatrix()[,ind]/max(desMatrix()[,ind])),
             tasteDist = 1-(tasteMatrix()[,ind]/max.taste.dist)) %>%
      mutate(Närhet = 100*(desDist+tasteDist)/2) %>%
      dplyr::select(-(desDist:tasteDist)) %>%
      arrange(desc(Närhet)) %>%
      slice(2:6)
    
    
  })
  
  
  
})





