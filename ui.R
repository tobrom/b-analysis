library(shiny)
library(ggplot2)
library(stringr)

beerData <- read.csv("bd.csv")
beerData$Alkoholhalt <- as.numeric(str_replace_all(beerData$Alkoholhalt, "%", ""))

shinyUI(fluidPage(
  
  titlePanel(""),
  
  navbarPage("",
             
             tabPanel("Trends",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          helpText("Here you can view the development in new
                                    new beers both in terms  of type of beer 
                                    and per assortment."),
                          
                          radioButtons("dimension", "Dimension type:",
                                       c("Type" = "type",
                                         "Assortment" = "sort")),
                          uiOutput("ui")
                          ),
                        mainPanel(
                          h3("Number of New Beers per Year"),
                          plotOutput("plot1", height = "700px")
                        )
                      )
             ),
             
             tabPanel("Price",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          helpText("Here you can look at the distribution of
                                   beer prices per type. In addition it is 
                                   possible to filter based on the assortment."),
                          
                          selectizeInput('assortment_price', 
                                         label = NULL, choices = paste(beerData$SortimentText),
                                         options = list(maxItems = length(unique(beerData$SortimentText)), 
                                                        placeholder = 'Select Assortment (s)...')),
                          
                          sliderInput("range_price", "Range:",
                                      min = 0, max = round(max(beerData$PrisPerLiter), 0),
                                      value = c(0, round(max(beerData$PrisPerLiter)), 0))
                        ),
                        
                        mainPanel(
                          plotOutput("plot2", height = "700px")
                        )
                      )
             ),
             
             tabPanel("Alcohol",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          helpText("Here you can look at the distribution of
                                   alcohol levels per type. In addition it is 
                                   possible to filter based on the assortment."),
                          
                          selectizeInput('assortment_alcohol', 
                                         label = NULL, choices = paste(beerData$SortimentText),
                                         options = list(maxItems = length(unique(beerData$SortimentText)), 
                                                        placeholder = 'Select Assortment (s)...'))
                          
                          ),
                        mainPanel(
                        plotOutput("plot3", height = "700px")
                     )
                  )
                ),
             
             tabPanel("Taste",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          helpText("Introduction to what the tabs does...")
                          
                        ),
                        
                        mainPanel(
                        
                          plotOutput("plotxxx")
                        )
                      )
             ),
             
             tabPanel("Recommendation",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          helpText("Introduction to what the tabs does..."),
                          
                          selectizeInput('beer', label = "dsdsd", choices = paste(beerData$Namn),
                            options = list(maxItems = 1, placeholder = 'select a state name'))
                          
                        ),
                        
                        mainPanel(
                   
                          plotOutput("plotyy")
                      )
                    )
                  )
             )
           )
        )
  
  
  
  
 



