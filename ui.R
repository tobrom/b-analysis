library(shiny)
library(ggplot2)
library(stringr)
library(plotly)

beerData <- read.csv("beerData.csv")
aggData <- read.csv("aggBeerData.csv", encoding = "UTF-8")

shinyUI(fluidPage(
  
  theme = "w3.css",
  
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
                          plotOutput("plot1", height = "600px")
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
                              label = NULL, choices = paste(unique(beerData$SortimentText)),
                              options = list(maxItems = length(unique(beerData$SortimentText)), 
                                             placeholder = 'Select Assortment (s)...')),
               
               sliderInput("range_price", "Range:",
                           min = 0, max = round(max(beerData$PrisPerLiter), 0),
                           value = c(0, round(max(beerData$PrisPerLiter)), 0))
               ),
             
             mainPanel(
               plotOutput("plot2", height = "600px")
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
             plotOutput("plot3", height = "600px")
           )
           )
  ),

tabPanel("Taste",
         
         sidebarLayout(
           
           sidebarPanel(
             
             helpText("Introduction to what the tabs does..."),
             selectizeInput('type_taste', 
                            label = NULL, choices = paste(beerData$Typ),
                            options = list(maxItems = length(unique(beerData$Typ)), 
                                           placeholder = 'Select Type (s)...'))
             
           ),
           
           mainPanel(
             
             plotlyOutput("plot4", height = "600px")
           )
         )
),

tabPanel("Description",
         
         sidebarLayout(
           
           sidebarPanel(
             
             helpText("Introduction to what the tabs does..."),
             selectizeInput('type_description', 
                            label = NULL, choices = paste(beerData$Typ),
                            options = list(maxItems = length(unique(beerData$Typ)), 
                                           placeholder = 'Select Type (s)...')),
             
             sliderInput("max_words", "Select Words to Show",
                         min = 1, max = 50,
                         value = 10)
             
           ),
           
           mainPanel(
             
             plotOutput("plot5", height = "600px")
           )
         )
),

tabPanel("Recommendation",
         
         sidebarLayout(
           
           sidebarPanel(
             
             helpText("Introduction to what the tabs does..."),
             
             selectizeInput('beer', label = "", choices = paste(aggData$FullNamn),
                            options = list(maxItems = 1, placeholder = 'select a state name')),
             hr(),
             h5("Summering"),
             textOutput("text_type"),
             hr(),
             h5("Smak"),
             textOutput("text_type2"),
             hr(),
             h5("Beskrivning"),
             textOutput("text_type3")
             
             
           ),
           
           mainPanel(
             tableOutput("table")
           )
         )
)
  )
  )
  )








