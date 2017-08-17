library(shiny)
library(ggplot2)

beerData <- read.csv("bd.csv")

shinyUI(fluidPage(
  
  titlePanel(""),
  
  navbarPage("",
             
             tabPanel("Overview",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          helpText("Introduction to what the tabs does..."),
                          
                          radioButtons("dimension", "Dimension type:",
                                       c("Type" = "type",
                                         "Sortiment" = "sort")),
                          
                          uiOutput("ui")
                          
                          ),
                        
                        mainPanel(
                          
                          h3("Yearly New Products"),
                          plotOutput("plot1")
                          
                        )
                      )
             ),
             
             
             tabPanel("Overview",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          helpText("Introduction to what the tabs does..."),
                          
                          selectInput("typx", "Select Company", choices = paste(beerData$Typ))
                          
                        ),
                        
                        mainPanel(
                          
                          h3("All IPOs - Bag of Words"),
                          plotOutput("plot2")
                          
                        )
                      )
             ),
  
             tabPanel("Overview",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          helpText("Introduction to what the tabs does..."),
                          
                          selectInput("typ3", "Select Company", choices = paste(beerData$Typ))
                          
                        ),
                        
                        mainPanel(
                          
                          h3("All IPOs - Bag of Words"),
                          plotOutput("plot3")
                          
                        )
                      )
             )
      
    )
   )
  )
  
  
  
  
 



