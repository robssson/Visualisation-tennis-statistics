#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
lista <- read.csv(paste0("C:/projektIE/player_name.csv"),header = FALSE)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Aplikacja"),
  tabsetPanel(
    tabPanel("Statystyki wieku zawodniczek",
             
             sidebarLayout(
               sidebarPanel(
                 print("Liczba zwyciestwa w nastepujacych przedzialach:"),
                 textOutput("textWIEK1"),
                 textOutput("textWIEK2"),
                 textOutput("textWIEK3"),
                 textOutput("textWIEK4"),
                 textOutput("textWIEK5")
               ),
               mainPanel(
                 plotOutput("agePlot"))
               
             )
    ),
    tabPanel("Statystyki zawodniczki",
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("variable4",
                             "Nazwa:",
                             
                             list(choices = lista),
                             multiple = FALSE
                 )
               ),
               
               mainPanel(
                 textOutput("text1"),plotOutput("statPlot") )
               
             )
    ),
    tabPanel("Intensywnosc zawodniczki",
             
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Data od:"),
                 sliderInput("binsYOD",
                             "Przedzial od Lata:",
                             min = 2002,
                             max = 2016,
                             value = 2002,
                             step = 1),
                 sliderInput("binsMOD",
                             "Miesiace:",
                             min = 1,
                             max = 12,
                             value = 1),
                 titlePanel("Data do:"),
                 sliderInput("binsYDO",
                             "Lata:",
                             min = 2002,
                             max = 2016,
                             value = 2002,
                             step = 1),
                 
                 sliderInput("binsMDO",
                             "Miesiace:",
                             min = 1,
                             max = 12,
                             value = 1),
                 selectInput("variable4",
                             "Nazwa:",
                             
                             choices = lista
                 )
               ),
               mainPanel(
                 textOutput("textSTAT1"),
                 textOutput("textSTAT3")
               )
    )
  )
  # Sidebar with a slider input for number of bins 
    
     
    
  
    # Show a plot of the generated distribution
    
  )
))
