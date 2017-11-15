library(shiny)
library(quantmod)

lista <- read.csv(paste0("C:/projektIE/player_name.csv"),header = FALSE)

shinyUI(fluidPage(
  titlePanel("Statystyki zawodniczek tenisa"),
  tabsetPanel(
    tabPanel("Statystyki wieku zaowodniczek",
             
             sidebarLayout(
               sidebarPanel(
                 textOutput("textWIEK")
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
    )
    )
  )
)


