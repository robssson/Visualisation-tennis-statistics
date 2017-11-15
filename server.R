#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
debounce <- function(expr, millis, env = parent.frame(), quoted = FALSE,
                     domain = getDefaultReactiveDomain()) {
  
  force(millis)
  
  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))
  
  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )  
  
  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)
  
  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()
    
    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })
  
  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}
library(shiny)

getData<-function(name){
  MyData <- read.csv(paste0("C:/projektIE/",name,".csv"),header=TRUE,sep=",")
}

intensywnosc<-function(name,przedzialOD,przedzialDO){
  przedzial1=0
  for(j in 2002:2016){
    dane<-getData(paste0("wta_matches_",j))
    dlugosc<-dim(dane)
    for(i in 1:dlugosc[1]){
      a<-dane[i,11:12]
      if(a[1]==name){
        b<-dane[i,6:7]
        #mc = szerokosc %% 12
        #lata=as.integer(szerokosc/12)
        #c<-przedzialOD+szerokosc*100+lata*10000
        if(b[1]>przedzialOD&&b[1]<przedzialDO)
        {
          przedzial1= przedzial1+1
        }
      }
      
    }
  }
  przedzial1
}
statystyki<-function(name){
  wygrane = 0
  przegrane = 0
  twarda = 0
  ziemia = 0
  dywan = 0
  trawa = 0
  for(j in 2002:2016){
    dane<-getData(paste0("wta_matches_",j))
    dlugosc<-dim(dane)
    for(i in 1:dlugosc[1]){
      a<-dane[i,11:12]
      if(a[1]==name){
        wygrane = wygrane+1
        b<-dane[i,3:4]
        if(b[1]=="Hard")
        {
          twarda = twarda +1
        }
        if(b[1]=="Clay")
        {
          ziemia = ziemia +1
        }
        if(b[1]=="Carpet")
        {
          dywan = dywan +1
        }
        if(b[1]=="Grass")
        {
          trawa = trawa +1
        }
      }
      a<-dane[i,21:22]
      if(a[1]==name){
        przegrane = przegrane+1
        
        
      }
    }
  }
  wynik = c(wygrane,przegrane,twarda,ziemia,dywan,trawa)
  names(wynik)<- c("wygrane","przegrane","twardaWYG","ziemiaWYG","dywanWYG","trawaWYG")
  wynik
}
wiek<-function(){ 
  wiek18=0
  wiek22=0
  wiek27=0
  wiek32=0
  wiek33=0
  for(j in 2002:2016){
    dane<-getData(paste0("wta_matches_",j))
    dlugosc<-dim(dane)
    for(i in 1:dlugosc[1]){
      a<-dane[i,15:16]
      if(is.na(a[1])){
      }
      else{
        if(a[1]<=18){
          wiek18=wiek18+1
        }
        if(a[1]>18 &&a[1]<=23){
          wiek22=wiek22+1
        }
        if(a[1]>23 &&a[1]<=27){
          wiek27=wiek27+1
        }
        if(a[1]>27 &&a[1]<=32){
          wiek32=wiek32+1
        }
        if(a[1]>33){
          wiek33=wiek33+1
        }
      }
      
    }
  }
  
  wiek=c(wiek18,wiek22,wiek27,wiek32,wiek33)
  names(wiek)<- c("<=18","18-22","23-27","28-32","33<")
  wiek
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$agePlot <- renderPlot({
    
    # generate an rnorm distribution and plot it
    #dist <- rnorm(input$obs)
    dist <-wiek()
    pie(dist)
  })
  output$text1 <- renderText({ 
    input$variable4
  })
  output$textWIEK1 <- renderText({ 
    paste("Wiek ponizej 18 lat: ",(wiek()[1]))
  })
  output$textWIEK2 <- renderText({ 
    paste("Wiek w przedziale 18-22 lata: ",(wiek()[2]))
  })
  output$textWIEK3 <- renderText({ 
    paste("Wiek w przedziale 23-27 lat: ",(wiek()[3]))
  })
  output$textWIEK4 <- renderText({ 
    paste("Wiek w przedziale 28-32 lata: ",(wiek()[4]))
  })
  output$textWIEK5 <- renderText({ 
    paste("Wiek powyzej 33 lat: ",(wiek()[5]))
  })
  output$statPlot <- renderPlot({
    
    # generate an rnorm distribution and plot it
    #dist <- rnorm(input$obs)
    
    stat <-statystyki(input$variable4)
    barplot(stat)
  })
   
  output$text1 <- renderText({ 
    input$variable4
  })
  
  
  output$textSTAT1 <- renderText({ 
    paste("Wybrany zawodnik: ", input$variable4)
  })
  
  #output$textSTAT2 <- renderText({
  #    
  #  paste("Wybrany okres: ", lata, " lat i ",miesiace," miesiecy")
  #})
  debounced <- debounce(input$variable4, 10000)
  output$textSTAT3 <- renderPrint({ 
    debounced()
    od<- 10000*input$binsYOD+100*input$binsMOD
    do<- 10000*input$binsYDO+100*input$binsMDO
    if(do>od){
      intensywnosc(input$variable4,od,do)
    }
    else{
      print("zly przedzial!")
    }
  
  })
})
