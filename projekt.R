getData<-function(name){
  MyData <- read.csv(paste0("C:/projektIE/",name,".csv"),header=TRUE,sep=",")
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




intensywnosc<-function(name,przedzialOD,szerokosc){
  przedzial1=0
  for(j in 2002:2016){
    dane<-getData(paste0("wta_matches_",j))
    dlugosc<-dim(dane)
      for(i in 1:dlugosc[1]){
      a<-dane[i,11:12]
      if(a[1]==name){
      b<-dane[i,6:7]
      mc = szerokosc %% 12
      lata=as.integer(szerokosc/12)
      c<-przedzialOD+szerokosc*100+lata*10000
      if(b[1]>przedzialOD&&b[1]<c)
      {
        przedzial1= przedzial1+1
      }
    }
    
  }
}
  przedzial1
}

View(wiek())
print(wiek())
View(statystyki("Justine Henin"))
View(intensywnosc("Justine Henin",20050101,3))
