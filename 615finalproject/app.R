---
  title: "mapping"
author: "Xinyi Wang,Tianying Xu,Xiangliang Liu,Albert Ding"
date: "11/4/2018"
# output: pdf_document
output: 
  pdf_document: 
  latex_engine: xelatex
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Read & Clean raw data to get Allston area data
```{r}
mydata <- read.csv("mayorsfoodcourt.csv")
library(dplyr)
library(tidyr)
Allston <- mydata %>% 
  filter(ViolStatus=="Fail" &  CITY == "Allston") %>%
  extract(Location, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)") %>%
  filter(!is.na(Latitude))
Allston$Latitude = as.numeric(Allston$Latitude)
Allston$Longitude = as.numeric(Allston$Longitude)
```


##Static Plot
```{r warning=FALSE}
# geocode("allston",source="dsk")
library(ggmap)
library(ggplot2)
# 
# ggmap(get_googlemap())
# register_google(key = "AIzaSyDsMKikezjPO_1HDzS_QpUX1_wC49M006E")
# map <- get_googlemap(center = c(-71.13, 42.355), zoom = 14)
# # map <- get_map(location = c(-71.13, 42.355), zoom = 14,maptype="toner",source="stamen")
# ggmap(map) +
#   geom_point(aes(x = Longitude, y = Latitude,colour=ViolLevel),data=Allston,alpha=0.5)
qmplot(Longitude, Latitude, data = Allston, maptype = "toner-lite", color = I("orange"))
```

##Using Leaflet
```{r out.width = "75%"}
library(leaflet)
library(mapview)
library(knitr)
# webshot:: install_phantomjs()
getColor <- function(D) {
  sapply(D$ViolLevel, function(x) {
    if(x == "*") {
      "green"
    } else if(x == "**") {
      "orange"
    } else {
      "red"
    } })
}
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(Allston)
)
pal <- colorFactor(c("#ff0000","#ffa500","#008000"), levels=c("*","**","***"))
map <- leaflet(Allston) %>% addTiles() %>%
  addAwesomeMarkers(~Longitude, ~Latitude, icon=icons, label=~as.character(ViolLevel)) %>% 
  addLegend("topleft", pal=pal, values = ~Allston$ViolLevel, title = "Violation Level") %>%
  setView(-71.13, 42.35, zoom = 15)
mapshot(map,file="mymap.png")
knitr::include_graphics("mymap.png")
```

##Using Leaflet 2
```{r out.width = "75%"}
library(leaflet)
map2 <- Allston[, c("Latitude", "Longitude")]%>%
  leaflet()%>%
  addTiles()%>%
  addMarkers()%>%
  setView(-71.13, 42.35, zoom = 15)
mapshot(map2,file="mymap2.png")
knitr::include_graphics("mymap2.png")
```


##shiny(will not include in this report)
```{r include=FALSE}
library(leaflet)
library(sp)
library(magrittr)
library(maps)
library(htmltools)
library(rgdal)
library(data.table)
library(tidyverse)
library(shiny)
levels<-c("*","**","***")
ui <- pageWithSidebar(
  titlePanel("Allston restuarants"),
  sidebarPanel(
    selectInput("level","Select the Violence level",choices=levels)
  ),
  mainPanel(
    leafletOutput("mapping")
  )
)
server <- function(input, output,session) {
  output$mapping <- renderLeaflet({
    level<-input$level
    map_3st <- Allston %>%
      filter(ViolLevel=="***")
    map_2st <- Allston %>%
      filter(ViolLevel=="**")
    map_1st <- Allston %>%
      filter(ViolLevel=="*")
    if(level=="***"){
      food_Boston <-  map_3st 
      leaflet(data = food_Boston) %>% 
        addTiles() %>%
        addTiles()%>%
        addMarkers()%>%
        setView(-71.13062, 42.35308, zoom = 14)
    }
    else if(level=="**"){
      food_Boston <- map_2st
      leaflet(data = food_Boston) %>% 
        addTiles() %>%
        addTiles()%>%
        addMarkers()%>%
        setView(-71.13062, 42.35308, zoom = 14)
    }
    else if(level=="*"){
      food_Boston <- map_1st
      leaflet(data = food_Boston) %>% 
        addTiles() %>%
        addTiles()%>%
        addMarkers()%>%
        setView(-71.13062, 42.35308, zoom = 14)
    }
    
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
```


