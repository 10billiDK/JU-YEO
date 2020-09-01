library(shiny)
library(leaflet)
library(dplyr)
library(readr)

rd<-read.csv("r.csv", header = T, stringsAsFactors = F, fileEncoding = "euc-kr")
rd<-mutate(rd, popup_text=paste0("<center>",
                                 ifelse(!is.na(image), paste0("<img src = ", rd$image, " width='100'>"), ""),
                                 "</br><b>",name , "</b>",
                                 "</br><b>Alcohol</b>: ", Alc,
                                 "</br><a href='", links, "' target='_blank'>More info...</a></center>"))

write_rds(rd, "r.rds") 
r<- read_rds("r.rds")

shinyServer(function (input, output, session) {
    
    output$map <- renderLeaflet({
        filtered <- 
            if (input$location == "All location") {
                r
            } else {
                filter(r, location == input$location)
            }
        
        leaflet(filtered) %>%
            addProviderTiles(providers$Esri.WorldTopoMap)%>%  
            addMarkers(lng=~longitude, 
                       lat=~latitude,
                       popup = ~popup_text
            )
    })
    
})
