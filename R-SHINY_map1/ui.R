library(shiny)
library(leaflet)

fluidPage(
    leafletOutput("map", height = 1000),   
    absolutePanel(top = 30, right =15,
                  selectInput("location", "Select a location:",
                              choices = list("All location",
                                             location = c("서울특별시", "부산광역시", "인천광역시",
                                                          "대전광역시", "울산광역시", "경기도",
                                                          "강원도", "충청북도",
                                                          "충청남도", "전라북도",
                                                          "전라남도", "경상북도",
                                                          "경상남도", "제주도" )))))
