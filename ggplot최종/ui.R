library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(extrafont)
library(showtext)
library(rsconnect)
library(tidyverse)
library(shinythemes)
library(devtools)

ui <- fluidPage(theme = shinytheme("lumen"),
  mainPanel(
    tabsetPanel(
     tabPanel("소비자가 선호하는 술은?",
              br(),
              br(),
              plotOutput('plot')),
     tabPanel("술에 어울리는 안주는?",
              br(),
              fluidRow(
                selectInput(
                  inputId="list",
                  label="List:",
                  choices=c("장수막걸리", "지평막걸리", "느린마을막걸리", "서울의밤","화요","복분자")
                 )
                ),
              br(),
              plotOutput('plot1')
              ),
     tabPanel("안주와 어울리는 술은?",
              br(),
              fluidRow(
                       selectInput(
                         inputId="snacklist",
                         label="List:",
                         choices=c("볶음류", "구이류", "해산물류", "탕/찌개류", "과일류", "마른안주류", "튀김류", "전류")
                       )
                ),
              br(),
              plotOutput('plot2')
            )
      )
    )
)

