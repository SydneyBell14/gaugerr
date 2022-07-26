# Functions for ANOVA based estimation
# Shiny App that displays these functions

library(shiny)
library(stringr)
library(tidyverse)
library(readr)

ui <- navbarPage(title = "gaugerr",
                 id = "gague",
                 tabPanel(
                   title = "Overview"
                 )
)

server <- function(input, output){

}

shinyApp(ui, server)
