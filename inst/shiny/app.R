# Functions for ANOVA based estimation
# Shiny App that displays these functions

library(shiny)
library(stringr)
library(tidyverse)
library(readr)

## means function







ui <- navbarPage(title = "Shiny Gague R&R",
                 id = "gague",
                 tabPanel(
                   title = "Data Selection",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "data",
                           label = "chose a data set",
                           choices = c("kf_rr.csv", "long_houf_berman.csv")
                         ) # end radioButtons
                       ), # end sidebarPanel
                       mainPanel(
                        textOutput(
                          outputId = "dataSelect"
                        ) # end textOutput
                       )# end mainPanel
                     ) # end sidebarLayout
                   ) # end fluidPage
                 ), # end Data Selection tab
                 tabPanel(title = "Means",
                   fluidPage(
                     radioButtons(
                       inputId = "click",
                       label = "chose which means you want to see",
                       choiceNames = c("Operator", "Part", "Operator and Part", "Total"),
                       choiceValues = c("O", "P", "O,P", "T"),
                       selected = "T"
                     ), # end radioButtons
                     tableOutput(
                       outputId = "table"
                       ) # end tableOutput
                   ) # end fluidPage
                 ), #end Means tab
                 tabPanel(title = "Square Means Difference",
                          fluidPage(
                            radioButtons(
                              inputId = "difference",
                              label = "chose which squared difference you want to see",
                              choiceNames = c("Operator", "Part", "Operator and Part"),
                              choiceValues = c("O", "P", "O,P"),
                              selected = "Part"
                            ), # end radioButtons
                            tableOutput(
                              outputId = "tableSq"
                              ) #end tableOutput
                          ) #end fluidPage
                        ), #end tabPanel
                 tabPanel(title = "Sum of Squared Differences",
                          fluidPage(
                             radioButtons(
                               inputId = "sumSquares",
                               label = "chose a value you want estimated",
                               choices = c("SSP", "SSO", "SSE", "SST", "SSPO")
                             ),# end radioButtons
                             tableOutput(
                               outputId = "tableSumSq"
                             ), # end tableOutput
                             textOutput(
                               outputId = "definition"
                             ) # end textOutput
                           ) # end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title="Mean Sum of Squares",
                   fluidPage(
                     radioButtons(
                       inputId = "meanSums",
                       label = "chose a measurement",
                       choices = c("MSP", "MSO", "MSE", "MSPO")
                     ), #end radioButtons
                     tableOutput(
                       outputId = "tableMeanSum"
                     ) #end tableOutput
                   ) #end fluidPage
                 ) #end Mean Sum of Squares Panel
) #end navbarPage

server <- function(input, output){
  data_frame <- reactive({
    if(input$data == "kf_rr.csv"){
      data_frame <- read_delim("kf_rr.csv", delim="\t")
      data_frame <- data.frame(data_frame)
    } # end if statement
    else if (input$data == "long_houf_berman.csv"){
      data_frame <- read_csv("long_houf_berman.csv") %>%
        rename(P=Part, O = Operator, Y=y)
    }# end else if statement
  }) # end data_frame

  output$dataSelect <- renderText({
    paste("data selected", input$data)
  }) # end output$dataSelect

  output$table <- renderTable({
    if(input$click == "T"){
      data_frame() %>%
        summarize(ybar = mean(Y))
    }else if(input$click == "O,P"){
      data_frame() %>%
        group_by(O,P) %>%
        summarize(ybar = mean(Y))
    }else{
      data_frame() %>%
        group_by(across(input$click)) %>%
        summarize(ybar = mean(Y))
    }
  }) # end output$table

  output$tableSq <- renderTable({
    if(input$difference == "O,P"){
      data_frame() %>%
        group_by(O, P) %>%
        mutate(ybar2 = mean(Y)) %>%
        summarize(SSe = sum((ybar2-Y)^2))
    } # end if statement (O)
    else {
      data_frame() %>%
        group_by(across(input$difference)) %>%
        mutate(ybar2 = mean(Y)) %>%
        summarise(SSp = sum((ybar2-Y)^2))
    }
  }) # end output$tableSq

  output$tableSumSq <- renderTable({
    n <- nrow(data_frame())
    p <- nrow(data_frame() %>%
                group_by(P) %>%
                summarize(ybar = mean(Y)))
    o <- nrow(data_frame() %>%
                group_by(O) %>%
                summarize(ybar = mean(Y)))
    r <- n/(o*p)

    if(input$sumSquares == "SSP"){
      data_frame() %>%
        group_by(P)%>%
        mutate(ybarI = mean(Y)) %>%
        ungroup() %>%
        mutate(ybar = mean(Y)) %>%
        summarize(ssP1 = (ybarI-ybar)^2) %>%
        distinct() %>%
        summarize(SSP = sum(ssP1)* r * o)

    }else if(input$sumSquares == "SSO"){
      data_frame() %>%
        group_by(O) %>%
        mutate(ybarJ = mean(Y)) %>%
        ungroup() %>%
        mutate(ybar = mean(Y)) %>%
        summarize(ssP1 = (ybarJ-ybar)^2) %>%
        distinct() %>%
        summarize(SSP = sum(ssP1)* r * p)

    }else if(input$sumSquares == "SSE"){
      data_frame()%>%
        group_by(O, P) %>%
        mutate(ybar2 = mean(Y)) %>%
        summarize(SSe = sum((ybar2-Y)^2)) %>%
        ungroup()%>%
        summarize(SSE=sum(SSe))

    }else if(input$sumSquares == "SST"){
      data_frame()%>%
        summarize(var = var(Y))%>%
        summarize(SST = var*(n-1))

    }else if(input$sumSquares == "SSPO"){
      SSP <- data_frame() %>%
        group_by(P)%>%
        mutate(ybarI = mean(Y)) %>%
        ungroup() %>%
        mutate(ybar = mean(Y)) %>%
        summarize(ssP1 = (ybarI-ybar)^2) %>%
        distinct() %>%
        summarize(SSP = sum(ssP1)* r * o)

      SSO <- data_frame() %>%
        group_by(O) %>%
        mutate(ybarJ = mean(Y)) %>%
        ungroup() %>%
        mutate(ybar = mean(Y)) %>%
        summarize(ssP1 = (ybarJ-ybar)^2) %>%
        distinct() %>%
        summarize(SSP = sum(ssP1)* r * p)

      SSE <- data_frame()%>%
        group_by(O, P) %>%
        mutate(ybar2 = mean(Y)) %>%
        summarize(SSe = sum((ybar2-Y)^2)) %>%
        ungroup()%>%
        summarize(SSE=sum(SSe))

      SST <- data_frame()%>%
        summarize(var = var(Y))%>%
        summarize(SST = var*(n-1))

      SSPO <- SST-sum(SSP, SSO, SSE)
      paste(SSPO)
    }
  }) #end output$tableSumSq

  output$definition <- renderText({
    if(input$sumSquares == "SSP"){
      paste("this tells us about the sum of square differences for all the parts")
    }else if(input$sumSquares == "SSO"){
      paste("this tells us about the sum of square differences for all the operators")
    }else if(input$sumSquares == "SSE"){
      paste("this tells us about the repeatability of the measurement")
    }else if(input$sumSquares == "SSPO"){
      paste("this tells us about the sum of square differences for the partop")
    }else if(input$sumSquares == "SST"){
      paste("this tells us about the total variation of the measurement")
    }
  }) #end output$definition

  #c("MSP", "MSO", "MSE", "MSPO")
  output$tableMeanSum <- renderTable({
    n <- nrow(data_frame())
    p <- nrow(data_frame() %>%
                group_by(P) %>%
                summarize(ybar = mean(Y)))
    o <- nrow(data_frame() %>%
                group_by(O) %>%
                summarize(ybar = mean(Y)))
    r <- n/(o*p)

    SSP <- data_frame() %>%
      group_by(P)%>%
      mutate(ybarI = mean(Y)) %>%
      ungroup() %>%
      mutate(ybar = mean(Y)) %>%
      summarize(ssP1 = (ybarI-ybar)^2) %>%
      distinct() %>%
      summarize(SSP = sum(ssP1)* r * o)

    SSO <- data_frame() %>%
      group_by(O) %>%
      mutate(ybarJ = mean(Y)) %>%
      ungroup() %>%
      mutate(ybar = mean(Y)) %>%
      summarize(ssP1 = (ybarJ-ybar)^2) %>%
      distinct() %>%
      summarize(SSP = sum(ssP1)* r * p)

    SSE <- data_frame()%>%
      group_by(O, P) %>%
      mutate(ybar2 = mean(Y)) %>%
      summarize(SSe = sum((ybar2-Y)^2)) %>%
      ungroup()%>%
      summarize(SSE=sum(SSe))

    SST <- data_frame()%>%
      summarize(var = var(Y))%>%
      summarize(SST = var*(n-1))

    SSPO <- SST-sum(SSP, SSO, SSE)

    if (input$meanSums == "MSP"){
      MSP <- SSP/(p-1)
      paste(MSP)
    }else if(input$meanSums == "MSO"){
      MSO <- SSO/(o-1)
      paste(MSO)
    }else if(input$meanSums == "MSE"){
      MSE <- SSE/(p*o*(r-1))
      paste(MSE)
    }else if(input$meanSums == "MSPO"){
      MSPO <- SSPO/((p-1)*(o-1))
      paste(MSPO)
    }
  })

}

shinyApp(ui, server)


