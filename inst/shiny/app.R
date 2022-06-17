# Functions for ANOVA based estimation
# Shiny App that displays these functions

library(shiny)
library(stringr)
library(tidyverse)
library(readr)

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
                 ), #end Mean Sum of Squares Panel
                 tabPanel(
                   title = "Point Estimates",
                   fluidPage(
                     radioButtons(
                       inputId = "est",
                       label = "choose an estimate",
                       choices = c("s2_repeat","s2_p","s2_o", "s2_po","s2_tot",
                                   "s2_repro", "s2_gauge", "pg_ratio","gt_ratio",
                                   "pr_ratio")
                     ),# end radioButtons
                     tableOutput(
                       outputId = "tableEst"
                     ) # end tableOutput
                   ) # end fluidPage
                 ), # end tabPanel
                 tabPanel(
                   title = "Confidence Intervals"
                 )
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

  output$tableEst <- renderTable({
    n <- nrow(data_frame())
    p <- nrow(data_frame() %>%
                group_by(P) %>%
                summarize(ybar = mean(Y)))
    o <- nrow(data_frame() %>%
                group_by(O) %>%
                summarize(ybar = mean(Y)))
    r <- n/(o*p)

    MSE <- data_frame()%>%
      group_by(O, P) %>%
      mutate(ybar2 = mean(Y)) %>%
      summarize(SSe = sum((ybar2-Y)^2)) %>%
      ungroup()%>%
      summarize(MSE=sum(SSe)/(p*o*(r-1)))

    MSP <- data_frame() %>%
      group_by(P)%>%
      mutate(ybarI = mean(Y)) %>%
      ungroup() %>%
      mutate(ybar = mean(Y)) %>%
      summarize(ssP1 = (ybarI-ybar)^2) %>%
      distinct() %>%
      summarize(MSP = (sum(ssP1)* r * o)/(p-1))

    MSO <- data_frame() %>%
      group_by(O) %>%
      mutate(ybarJ = mean(Y)) %>%
      ungroup() %>%
      mutate(ybar = mean(Y)) %>%
      summarize(ssP1 = (ybarJ-ybar)^2) %>%
      distinct() %>%
      summarize(MSO = (sum(ssP1)* r * p)/(o-1))

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
    MSPO <- SSPO/((p-1)*(o-1))

    s2_p <-(MSP-MSPO)/(o*r)
    s2_o <- (MSO-MSPO)/(p*r)
    s2_po <- (MSPO-MSE)/r
    s2_tot <- (p*MSP+o*MSO+(p*o-p-o)*MSPO+p*o*(r - 1)*MSE)/(p*o*r)
    s2_repro <- (MSO+(p-1)*MSPO-p*MSE)/(p*r)
    s2_gauge <- (MSO + (p-1)*MSPO + p*(r - 1)*MSE) / (p*r)
    # c("s2_repeat","s2_p","s2_o", "s2_po","s2_tot",
    # "s2_repro", "s2_gauge", "pg_ratio","gt_ratio",
    # "pr_ratio")

    if (input$est == "s2_repeat"){
        paste(MSE)
    }else if (input$est == "s2_p"){
        if (s2_p > 0){
          paste(s2_p)
        }else{
          paste(0)
        }
    }else if (input$est == "s2_o"){
        if(s2_o > 0){
          paste(s2_o)
        }else{
          paste(0)
        }
    }else if (input$est == "s2_po"){
      if(s2_po > 0){
        paste(s2_po)
      }else{
        paste(0)
      }
    }else if (input$est == "s2_tot"){
      paste(s2_tot)
    }else if (input$est == "s2_repro"){
      if(s2_repro > 0){
        paste(s2_repro)
      }else{
        paste(0)
      }
    }else if(input$est == "s2_gauge"){
      paste(s2_gauge)
    }else if(input$est == "pg_ratio"){
      pg_ratio <- s2_p / s2_gauge
      paste(pg_ratio)
    }else if(input$est == "gt_ratio"){
      gt_ratio <- s2_gauge/s2_tot
      paste(gt_ratio)
    }else if(input$est == "pr_ratio"){
      pr_ratio <- s2_p/MSE
      paste(pr_ratio)
    }
  })
}

shinyApp(ui, server)


