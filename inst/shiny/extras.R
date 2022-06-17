#extra functions

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
