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
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "meanSums",
                           label = "chose a measurement",
                           choices = c("MSP", "MSO", "MSE", "MSPO")
                         ) #end radioButtons
                       ),#end siderbarPanel
                       mainPanel(
                         tableOutput(
                           outputId = "tableMeanSum"
                         ) #end tableOutput
                       )#end mainPanel
                     )#end siderbarLayout
                   ) #end fluidPage
                 ), #end Mean Sum of Squares Panel
                 tabPanel(
                   title = "Point Estimates",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "est",
                           label = "choose an estimate",
                           choices = c("s2_repeat","s2_p","s2_o", "s2_po","s2_tot",
                                       "s2_repro", "s2_gauge", "pg_ratio","gt_ratio",
                                       "pr_ratio")
                         )# end radioButtons
                       ), #end sidebarPanel
                       mainPanel(
                         tableOutput(
                           outputId = "tableEst"
                         ) # end tableOutput
                       ) #end mainPanel
                     ) #end sidebar Layout
                   ) # end fluidPage
                 ), # end tabPanel
                 tabPanel(
                   title = "Confidence Intervals",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "conf",
                           label = "choose an estimator you want the confidence interval for",
                           choices = c("s2_repeat","s2_p","s2_o", "s2_po","s2_tot",
                                       "s2_repro", "s2_gauge", "pg_ratio","tg_ratio",
                                       "pr_ratio")
                         ),# end radioButtons
                         radioButtons(
                           inputId = "type",
                           label = "choose the type of estimator you want calculated",
                           choices = c("mls", "gpq")
                         ), #end radioButtons
                         sliderInput(
                           inputId = "alpha",
                           label = "chose a value for alpha",
                           min = 0,
                           max = 1,
                           value = 0.90,
                           step = 0.05
                         )# end sliderInput
                       ),#end sidebarPanel
                       mainPanel(
                         tableOutput(
                           outputId = "tableConf"
                         ) # end tableOutput
                       ) #end mainPanel
                     )#end sidebarLayout
                   ) # end fluidPage
                 ) # end tabPanel
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

  MSE <- reactive({data_frame()%>%
      group_by(O, P) %>%
      mutate(ybar2 = mean(Y)) %>%
      summarize(SSe = sum((ybar2-Y)^2)) %>%
      ungroup()%>%
      summarize(MSE=sum(SSe)/(p*o*(r-1)))}) #end MSE

  MSP <- reactive({data_frame() %>%
      group_by(P) %>%
      mutate(ybarI = mean(Y)) %>%
      ungroup() %>%
      mutate(ybar = mean(Y)) %>%
      summarize(ssP1 = (ybarI-ybar)^2) %>%
      distinct() %>%
      summarize(MSP = (sum(ssP1)* r * o)/(p-1))}) #end MSP

  MSO <- reactive({data_frame() %>%
      group_by(O) %>%
      mutate(ybarJ = mean(Y)) %>%
      ungroup() %>%
      mutate(ybar = mean(Y)) %>%
      summarize(ssP1 = (ybarJ-ybar)^2) %>%
      distinct() %>%
      summarize(MSO = (sum(ssP1)* r * p)/(o-1))}) #end MSO

  SSP <- reactive({data_frame() %>%
      group_by(P)%>%
      mutate(ybarI = mean(Y)) %>%
      ungroup() %>%
      mutate(ybar = mean(Y)) %>%
      summarize(ssP1 = (ybarI-ybar)^2) %>%
      distinct() %>%
      summarize(SSP = sum(ssP1)* r * o)}) #end SSP

  SSO <- reactive({data_frame() %>%
      group_by(O) %>%
      mutate(ybarJ = mean(Y)) %>%
      ungroup() %>%
      mutate(ybar = mean(Y)) %>%
      summarize(ssP1 = (ybarJ-ybar)^2) %>%
      distinct() %>%
      summarize(SSO = sum(ssP1)* r * p)}) #end SSO
  SSE <- reactive({data_frame()%>%
      group_by(O, P) %>%
      mutate(ybar2 = mean(Y)) %>%
      summarize(SSe = sum((ybar2-Y)^2)) %>%
      ungroup()%>%
      summarize(SSE=sum(SSe))})
  SST<-reactive({data_frame()%>%
      summarize(var = var(Y))%>%
      summarize(MSPO = var*(n-1))}) #relabeled for the convention

  output$dataSelect <- renderText({
    paste("data selected", input$data)
  }) # end output$dataSelect

  output$tableMeanSum <- renderTable({
    n <-  nrow(data_frame())
    p <- nrow(data_frame() %>%
                group_by(P) %>%
                summarize(ybar = mean(Y)))
    o <- nrow(data_frame() %>%
                group_by(O) %>%
                summarize(ybar = mean(Y)))
    r <- n/(o*p)

    SSPO <- SST()-sum(SSP(), SSO(), SSE())

    if (input$meanSums == "MSP"){
      return(MSP())
    }else if(input$meanSums == "MSO"){
      return(MSO())
    }else if(input$meanSums == "MSE"){
      return(MSE())
    }else if(input$meanSums == "MSPO"){
      MSPO <- SSPO/((p-1)*(o-1))
      return(MSPO)
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
    MSE <- MSE()
    MSP <- MSP()
    MSO <- MSO()

    SSPO <- SST()-sum(SSP(), SSO(), SSE())
    MSPO <- SSPO/((p-1)*(o-1))

    s2_p <-(MSP-MSPO)/(o*r)
    s2_o <- (MSO-MSPO)/(p*r)
    s2_po <- (MSPO-MSE)/r
    s2_tot <- (p*MSP+o*MSO+(p*o-p-o)*MSPO+p*o*(r-1)*MSE)/(p*o*r)
    s2_repro <- (MSO+(p-1)*MSPO-p*MSE)/(p*r)
    s2_gauge <- (MSO + (p-1)*MSPO + p*(r - 1)*MSE) / (p*r)

    if (input$est == "s2_repeat"){
        return(MSE)
    }else if (input$est == "s2_p"){
        return(max(unlist(0,s2_p)))
    }else if (input$est == "s2_o"){
        paste(max(0,s2_o))
    }else if (input$est == "s2_po"){
      paste(unlist(max(0,s2_po)))
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
  output$tableConf <- renderTable({
    n <- nrow(data_frame())
    p <- nrow(data_frame() %>%
                group_by(P) %>%
                summarize(ybar = mean(Y)))
    o <- nrow(data_frame() %>%
                group_by(O) %>%
                summarize(ybar = mean(Y)))
    r <- n/(o*p)
    MSE <- MSE()
    MSP <- MSP()
    MSO <- MSO()

    SSPO <- SST()-sum(SSP(), SSO(), SSE())
    MSPO <- SSPO/((p-1)*(o-1))

    s2_p <-(MSP-MSPO)/(o*r)
    s2_o <- (MSO-MSPO)/(p*r)
    s2_po <- (MSPO-MSE)/r
    s2_tot <- (p*MSP+o*MSO+(p*o-p-o)*MSPO+p*o*(r-1)*MSE)/(p*o*r)
    s2_repro <- (MSO+(p-1)*MSPO-p*MSE)/(p*r)
    s2_gauge <- (MSO + (p-1)*MSPO + p*(r - 1)*MSE) / (p*r)
    pg_ratio <- s2_p / s2_gauge
    gt_ratio <- s2_gauge/s2_tot
    pr_ratio <- s2_p/MSE

    # alpha value
    alpha <- input$alpha

    if (input$type == "mls"){
    # coefficients
    G1 <- 1 - qf(alpha/2, Inf, p - 1)
    G2 <- 1 - qf(alpha/2, Inf, o - 1)
    G3 <- 1 - qf(alpha/2, Inf, (p-1) * (o-1))
    G4 <- 1 - qf(alpha/2, Inf, p * o * (r - 1))
    H1 <- qf(1 - alpha/2, Inf, p - 1) - 1
    H2 <- qf(1 - alpha/2, Inf, o - 1) - 1
    H3 <- qf(1 - alpha/2, Inf, (p-1) * (o-1)) - 1
    H4 <- qf(1 - alpha/2, Inf, p * o * (r - 1)) - 1
    F1 <- qf(1 - alpha/2, p - 1, (p - 1) * (o - 1))
    F2 <- qf(alpha/2, p - 1, (p - 1) * (o - 1))
    F3 <- qf(1 - alpha/2, p - 1, o - 1)
    F4 <- qf(alpha/2, p - 1, o - 1)
    F5 <- qf(1 - alpha/2, o - 1, (p - 1) *(o - 1))
    F6 <- qf(alpha/2, o - 1, (p - 1) *(o - 1))
    F7 <- qf(1 - alpha/2, (p - 1) * (o - 1), p * o * (r - 1))
    F8 <- qf(alpha/2, (p - 1) * (o - 1), p * o * (r - 1))
    F9 <- qf(1 - alpha/2, p - 1, p * o * (r - 1))
    F10 <- qf(alpha/2, p - 1, p * o * (r - 1))
    F11 <- qf(1 - alpha/2, o - 1, p * o * (r - 1))
    F12 <- qf(alpha/2, o - 1, p * o * (r - 1))
    G13 <- ((F1 - 1)^2 - G1^2 * F1^2 - H3^2) / F1
    G23 <- ((F5 - 1)^2 - G2^2 * F5^2 - H3^2) / F5
    G23.star <- (1 - 1/qf(1-alpha/2, p * (o - 1), Inf))^2 * (p^2 / (p - 1)) -
      G2^2 / (p-1) - G3^2 * (p-1)
    G24 <- ((1 - F11)^2 - G2^2 * F11^2 - H4^2) / F11
    G34 <- ((1 - F7)^2 - G3^2 * F7^2 - H4^2) / F7
    H13 <- ((1 - F2)^2 - H1^2 * F2^2 - G3^2) / F2
    H23 <- ((1 - F6)^2 - H2^2 * F6^2 - G3^2) / F6
    H24 <- ((1 - F12)^2 - H2^2 * F12^2 - G4^2) / F12
    H34 <- ((1 - F8)^2 - H3^2 * F8^2 - G4^2) / F8

    if(input$conf == "s2_repeat"){
      repeat.lower <- (1 - G4) * MSE
      repeat.upper <- (1 + H4) * MSE
      paste("lower:", repeat.lower, " upper:",repeat.upper)
    }else if(input$conf =="s2_p"){
      v_lp <- G1^2 * MSP^2 + H3^2 * MSPO^2 + G13 * MSP * MSPO
      v_up <- H1^2 * MSP^2 + G3^2 * MSPO^2 + H13 * MSP * MSPO
      parts.lower <- s2_p - sqrt(v_lp) / (o * r)
      parts.upper <- s2_p + sqrt(v_up) / (o * r)
      paste("lower:", parts.lower, " upper:",parts.upper)
    }else if(input$conf =="s2_o"){
      v_lo <- G2^2 * MSO^2 + H3^2 * MSPO^2 + G23 * MSO * MSPO
      v_uo <- H2^2 * MSO^2 + G3^2 * MSPO^2 + H23 * MSO * MSPO
      oper.lower <- s2_o - sqrt(v_lo) / (p * r)
      oper.upper <- s2_o + sqrt(v_uo) / (p * r)
      paste("lower:", oper.lower, " upper:", oper.upper)
    }else if(input$conf =="s2_po"){
      v_lpo <- G3^2 * MSPO^2 + H4^2 * MSE^2 + G34 * MSPO * MSE
      v_upo <- H3^2 * MSPO^2 + G4^2 * MSE^2 + H34 * MSPO * MSE
      po.lower <- s2_po - sqrt(v_lpo) / r
      po.upper <- s2_po + sqrt(v_upo) / r
      paste("lower:", po.lower, " upper:", po.upper)
    }else if(input$conf =="s2_tot"){
      v_lt <- G1^2 * p^2 * MSP^2 +
        G2^2 * o^2 * MSO^2 +
        G3^2 * (p * o - p - o)^2 * MSPO^2 +
        G4^2 * (p * o)^2 * (r - 1)^2 * MSE^2
      v_ut <- H1^2 * p^2 * MSP^2 +
        H2^2 * o^2 * MSO^2 +
        H3^2 * (p * o - p - o)^2 * MSPO^2 +
        H4^2 * (p * o)^2 * (r - 1)^2 * MSE^2
      total.lower <- s2_tot - sqrt(v_lt) / (p * o * r)
      total.upper <- s2_tot + sqrt(v_ut) / (p * o * r)
      paste("lower:", total.lower, " upper:", total.upper)
    }else if(input$conf =="s2_repro"){
      v_lrepro <- (G2^2 * MSO^2 +
                     G3^2 * (p - 1)^2 * MSPO^2 +
                     H4^2 * p^2 * MSE^2 +
                     G24 * p * MSO * MSE +
                     G34 * (p - 1) * p * MSPO * MSE +
                     G23.star * (p - 1) * MSO * MSPO) / (p * r)^2
      v_urepo <- (H2^2 * MSO^2 +
                    H3^2 * (p - 1)^2 * MSPO^2 +
                    G4^2 * p^2 * MSE^2 +
                    H24 * p * MSO * MSE +
                    H34 * (p - 1) * p * MSPO * MSE) / (p * r)^2
      repro.lower <- s2_repro - sqrt(v_lrepro)
      repro.upper <- s2_repro + sqrt(v_urepo)
      paste("lower:", repro.lower, " upper:", repro.upper)
    }else if(input$conf =="s2_gauge"){
      v_lm <- G2^2 * MSO^2 + G3^2 * (p - 1)^2 * MSPO^2 + G4^2 * p^2 * (r - 1)^2 * MSE^2
      v_um <- H2^2 * MSO^2 + H3^2 * (p - 1)^2 * MSPO^2 + H4^2 * p^2 * (r - 1)^2 * MSE^2
      gauge.lower <- s2_gauge - sqrt(v_lm) / (p * r)
      gauge.upper <- s2_gauge + sqrt(v_um) / (p * r)
      paste("lower:", gauge.lower, " upper:", gauge.upper)
    }else if(input$conf =="pg_ratio"){
      pg_ratio_lower <- (p * (1 - G1) * (MSP - F1 * MSPO)) /
        (p * o * (r - 1) * MSE + o * (1 - G1) * F3 * MSO + o * (p - 1) * MSPO)
      pg_ratio_upper <- (p * (1 + H1) * (MSP - F2 * MSPO)) /
        (p * o * (r - 1) * MSE + o * (1 + H1) * F4 * MSO + o * (p - 1) * MSPO)
      paste("lower:", pg_ratio_lower, " upper:", pg_ratio_upper)
    }else if(input$conf =="tg_ratio"){
      pg_ratio_lower <- (p * (1 - G1) * (MSP - F1 * MSPO)) /
        (p * o * (r - 1) * MSE + o * (1 - G1) * F3 * MSO + o * (p - 1) * MSPO)
      pg_ratio_upper <- (p * (1 + H1) * (MSP - F2 * MSPO)) /
        (p * o * (r - 1) * MSE + o * (1 + H1) * F4 * MSO + o * (p - 1) * MSPO)
      tg_ratio_lower <- 1 + pg_ratio_lower
      tg_ratio_upper <- 1 + pg_ratio_upper
      paste("lower:", tg_ratio_lower, " upper:", tg_ratio_upper)
    }else if(input$conf =="pr_ratio"){MSPO
      pr_ratio_lower <- (MSPO / (o * r * MSE * F9)) *
        ((MSP / MSPO) - (1 / (1 - G1)) + (MSPO * F1 * (1 - F1 * (1 - G1))) / (MSP * (1 - G1)))
      pr_ratio_upper <- (MSPO / (o * r * MSE * F10)) *
        ((MSP / MSPO) - (1 / (1 + H1)) + (MSPO * F2 * (1 - F2 * (1 + H1))) / (MSP * (1 + H1)))
      paste("lower:", pr_ratio_lower, " upper:", pr_ratio_upper)
    }
    }
    else if(input$type == "gpq"){
      N <- 1e5
      W1 <- rchisq(N, p - 1)
      W2 <- rchisq(N, o - 1)
      W3 <- rchisq(N, (o - 1) * (p - 1))
      W4 <- rchisq(N, p * o * (r - 1))

      gpq_part  <- pmax(0, ((p - 1) * MSP) / (o * r * W1) -
                          ((p - 1) * (o - 1) * MSPO) / (o * r * W3))

      gpq_oper <- pmax(0, ((o - 1) * MSO) / (p * r * W2) -
                         ((p - 1) * (o - 1) * MSPO) / (p * r * W3))

      gpq_po <- pmax(0, ((p - 1) * (o - 1) * MSPO) / (r * W3) -
                       (p * o * (r - 1) * MSE) / (r * W4))

      gpq_total <- ((p - 1) * MSP) / (o * r * W1) + ((o - 1) * MSO) / (p * r * W2) +
        (((p * o - p - o) * (p - 1) * (o - 1)) * MSPO) / (p * o * r * W3) +
        (p * o * (r - 1)^2 * MSE) / (r * W4)

      gpq_repro <- pmax(0,
                        ((o - 1) * MSO) / (p * r * W2) +
                          (1 / r) * (1 - 1/ p) * ((p - 1) * ((o - 1) * MSPO) / W3) -
                          (p * o * (r - 1) * MSE) / (r * W4) )

      gpq_gauge <- ((o - 1) * MSO) / (p * r * W2) +
        ((p - 1)^2 * (o - 1) * MSPO) / (p * r * W3) +
        (p * o * (r - 1)^2 * MSE) / (r * W4)

      gpq_repeat <- (p * o * (r - 1) * MSE) / W4

      probs <- c(alpha/2, 1 - alpha/2)

      if(input$conf == "s2_repeat"){
        bounds <- quantile(gpq_repeat, probs, na.rm = TRUE)
        lower <- MSE - max(0,bounds)
        upper <- MSE + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "s2_p"){
        bounds <- quantile(gpq_part, probs, na.rm = TRUE)
        lower <- s2_p - max(0,bounds)
        upper <- s2_p + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "s2_o"){
        bounds <- quantile(gpq_oper, probs, na.rm = TRUE)
        lower <- s2_o - max(0,bounds)
        upper <- s2_o + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "s2_po"){
        bounds <- quantile(gpq_po, probs, na.rm = TRUE)
        lower <- s2_po - max(0,bounds)
        upper <- s2_po + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "s2_tot"){
        bounds <- quantile(gpq_repeat, probs, na.rm = TRUE)
        paste(bounds)
        lower <- s2_tot - max(0,bounds)
        upper <- s2_tot + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "s2_repro"){
        bounds <- quantile(gpq_repro, probs, na.rm = TRUE)
        lower <- s2_repro - max(0,bounds)
        upper <- s2_repro + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "s2_gauge"){
        bounds <- quantile(gpq_gauge, probs, na.rm = TRUE)
        quantile(gpq_gauge, probs, na.rm = TRUE)
        lower <- s2_gauge - max(0,bounds)
        upper <- s2_gauge + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "pg_ratio"){
        gpq_part_gauge <- gpq_part / gpq_gauge
        bounds <- quantile(gpq_part_gauge, probs, na.rm = TRUE)
        lower <- pg_ratio - max(0,bounds)
        upper <- pg_ratio + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "tg_ratio"){
        gpq_gauge_total <- gpq_gauge / gpq_total
        bounds <- quantile(gpq_gauge_total, probs, na.rm = TRUE)
        lower <- gt_ratio - max(0,bounds)
        upper <- gt_ratio + bounds
        paste("lower:", lower, " upper:", upper)
      }else if(input$conf == "pr_ratio"){
        gpq_part_repeat <- gpq_part / gpq_repeat
        bounds <- quantile(gpq_part_repeat, probs, na.rm = TRUE)
        lower <- pr_ratio - max(0,bounds)
        upper <- pr_ratio + bounds
        paste("lower:", lower, " upper:", upper)
      }
    }
  })
}

shinyApp(ui, server)

