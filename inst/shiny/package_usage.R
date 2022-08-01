# Functions for ANOVA based estimation
# Shiny App that displays these functions

library(shiny)
library(stringr)
library(tidyverse)
library(readr)
library(gaugerr)

ui <- navbarPage(title = "gaugerr",
                 id = "gague",
                 tabPanel(
                   title = "Overview",
                   fluidPage(
                     mainPanel(
                       h2("Gauge R&R Package"),
                       textOutput(
                         outputId = "otext"
                       ), #end textOutput
                       h3("Definitions"),
                       h5("MLS: stands for Modified Large Sample and is a type
                          of confidence interval computation"),
                       h5("GPQ: stands for Generalized Pivotal Quanities and is a
                          type of confidence interval computation"),
                       h5("Balanced: the number of measurements by each part is the same"),
                       h5("Unbalanced: the number of measurements by each part is not
                          the same")
                     ) #end mainPanel
                   ) #end fluidPage
                 ), #end tabPanel Overview
                 tabPanel(
                   title = "balanced_one_factor()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput11",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ), #end dataInput11
                         sliderInput(
                           inputId = "alpha11",
                           label = "choose a confidence level",
                           max = 1,
                           min = 0,
                           value = 0.05,
                           step = 0.05
                         ) #end alpha11
                       ),#end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "bal_one_factor_info"
                         ), #end bal_one_factor_info
                         verbatimTextOutput(
                           outputId = "bal_one_factor_fun"
                         ), #end bal_one_factor_fun
                         tableOutput(
                           outputId = "bal_one_factor_table"
                         ), #end bal_one_factor_table
                         textOutput(
                           outputId = "bal_one_factor_notes"
                         ) #end bal_one_factor_notes
                       )#end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "balanced_no_interaction()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "data",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ), # end radioButtons
                         sliderInput(
                           inputId = "alpha",
                           label = "choose a confidence level",
                           min = 0,
                           max = 1,
                           value = 0.05,
                           step = 0.05
                         ), # end sliderInput
                         radioButtons(
                           inputId = "conf_type",
                           label = "choose a confidence interval type",
                           choices = c("mls", "gpq")
                         ) # end radioButtons
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "balanced_no_interaction"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "vbal_no_interaction"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "bal_no_interaction"
                         ), #end tableOutput
                         textOutput(
                           outputId = "bal_no_interaction_notes"
                         )
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end  fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "balanced_with_interaction()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ), # end radioButtons
                         sliderInput(
                           inputId = "alpha2",
                           label = "choose a confidence level",
                           min = 0,
                           max = 1,
                           value = 0.05,
                           step = 0.05
                         ), # end sliderInput
                         radioButtons(
                           inputId = "conf_type2",
                           label = "choose a confidence interval type",
                           choices = c("mls", "gpq")
                         ) # end radioButtons
                       ), #end sidebarPanel
                       mainPanel(
                        textOutput(
                          outputId = "balanced_with_interaction"
                        ), #end textOutput
                        verbatimTextOutput(
                          outputId = "bal_w_interact"
                        ), #end verbatimTextOutput
                        tableOutput(
                          outputId = "bal_with_interaction"
                        ), #end tableOutput
                        textOutput(
                          output = "notes_bwint"
                        ) #end textOutput
                       )#end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "unbalanced_one_factor()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput2",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ), # end radioButtons
                         sliderInput(
                           inputId = "alpha3",
                           label = "choose a confidence level",
                           min = 0,
                           max = 1,
                           value = 0.05,
                           step = 0.05
                         ) #end sliderInput
                       ), #end sidebarPanel
                       mainPanel(
                          textOutput(
                            outputId = "unbalanced_one_factor"
                          ), #end textOutput
                          verbatimTextOutput(
                            outputId = "unbal_1_factor"
                          ), #end verbatimTextOutput
                          tableOutput(
                            outputId = "unbal_1factor_table"
                          ) #end tableOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "unbalanced_with_interaction()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput3",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ), # end radioButtons
                         sliderInput(
                           inputId = "alpha4",
                           label = "choose a confidence level",
                           min = 0,
                           max = 1,
                           value = 0.05,
                           step = 0.05
                         ) #end sliderInput
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "unbalanced_with_interaction"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "unbal_with_interaction"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "unbal_with_interaction_table"
                         ), #end tableOutput
                         textOutput(
                           outputId = "unbal_with_interaction_notes"
                         ) #end textOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "unbalanced_no_interaction()",
                   sidebarLayout(
                     sidebarPanel(
                       radioButtons(
                         inputId = "dataInput12",
                         label = "choose a data set",
                         choices = c("data1", "data2")
                       ), #end radioButtons
                       sliderInput(
                         inputId = "alpha8",
                         label = "choose a confidence level",
                         min = 0,
                         max = 1,
                         value = 0.05,
                         step = 0.05
                       ) #end sliderInput
                     ), #end sidebarPanel
                     mainPanel(
                       textOutput(
                         outputId = "unbal_no_interact_info"
                       ), #end textOutput
                       verbatimTextOutput(
                         outputId = "unbal_no_interact_fun"
                       ), #end verbatimTextOutput
                       tableOutput(
                         outputId = "unbal_no_interact_table"
                       ), #end tableOutput
                       textOutput(
                         outputId = "unbal_no_interact_notes"
                       ) #end textOutput
                     ) #end mainPanel
                   ) #end sidebarLayout
                 ), #end tabPanel
                 tabPanel(
                   title = "gauge_rr()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput4",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ), # end radioButtons
                         radioButtons(
                           inputId = "interact",
                           label = "Is there interaction in the data set?",
                           choices = c(TRUE, FALSE)
                         ), #end radioButtons
                         radioButtons(
                           inputId = "factor1",
                           label = "Is factor 1 present?",
                           choices = c("yes", "no")
                         ),#end radioButtons
                         radioButtons(
                           inputId = "factor2",
                           label = "Is factor 2 present?",
                           choices = c("yes", "no")
                         ) #end radioButtons
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "gauge_rr"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "gauge_rr_fun"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "gauge_rr_table"
                         ), #end tableOutput
                         textOutput(
                           outputId = "gauge_rr_notes"
                         ) #end textOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "plot()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "plot_info"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "plot_fun"
                         ), #end verbatimTextOutput
                         plotOutput(
                           outputId = "plot_plot"
                         ), #end plotOutput
                         textOutput(
                           outputId = "plot_notes"
                         ) # end textOutput
                       ) #end mainPanel
                     ) #end sidebarPanel
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "conf_intervals()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput5",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ), #end radioButtons
                         sliderInput(
                           inputId = "alpha5",
                           label = "choose a confidence level",
                           min = 0,
                           max = 1,
                           value = 0.05,
                           step = 0.05
                         ) #end sliderInput
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "conf_info"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "conf_fun"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "conf_table"
                         ), #end tableOutput
                         textOutput(
                           outputId = "conf_notes"
                         ) #end textOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "conf_intervals_gpq()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput6",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ), #end radioButtons
                         sliderInput(
                           inputId = "alpha6",
                           label = "choose a confidence level",
                           min = 0,
                           max = 1,
                           value = 0.05,
                           step = 0.05
                         ), #end sliderInput
                         sliderInput(
                           inputId = "N",
                           label = "choose a number of resamples",
                           min = 1000,
                           max = 1000000,
                           value = 100000,
                           step = 1000
                         ) #end sliderInput
                       ), #end sidebarLayout
                       mainPanel(
                         textOutput(
                           outputId = "gpq_info"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "gpq_fun"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "gpq_plot"
                         ), #end tableOutput
                         textOutput(
                           outputId = "gpq_notes"
                         ) #end textOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "gauge_variance_per()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput7",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ) #end radioButtons
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "var_per_info"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "var_per_fun"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "var_per_table"
                         ) #end tableOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "gauge_variance()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput8",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ) #end radioButtons
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "var_info"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "var_fun"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "var_table"
                         ) #end tableOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "mean_ss()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput9",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ) #end radioButtons
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "mean_info"
                         ), #end mainPanel
                         verbatimTextOutput(
                           outputId = "mean_fun"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "mean_table"
                         ) #end tableOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ), #end tabPanel
                 tabPanel(
                   title = "point_estimate()",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId = "dataInput10",
                           label = "choose a data set",
                           choices = c("data1", "data2")
                         ) #end radioButtons
                       ), #end sidebarPanel
                       mainPanel(
                         textOutput(
                           outputId = "est_info"
                         ), #end textOutput
                         verbatimTextOutput(
                           outputId = "est_fun"
                         ), #end verbatimTextOutput
                         tableOutput(
                           outputId = "est_table"
                         ) #end tableOutput
                       ) #end mainPanel
                     ) #end sidebarLayout
                   ) #end fluidPage
                 ) #end tabPanel
)

server <- function(input, output){

## Overview Output
output$otext <- renderText({"This package analyzes Gauge R&R data that is provided by the user
  and outputs the desired values. There are several models that can be used to implement the
  analysis depending on if the data set is balanced and if there is interaction or multiple
  factors. The package also provides specific functions for calculations
  including sums of squares, variances and point estimates. The other functions provide
  confidence intervals and point estimates for the data inputted. Below are some important
  defintions to familiarize yourself before diving into the package. As you navigate through
  the pages you can interact with the input of the functions and observe the different outputs"})


## Balanced No Interaction Outputs
output$bal_no_interaction <- renderTable({
  if(input$data == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::balanced_no_interaction(data, alpha=input$alpha, conf_type=input$conf_type)
})
output$balanced_no_interaction <- renderText({
  "Sometimes it is unnecessary to include interaction
  effects. If an investigator omits the interaction from the two-factor model then it is called
  the two-factor crossed random model with no interaction. In this function you can change
  the data, alpha and type of confidence intervals. There are three other parameters for the
  function which are part, operator and measurement."
  })
output$vbal_no_interaction <- renderText({
 "balanced_no_interaction(data, part=P, operator=O, measurement=Y,
                          alpha=0.05, conf_type='mls')"
})
output$bal_no_interaction_notes <- renderText({
  "*Notes*: if the data has columns named P, O,
  and Y, then part, measurement and operator do not need to be specified. The function is shown
  with its default parameters below."
})


## Balanced With Interaction Outputs
output$balanced_with_interaction <- renderText({
  "The two-factor crossed design with interaction is the classical gauge R&R model.
  The two factors are typically referred to as 'parts' and 'operators'. This function
  specifically considers balanced experiments with random factors, which means parts and
  operators that are selected at random from larger populations. The function is
  displayed below with the defaults:"
})
output$bal_w_interact <- renderText({
  "balanced_with_interaction(data, part=P, operator=O, measurement=Y,
                             alpha=0.05, conf_type='mls'"
})
output$bal_with_interaction <- renderTable({
  if(input$dataInput == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::balanced_with_interaction(data, alpha=input$alpha2, conf_type=input$conf_type2)
})
output$notes_bwint <- renderText({"*Notes*: This was the first model that I worked on
  for the package, so this function is the combination of two other functions,
  conf_intervals() and cons_intervals_gpq(). There are also functions for point
  estimates, variance, and sums of squares of means that are based in this balanced
  two factor setting. Take a look at these other pages for more information on these
  functions and how to use them."})


## Unbalanced One Factor Outputs
output$unbalanced_one_factor <- renderText({
  "When a data set is unbalanced, this means that we cannot assume normality and independence.
  This means that mean squares in an unbalanced model are not functions of chi-squared random
  variables. This is important with the computation of confidence intervals specifically.
  In this case we use unweighted sums of squares instead. In the context of one factor models,
  this means that the measurements were all taken by a single operator. The function is displayed
  below with the associated default values."
})
output$unbal_1_factor <- renderText({
  "unbalanced_one_factor(data, part=P, operator=O,
                        measurement=Y, alpha=0.05)"
})
output$unbal_1factor_table <- renderTable({
  if(input$dataInput2 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::unbalanced_one_factor(data, alpha=input$alpha3)
})

### Unbalanced With Interaction Outputs
output$unbalanced_with_interaction <- renderText({
  "When a data set is unbalanced, this means that we cannot assume normality and independence.
  This means that mean squares in an unbalanced model are not functions of chi-squared
  random variables. This is important with the computation of confidence intervals
  specifically. In this case we use unweighted sums of squares instead. This function looks
  at a two factor model meaning that part and operator are both factors. This model
  considers that there is interaction present in the given data set and we use the
  USS modification to the ANOVA calculations. This modification helps us to approximate the
  distributional properties of the balanced model sums of squares. The function is shown
  below with the associated defaults for its parameters."
})
output$unbal_with_interaction <- renderText({
  "unbalanced_two_factor(data, part=P, operator=O,
                        measurement=Y, alpha=0.05)"
})
output$unbal_with_interaction_table <- renderTable({
  if(input$dataInput3 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::unbalanced_with_interaction(data, alpha=input$alpha4)
})
output$unbal_with_interaction_notes <- renderText({
  "*Notes*: it is important to note that the USS modification can only be used if there are no
  missing cells in the data set. While there are other calculation for missing cells, this
  is a simplification that discards all measurements for any part that is not measured by all
  operators. In general, with a large enough number of parts this will not have an
  impact on the analysis. "
})


## gauge_rr Outputs
output$gauge_rr <- renderText({
  "This function is most likely the most user friendly one in the package. The function
  works such that the user only needs to provide a data set (*specify part, operator
  and measurement if they differ from the default), indicate whether there is interaction
  and indicate the factos of the data. The function determines the type of analysis to run
  based on the given information. This is a very useful function and works by putting together
  other functions. The output of this function will always be a data frame like the one provided
  below that shows the confidenct intervals in the 'mls' context. The function is also
  shown below with the associated defaults."
})
output$gauge_rr_fun <- renderText({
  "gauge_rr(data, part=P, operator=O, measurement=Y,
            interaction=FALSE, factor1=NULL, factor2=NULL)"
})
output$gauge_rr_table <- renderTable({
  if(input$dataInput4 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  if(input$factor1 == "no"){
    fact1 <- NULL
  }else{
    fact1 <- "yes"
  }

  if(input$factor2 == "no"){
    fact2 <- NULL
  }else{
    fact2 <- "yes"
  }
  gaugerr::gauge_rr(data, interaction = input$interact, factor1=fact1, factor2=fact2)
})
output$gauge_rr_notes <- renderText({
  "*Note* As I keep working on this, I hope to include a formula input. This will be written
  in the lme4 format for processing purposes."
})


## plot() Outputs
output$plot_info <- renderText({
  "This function gives the information from the data frame analysis of Gauge R&R into
  a visual plot. This plotting function allows the user to view values graphically.
  Each confidence interval is plotted by a black line segment and the point estimate
  is represented by a black dot. Below is the plot function and an example of the plot
  output. "
})
output$plot_fun <- renderText({
  "plot(intervals_table object)"
})
output$plot_notes <- renderText({
  "*Note* This function uses object oriented programming (OOP) to rewrite the
  plot fucntion to work on a specific type of object used in the context of this
  analysis."
})
output$plot_plot <- renderPlot({
  data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                      Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                      O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
  plot(gaugerr::gauge_rr(data1, factor1=data1$P, factor2 = data1$O))
})


## conf_intervals() Outputs
output$conf_info <- renderText({
  "This function gives the same output as balanced_with_interaction(), given that
  conf_type='mls'. This means that it gives information about the point estimates
   and confidence intervals in the balanced setting and when there is interaction
  in the data. Below is the function with the associated defaults as well as a table of
  values. "
})
output$conf_fun <- renderText({
  "conf_intervals(data, part=P, operator=O,
                  measurement=Y, alpha=0.05)"
})
output$conf_notes <- renderText({
  "*Note* This is intended to be used when you specifically want the mls type of confidence intervals
  for a balanced data set with interaction"
})
output$conf_table <- renderTable({
  if(input$dataInput5 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::conf_intervals(data, alpha=input$alpha5)
})


## conf_intervals_gpq() Outputs
output$gpq_info <- renderText({
  "This function gives the same output as balanced_with_interaction(), given that
  conf_type='gpq'. This means that it gives information about the point estimates
  and confidence intervals in the balanced setting and when there is interaction
  in the data. Below is the function with the associated defaults as well as a table of
  values."
})
output$gpq_fun <- renderText({
  "conf_intervals_gpq(data, part=P, operator=O,
                      measurement=Y, alpha=0.05,
                      N=1e+05"
})
output$gpq_plot <- renderTable({
  if(input$dataInput6 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::conf_intervals_gpq(data, alpha=input$alpha6, N=input$N)
})

## Percent of Variance Contribution Outputs
output$var_per_info <- renderText({
  "This function uses the variance of each point estimate and then measures how much
  each estimate contributes to the total variance. The amount of contribution is
  represented as a percentage. Below shows the function code and its associated default
  values."
})
output$var_per_fun <- renderText({
  "gauge_variance_per(data, part=P, operator=O,measurement=Y)"
})
output$var_per_table <- renderTable({
  if(input$dataInput7 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::gauge_variance_per(data)
})


## Variance of Point Estimates Outputs
output$var_info <- renderText({
  "This function calculates the variance for important values in Gauge R&R analysis.
  The computation of variance is sometimes of interest for analyzing the data. Below is
  the function show with its associate default parameters."
})
output$var_fun <- renderText({
  "gauge_variance(data, part=P, operator=O, measurement=Y)"
})
output$var_table <- renderTable({
  if(input$dataInput8 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::gauge_variance(data)
})


## Mean Sums of Squares Outputs
output$mean_info <- renderText({
  "This function finds the mean sum of squares for the point estimates. This is an intermediary
   step in the process and not very meaningful for the analysis. Below is the function with the
  its associated defaults."
})
output$mean_fun <- renderText({
  "mean_ss(data, part=P, operator=O, measurement=Y)"
})
output$mean_table <- renderTable({
  if(input$dataInput9 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::mean_ss(data)
})


## Point Estimates Outputs
output$est_info <- renderText({
  "This function returns the point estimates for the Gauge R&R analysis. The function is
  only meant to be used with data that is balanced and has interaction. The function is
  displayed below with the associated default parameters."
})
output$est_fun <- renderText({
  "point_estimate(data, part=P, operator=O, measurement=Y)"
})
output$est_table <- renderTable({
  if(input$dataInput10 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::point_estimate(data)
})

## balanced_one_factor() Outputs
output$bal_one_factor_info <- renderText({
  "This model describes a gauge R&R study in which a single operator selects a random sample of p parts
  and measures each part r times using the same measurement gauge. In this analysis the model
  only uses MLS confidence intervals. The function is printed below with the associated default
  parameters. The output of the function is a data frame with the confidence intervals and point
  estimate values."
})
output$bal_one_factor_fun <- renderText({
  "balanced_one_factor(data, part=P, operator=O, measurement=Y,
                        alpha=0.05)"
})
output$bal_one_factor_table <- renderTable({
  if(input$dataInput11 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::balanced_one_factor(data, alpha = input$alpha11)
})
output$bal_one_factor_notes <- renderText({
  "*Notes*: a balanced data set is one in which the number of measurements is consistent for each
   part. In this analysis a balanced data set is required to run the appropriate analysis."
})


# Unbalanced No Interaction Outputs
output$unbal_no_interact_info <- renderText({
  "This model analyzes the unbalanced Gauge R&R data with interaction. The approach to analyze
  this data is to use the balanced model equation with the USS modification. The function
  is printed below with the associated default values. "
})
output$unbal_no_interact_fun <- renderText({
  "unbalanced_no_interaction(data, part=P, operator=O,
                            measurement=Y, alpha=0.05)"
})
output$unbal_no_interact_table <- renderTable({
  if(input$dataInput12 == "data1"){
    data1 <- data.frame(P=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10),
                        Y=c(37,38,37,41,41,40,41,42,41,42,41,43,42,42,42,43,42,43, 30,31,31,31,31,31,29,30,28,42,43,42,43,43,43,42,42,42,28,30,29,29,30,29,31,29,29,42,42,43,45,45,45,44,46,45,25,26,27,28,28,30,29,27,27,40,40,40,43,42,42,43,43,41,25,25,25,27,29,28,26,26,26,35,34,34,35,35,34,35,34,35),
                        O=c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
    data <- data1
  }else{
    data2 <- data.frame(P=c(5,5,7,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,5,6,7,6,6,6,6,6,7,7,6,7,6,7,6,6,6,2,7,6,6,6,7,7,6,7,7,6,6,6,7,3,6,7,7,6,2,7,2,6,6,7,7,6,7,6,7,5,6,6,7,2,2,6,5,6,7,6,7,7,6,6,4,6,7,7,7,6,7,2,7,7,2,5,7,6,6,7,7,6,6,6,7,7,7,7,2,6,5,5,7,6,7,6,7,6,6,6,7,7,6,7,7,7,6,6,7,7,7,7,5,7,6,2,6,5,2,6,7,2,7,7,5,2,2,5,2,7,5,2,2,5,2,5,6,7,6,2,6,7,7,2,2,7,
                            2,5,2,2,6,7,6,2,5,2,7,6,6,5,7,6,6,2,2,2,2,7,7,7,2,5,2,5,5,2,5,2,7,4,4,4,4,4,4,2,2,7,2,5,2,2,5,2,7,2,7,2,5,2,2,2,6,5,5,2,2,5,5,2,2,2,5,7,5,2,6,6,5,5,6,5,6,5,5,5,5,5,3,3,5,5,5,3,3,5,5,1,5,6,5,5,5,5,5,5,6,3,5,5,5,5,5,5,6,6,5,5,5,5,6,5,3,3,3,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                            5,5,2,5,5,5,5,5,5,5,5,5,5,5,5,5,2,2,5,5,5,5,5,5,5,5),
                        O=c(2,2,8,1,2,4,6,2,6,2,2,6,8,2,8,2,1,2,2,2,8,2,1,2,10,2,4,4,5,2,2,5,2,2,8,6,2,2,2,8,2,2,6,2,2,2,2,2,2,7,8,2,5,8,6,2,6,5,6,2,2,2,2,5,2,2,2,4,2,6,6,8,2,2,2,2,2,6,2,2,9,2,2,5,6,2,4,6,8,2,6,2,2,2,8,6,5,6,5,2,4,5,8,2,6,6,2,2,2,2,2,2,2,5,5,2,2,4,2,2,2,4,2,4,4,2,2,2,2,2,2,1,5,2,6,4,8,6,3,2,2,6,6,2,1,2,2,6,6,2,6,2,2,2,2,6,8,2,5,6,6,
                            11,6,2,6,6,2,8,4,6,2,1,4,5,2,2,4,5,2,6,6,6,6,5,11,8,6,2,1,2,2,6,2,6,2,9,9,9,9,9,7,6,6,2,6,2,6,6,2,1,2,6,4,6,2,1,6,6,2,5,5,6,6,2,2,6,6,6,2,6,9,6,2,2,2,9,8,2,6,5,2,5,9,2,7,7,9,2,2,7,7,9,2,2,2,10,2,2,2,2,2,5,6,7,2,2,2,2,2,2,2,8,2,2,2,2,2,2,7,7,7,2,9,2,2,5,2,2,2,6,2,2,2,2,2,2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,9,2,2,2,2,2,2,2,2,2,2,
                            2,2,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,2,2,2,9,2,9,9,2),
                        Y=c(-0.0769,-0.0755,-0.4500,-0.2769,-0.2605,-0.2574,-0.2444,-0.2275,-0.2284,-0.0446,-0.2167,-0.2196,-0.2105,-0.2014,-0.1941,-0.1690,-0.1683,-0.1639,-0.0335,-0.1633,-0.2219,-0.1593,-0.1608,-0.1565,-0.1595,-0.1559,-0.2105,-0.2108,-0.1485,-0.1967,-0.1432,-0.1904,-0.1389,-0.1387,-0.1375,-0.1551,-0.1786,-0.1311,-0.1290,-0.1286,-0.1690,-0.1690,-0.1264,-0.1666,-0.1638,-0.1181,-0.1186,-0.1150,-0.1546,
                            -0.0774,-0.1131,-0.1514,-0.1493,-0.1077,-0.0278,-0.1468,-0.1198,-0.1045,-0.1061,-0.1426,-0.1427,-0.1034,-0.1370,-0.1009,-0.1366,-0.0200,-0.0981,-0.0976,-0.1278,-0.0360,-0.0300,-0.0930,-0.0185,-0.0904,-0.1175,-0.0849,-0.1106,-0.1123,-0.0795,-0.0794,-0.0560,-0.0773,-0.1008,-0.1003,-0.1023,-0.0737,-0.0998,-0.0790,-0.0940,-0.0930,-0.0841,-0.0137,-0.0875,-0.0638,-0.0641,-0.0853,-0.0831,-0.0597,
                            -0.0581,-0.0582,-0.0790,-0.0783,-0.0775,-0.0768,-0.0867,-0.0550,-0.0108,-0.0106,-0.0709,-0.0521,-0.0698,-0.0497,-0.0656,-0.0482,-0.0466,-0.0461,-0.0605,-0.0582,-0.0415,-0.0559,-0.0551,-0.0552,-0.0400,-0.0400,-0.0543,-0.0537,-0.0527,-0.0517,-0.0076,-0.0500,-0.0342,-0.0618,-0.0339,-0.0065,-0.0364,-0.0319,-0.0429,-0.0412,-0.0363,-0.0356,-0.0048,-0.0302,-0.0330,-0.0045,-0.0255,-0.0288,-0.0039,
                            -0.0237,-0.0188,-0.0037,-0.0190,-0.0031,-0.0147,-0.0192,-0.0136,-0.0165,-0.0107,-0.0142,-0.0133,-0.0110,-0.0102,-0.0115,-0.0123,-0.0016,-0.0109,-0.0103,-0.0067,-0.0038, -0.0021,0.0012,0.0003,0.0020,0.0025,0.0021,0.0023,0.0005,0.0058,0.0042,0.0054,0.0094,0.0080,0.0145,0.0129,0.0124,0.0146,0.0156,0.0177,0.0025,0.0219,0.0032,0.0032,0.0150,0.0035,0.0256,0.0244,0.0139,0.0139,0.0139,0.0139,0.0139,
                            0.0139,0.0286,0.0277,0.0284,0.0327,0.0044,0.0333,0.0383,0.0056,0.0404,0.0419,0.0587,0.0451,0.0657,0.0080,0.0581,0.0599,0.0682,0.0444,0.0092,0.0102,0.0777,0.0783,0.0120,0.0124,0.1152,0.1217,0.0860,0.0139,0.0956,0.0149,0.1116,0.0803,0.0803,0.0165,0.0197,0.1025,0.0206,0.1047,0.0217,0.0219,0.0227,0.0227,0.0235,0.0779,0.0797,0.0240,0.0253,0.0254,0.0863,0.0895,0.0264,0.0274,0.0277,0.0272,0.1367,0.0279,
                            0.0282,0.0283,0.0288,0.0290,0.0298,0.1481,0.0987,0.0302,0.0310,0.0312,0.0314,0.0322,0.0337,0.1669,0.1778,0.0359,0.0358,0.0369,0.0366,0.1820,0.0373,0.1281,0.1282,0.1313,0.0395,0.0417,0.0434,0.0432,0.0441,0.0441,0.0448, 0.0452,0.0868,0.0455,0.0461,0.0473,0.0476,0.0481,0.0495, 0.0502,0.0503,0.0504,0.5010,0.0520,0.0523,0.0529,0.0522,0.0560,0.0556,0.0569,0.0564,0.0565,0.0570,0.0603,0.0595,0.0622,0.0621,
                            0.0626,0.0631,0.0638,0.0645,0.0647,0.0670,0.0684,0.0679,0.0674,0.0696,0.0694,0.0712,0.7018,0.0746,0.0755,0.0793,0.0801,0.0809,0.0823,0.0876,0.0886,0.0915,0.0934,0.0932,0.0954,0.0985,0.9582,0.1385,0.1100,0.1144,0.1213,0.1242,0.1363,0.1519,0.1556,0.1757))
    data <- data2
  }
  gaugerr::unbalanced_no_interaction(data, alpha = input$alpha8)
})

}

shinyApp(ui, server)
