#' Variance Calculations for Gauge R&R data
#'
#' @param data a data frame that contains measurements, operators and parts
#' for a Gauge R&R analysis
#' @param part a column of the data frame that has the part labels for the
#' measurements
#' @param operator a column of the data frame that has the operator labels
#' for the measurements
#' @param measurement a column of the data frame that has measurements of the
#' object collected
#'
#' @return a data frame with the variance values and the names of the quantities
#' that are associated with such values.
#' @export
#' @import dplyr
#'
#' @examples
#' mydata <- data.frame(P=c(2,2,4,4,5,5),O=c(4,4,4,4,4,4),Y=c(5.3, 6.5, 5.4, 6.4, 6.9, 5.8))
#' gauge_variance(mydata)
#'
#'
gauge_variance <- function(data, part=P, operator=O, measurement=Y) {
  # calcuations for n, p, o and r
  n <- nrow(data)
  p <- nrow(data %>%
              group_by({{part}}) %>%
              summarize(ybar = mean({{measurement}})))
  o <- nrow(data %>%
              group_by({{operator}}) %>%
              summarize(ybar = mean({{measurement}})))
  r <- n/(o*p)

  #SSP: sum of squares for parts
  SSP <- data %>%
    group_by({{part}})%>%
    mutate(ybarI = mean({{measurement}})) %>%
    ungroup() %>%
    mutate(ybar = mean({{measurement}})) %>%
    summarize(ssP1 = (.data$ybarI-data$ybar)^2) %>%
    distinct() %>%
    summarize(SSP = sum(.data$ssP1)* r * o)
  #SSO: sum of squares for operator
  SSO <- data %>%
    group_by({{operator}}) %>%
    mutate(ybarJ = mean({{measurement}})) %>%
    ungroup() %>%
    mutate(ybar = mean({{measurement}})) %>%
    summarize(ssP1 = (.data$ybarJ-.data$ybar)^2) %>%
    distinct() %>%
    summarize(SSO = sum(.data$ssP1)* r * p)
  #SSE: sum of squares for equipment (part/operator interaction)
  SSE <- data%>%
    group_by({{operator}}, {{part}}) %>%
    mutate(ybar2 = mean({{measurement}})) %>%
    summarize(SSe = sum((.data$ybar2-{{measurement}})^2)) %>%
    ungroup()%>%
    summarize(SSE=sum(.data$SSe)) # end SSE
  #SST: the total variance for sum of squares
  SST<- data%>%
    summarize(varT = var({{measurement}}))%>%
    summarize(MSPO = .data$varT*(n-1)) # end SST
  #SSPO: total - sum of the sum of squares for part, operator and equipment (part/operator interaction)
  SSPO <- SST-sum(SSP, SSO, SSE)

  #calculations for MSP, MSO, MSE and MSPO
  MSP <- SSP/(p - 1)
  MSO <- SSO/(o - 1)
  MSE <- SSE/(p * o * (r - 1))
  MSPO <- SSPO/((p-1)*(o-1))

  #variance calculations
  var_rep <- pmax(0, MSE)
  var_po <- pmax(0, MSPO - MSE)
  var_p <- pmax(0, (MSP- MSPO)/(o *r))
  var_o <- pmax(0, MSO-MSPO/(p*r))
  var_rr <- pmax(0, MSE + (MSO-MSPO/(p*r)))
  var_tot <- pmax(0,MSE) + pmax(0,((MSP- MSPO)/(o *r))) +
      pmax(0,(MSO-MSPO/(p*r))) + pmax(0, MSPO - MSE)

  quantity <- c("repeatability", "operators*parts",
                "parts" ,"operators", "R&R", "total")
  variance <- c(var_rep, var_po, var_p, var_o, var_rr, var_tot)
  var <- data.frame(quantity, variance)

  return(var)
}
