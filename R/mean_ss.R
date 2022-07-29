#' @title Mean Sum of Squares
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
#' @return The 4 mean sum of square values
#' MSE: which considers both part and operator
#' MSO: which just considers the operator
#' MSP: which just considers the part
#' MSPO: which considers the values for part, operator and part/operator interaction
#' @export
#'
#' @examples
#' mydata <- data.frame(P=c(2,2,4,4,5,5),O=c(4,4,4,4,4,4),Y=c(5.3, 6.5, 5.4, 6.4, 6.9, 5.8))
#' mean_ss(mydata, part=P, operator=O, measurement = Y)
#'
#' @import dplyr
mean_ss <- function(data, part=P, operator=O, measurement=Y){
  #calculations for n,p,o, and r constants for the data set
  n <- nrow(data)
  p <- nrow(data |>
              group_by({{part}}) |>
              summarize(ybar = mean({{measurement}})))
  o <- nrow(data |>
              group_by({{operator}}) |>
              summarize(ybar = mean({{measurement}})))
  r <- n/(o*p)

  #SSP: sum of squares for parts
  SSP <- data |>
    group_by({{part}}) |>
    mutate(ybarI = mean({{measurement}})) |>
    ungroup() |>
    mutate(ybar = mean({{measurement}})) |>
    summarize(ssP1 = ((.data$ybarI-.data$ybar)^2)) |>
    distinct() |>
    summarize(SSP = (sum(.data$ssP1)) * r * o) |>
    pull()
  #SSO: sum of squares for operator
  SSO <- data |>
    group_by({{operator}}) |>
    mutate(ybarJ = mean({{measurement}})) |>
    ungroup() |>
    mutate(ybar = mean({{measurement}})) |>
    summarize(ssP1 = (.data$ybarJ-.data$ybar)^2) |>
    distinct() |>
    summarize(SSO = (sum(.data$ssP1))* r * p) |>
    pull()
  #SSE: sum of squares for equipment (part/operator interaction)
  SSE <- data|>
    group_by({{operator}}, {{part}}) |>
    mutate(ybar2 = mean({{measurement}})) |>
    summarize(SSe = sum((.data$ybar2-{{measurement}})^2)) |>
    ungroup() |>
    summarize(SSE=sum(.data$SSe)) |>
    pull()# end SSE
  #SST: the total variance for sum of squares
  SST <- data|>
    summarize(varT = stats::var({{measurement}}))|>
    summarize(MSPO = .data$varT*(n-1)) |>
    pull()# end SST
  #SSPO: total - sum of the sum of squares for part, operator and equipment (part/operator interaction)
  SSPO <- SST-sum(SSP, SSO, SSE)

  #calculations for MSP, MSO, MSE and MSPO
  s_p <- SSP/(p - 1)
  s_o <- SSO/(o - 1)
  s_e <- SSE/(p * o * (r - 1))
  s_po <- SSPO/((p-1)*(o-1))


  #return statement for the function, outputs the values
  return(cbind(s_p=s_p, s_o=s_o, s_e=s_e, s_po=s_po))

}
