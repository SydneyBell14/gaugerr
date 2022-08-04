#' @title Percent of Variance Contribution
#'
#' @param data a data frame that contains measurements, operators and parts
#' for a Gauge R&R analysis
#' @param part a column of the data frame that has the part labels for the
#' measurements (this is can be specified by just the column name i.e. P)
#' @param operator a column of the data frame that has the operator labels
#' for the measurements (this is can be specified by just the column name i.e. O)
#' @param measurement a column of the data frame that has measurements of the
#' object collected (this is can be specified by just the column name i.e. Y)
#'
#' @return returns a data frame with the values of the percent of variance contribution
#' @export
#'
#' @import dplyr
#'
#' @examples
#' mydata <- data.frame(p=c(1,1,2,2), o=c(1,2,1,2), m=c(2.3,4.5,3.4,4.3))
#' gauge_variance_per(mydata, part=p, operator=o, measurement=m)
#'
gauge_variance_per <- function(data, part=P, operator=O, measurement=Y) {
  # calculations of n,p,r and o
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
    group_by({{part}})|>
    mutate(ybarI = mean({{measurement}})) |>
    ungroup() |>
    mutate(ybar = mean({{measurement}})) |>
    summarize(ssP1 = (.data$ybarI-.data$ybar)^2) |>
    distinct() |>
    summarize(SSP = sum(.data$ssP1)* r * o)|>
    pull()
  #SSO: sum of squares for operator
  SSO <- data |>
    group_by({{operator}}) |>
    mutate(ybarJ = mean({{measurement}})) |>
    ungroup() |>
    mutate(ybar = mean({{measurement}})) |>
    summarize(ssP1 = (.data$ybarJ-.data$ybar)^2) |>
    distinct() |>
    summarize(SSO = sum(.data$ssP1)* r * p)|>
    pull()
  #SSE: sum of squares for equipment (part/operator interaction)
  SSE <- data|>
    group_by({{operator}}, {{part}}) |>
    mutate(ybar2 = mean({{measurement}})) |>
    summarize(SSe = sum((.data$ybar2-{{measurement}})^2)) |>
    ungroup()|>
    summarize(SSE=sum(.data$SSe))|>
    pull() # end SSE
  #SST: the total variance for sum of squares
  SST<- data|>
    summarize(varT = stats::var({{measurement}}))|>
    summarize(MSPO = .data$varT*(n-1)) |>
    pull() # end SST
  #SSPO: total - sum of the sum of squares for part, operator and equipment (part/operator interaction)
  SSPO <- SST-sum(SSP, SSO, SSE)

  #calculations for MSP, MSO, MSE and MSPO
  s_p <- SSP/(p - 1)
  s_o <- SSO/(o - 1)
  s_e <- SSE/(p * o * (r - 1))
  s_po <- SSPO/((p-1)*(o-1))
  total<- (pmax(0,s_e) +
             pmax(0,((s_p- s_po)/(o *r))) +
             pmax(0,(s_o-s_po/(p*r))) +
             pmax(0, s_po - s_e))
    per_rep <- s_e
    (pmax(0,per_rep)/total)*100

    per_po <- (s_po - s_e)
    (pmax(0,per_po)/total)*100
    per_p <- ((s_p- s_po)/(o *r))
    (pmax(0,per_p)/total)*100
    per_o <- (s_o-s_po/(p*r))
    (pmax(0,per_o)/total)*100
    per_rr <- (s_e + (s_o-s_po/(p*r)))
    (pmax(0,per_rr)/total)*100

    return(cbind(per_rep=(pmax(0,per_rep)/total)*100, per_po=(pmax(0,per_po)/total)*100,
                 per_p=(pmax(0,per_p)/total)*100, per_o=(pmax(0,per_o)/total)*100,
                 per_rr=(pmax(0,per_rr)/total)*100))
}
