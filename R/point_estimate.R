#' Title
#'
#' @param data a data frame consisting of gauge R&R data to run the analysis
#' @param part a column of the data frame that has the part labels for the
#' measurements
#' @param operator a column of the data frame that has the operator labels
#' for the measurements
#' @param measurement a column of the data frame that has measurements of the
#' object collected
#'
#' @return a list of the point estimates
#' s2_p: the point estimate for parts
#' s2_o: the point estimate for operators
#' s2_po the point estimate for the parts and operators
#' s2_tot: the total point estimate
#' s2_repro: the point estimate for reproducibility
#' s2_gauge:
#' pg_ratio: the part/gauge ratio (s2_p/s2_gauge)
#' gt_ratio: the gauge/total ratio (s2_gauge/s2_tot)
#' pr_ratio: the part/reproducibility ratio (s2_p/s2_repro)
#' @export
#'
#' @import dplyr
#'
#' @examples
#' mydata <- data.frame(P=c(1,2), O=c(1,1), Y=c(5, 4))
#' point_estimate(mydata)
#' point_estimate(mydata, part=P, operator=O, measurement=Y)
point_estimate <- function(data, part=P, operator=O, measurement=Y) {

  # calculations of n,p,r and o
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
    summarize(ssP1 = (.data$ybarI-.data$ybar)^2) %>%
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
    summarize(varT = stats::var({{measurement}}))%>%
    summarize(MSPO = .data$varT*(n-1)) # end SST
  #SSPO: total - sum of the sum of squares for part, operator and equipment (part/operator interaction)
  SSPO <- SST-sum(SSP, SSO, SSE)

  #calculations for MSP, MSO, MSE and MSPO
  MSP <- SSP/(p - 1)
  MSO <- SSO/(o - 1)
  MSE <- SSE/(p * o * (r - 1))
  MSPO <- SSPO/((p-1)*(o-1))

  # point estimate calculations
  s2_p <-pmax(0,(MSP-MSPO)/(o*r))
  s2_o <- pmax(0,(MSO-MSPO)/(p*r))
  s2_po <- pmax(0,(MSPO-MSE)/r)
  s2_tot <- (p*MSP+o*MSO+(p*o-p-o)*MSPO+p*o*(r-1)*MSE)/(p*o*r)
  s2_repro <- pmax(0,(MSO+(p-1)*MSPO-p*MSE)/(p*r))
  s2_gauge <- pmax(0,(MSO + (p-1)*MSPO + p*(r - 1)*MSE) / (p*r))
  pg_ratio <- s2_p / s2_gauge
  gt_ratio <- s2_gauge/s2_tot
  pr_ratio <- s2_p/MSE

  #return statement will output the point estimate values
  return(c(s2_p, s2_o, s2_po, s2_tot, s2_repro, s2_gauge, pg_ratio, gt_ratio, pr_ratio))
}
