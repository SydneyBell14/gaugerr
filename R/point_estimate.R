#' @title Point Estimates for Gauge R&R Analysis
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
#' s_p: the point estimate for parts
#' s_o: the point estimate for operators
#' s_po the point estimate for the parts and operators
#' s_tot: the total point estimate
#' s_repro: the point estimate for reproducibility
#' s_gauge:
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
    summarize(SSP = sum(.data$ssP1)* r * o) %>%
    pull()
  #SSO: sum of squares for operator
  SSO <- data %>%
    group_by({{operator}}) %>%
    mutate(ybarJ = mean({{measurement}})) %>%
    ungroup() %>%
    mutate(ybar = mean({{measurement}})) %>%
    summarize(ssP1 = (.data$ybarJ-.data$ybar)^2) %>%
    distinct() %>%
    summarize(SSO = sum(.data$ssP1)* r * p) %>%
    pull()
  #SSE: sum of squares for equipment (part/operator interaction)
  SSE <- data%>%
    group_by({{operator}}, {{part}}) %>%
    mutate(ybar2 = mean({{measurement}})) %>%
    summarize(SSe = sum((.data$ybar2-{{measurement}})^2)) %>%
    ungroup()%>%
    summarize(SSE=sum(.data$SSe)) %>%
    pull() # end SSE
  #SST: the total variance for sum of squares
  SST<- data%>%
    summarize(varT = stats::var({{measurement}}))%>%
    summarize(MSPO = .data$varT*(n-1)) %>%
    pull() # end SST
  #SSPO: total - sum of the sum of squares for part, operator and equipment (part/operator interaction)
  SSPO <- SST-sum(SSP, SSO, SSE)

  #calculations for MSP, MSO, MSE and MSPO
  s_p <- SSP/(p - 1)
  s_o <- SSO/(o - 1)
  s_e <- SSE/(p * o * (r - 1))
  s_po <- SSPO/((p-1)*(o-1))

  # point estimate calculations
  s2_p <-pmax(0,(s_p-s_po)/(o*r))
  s2_o <- pmax(0,(s_o-s_po)/(p*r))
  s2_po <- pmax(0,(s_po-s_e)/r)
  s2_tot <- (p*s_p+o*s_o+(p*o-p-o)*s_po+p*o*(r-1)*s_e)/(p*o*r)
  s2_repro <- pmax(0,(s_o+(p-1)*s_po-p*s_e)/(p*r))
  s2_gauge <- pmax(0,(s_o + (p-1)*s_po + p*(r - 1)*s_e) / (p*r))
  pg_ratio <- s2_p / s2_gauge
  gt_ratio <- s2_gauge/s2_tot
  pr_ratio <- s2_p/s_e

  #point estimator calculation using the book (page 30)
  mu_y <- data %>%
    summarize(ybar = mean({{measurement}})) %>%
    pull()
  gamma_p <- (s_p - s_po)/(o*r)

  gamma_m <- (s_o + (p-1)*s_po + p*(r-1)*s_e)/(p*r)

  gamma_r <- gamma_p/gamma_m

  #return statement will output the point estimate values
  #return(cbind(s2_p, s2_o, s2_po, s2_tot, s2_repro, s2_gauge, pg_ratio, gt_ratio, pr_ratio))

  return(cbind(mu_y, gamma_p, gamma_m, gamma_r))
}
