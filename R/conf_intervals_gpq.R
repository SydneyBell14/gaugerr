#' Generalized Pivotal Quantities (GPQ) Confidence Intervals
#'
#' @param data a data frame that contains measurements, operators and parts
#' for a Gauge R&R analysis
#' @param part a column of the data frame that has the part labels for the
#' measurements
#' @param operator a column of the data frame that has the operator labels
#' for the measurements
#' @param measurement a column of the data frame that has measurements of the
#' object collected
#' @param alpha the value for the confidence interval calculation (i.e. 95% CI
#' would have alpha=0.05)
#' @param N the number of simulations run for the CI calculation.
#'
#' @return a data frame with the value of the estimate and the upper and lower
#' bounds of the CI.
#' @export
#'
#' @import dplyr  tidyr
#'
#' @examples
#' mydata <- data.frame(P=c(2,2,4,4,5,5),O=c(4,4,4,4,4,4),Y=c(5.3, 6.5, 5.4, 6.4, 6.9, 5.8))
#' conf_intervals_gpq(mydata, alpha = 0.1)
#' conf_intervals_gpq(mydata, N=1000)
#' conf_intervals_gpq(mydata, part=P, operator=O, measurement=Y, alpha=0.01, N=1e4)
conf_intervals_gpq <- function(data, part=P, operator=O, measurement=Y, alpha = 0.05, N=1e5) {
  #calculations for n, p, o, r based on the data frame
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
  SSPO <- SST - sum(SSP, SSO, SSE)

  #calculations for MSP, MSO, MSE and MSPO
  s_p <- SSP/(p - 1)
  s_o <- SSO/(o - 1)
  s_e <- SSE/(p * o * (r - 1))
  s_po <- SSPO/((p-1)*(o-1))

  #calculations for the point estimate values
  s2_p <- max(0,(s_p-s_po)/(o*r))
  s2_o <- max(0,(s_o-s_po)/(p*r))
  s2_po <- max(0,(s_po-s_e)/r)
  s2_tot <- max(0,(p*s_p+o*s_o+(p*o-p-o)*s_po+p*o*(r-1)*s_e)/(p*o*r))
  s2_repro <- max(0,(s_o+(p-1)*s_po-p*s_e)/(p*r))
  s2_gauge <- max(0,(s_o + (p-1)*s_po + p*(r - 1)*s_e) / (p*r))
  pg_ratio <- max(0,s2_p / s2_gauge)
  gt_ratio <- max(0,s2_gauge/s2_tot)
  pr_ratio <- max(0,s2_p/s_e)

  # coefficients based on N the number of simulations
  W1 <- stats::rchisq(N, p - 1)
  W2 <- stats::rchisq(N, o - 1)
  W3 <- stats::rchisq(N, (o - 1) * (p - 1))
  W4 <- stats::rchisq(N, p * o * (r - 1))

  # the estimators for the gpq calculations
  gpq_part  <- pmax(0, ((p - 1) * s_p) / (o * r * W1) -
                      ((p - 1) * (o - 1) * s_po) / (o * r * W3))

  gpq_oper <- pmax(0, ((o - 1) * s_o) / (p * r * W2) -
                     ((p - 1) * (o - 1) * s_po) / (p * r * W3))

  gpq_po <- pmax(0, ((p - 1) * (o - 1) * s_po) / (r * W3) -
                   (p * o * (r - 1) * s_e) / (r * W4))

  gpq_total <- ((p - 1) * s_p) / (o * r * W1) + ((o - 1) * s_o) / (p * r * W2) +
    (((p * o - p - o) * (p - 1) * (o - 1)) * s_po) / (p * o * r * W3) +
    (p * o * (r - 1)^2 * s_e) / (r * W4)

  gpq_repro <- pmax(0,
                    ((o - 1) * s_o) / (p * r * W2) +
                      (1 / r) * (1 - 1/ p) * ((p - 1) * ((o - 1) * s_po) / W3) -
                      (p * o * (r - 1) * s_e) / (r * W4) )

  gpq_gauge <- ((o - 1) * s_o) / (p * r * W2) +
    ((p - 1)^2 * (o - 1) * s_po) / (p * r * W3) +
    (p * o * (r - 1)^2 * s_e) / (r * W4)

  #coefficients for the repeat upper and lower bounds
  G4 <- 1-stats::qf(alpha/2, Inf, p*o*(r-1))
  H4 <- stats::qf(1-alpha/2, Inf, p*o*(r-1))-1
  repeat.lower <- (1-G4)*s_e
  repeat.upper <- (1+H4)*s_e

  gpq_repeat <- (p * o * (r - 1) * s_e) / W4

  gpq_part_gauge <- gpq_part / gpq_gauge
  gpq_gauge_total <- gpq_gauge / gpq_total
  gpq_part_repeat <- gpq_part / gpq_repeat

  # assigning the confidence levels based on the alpha value
  probs <- c(alpha/2, 1 - alpha/2)

  # defining the columns for the data frame of values.
  quantity <- c("s2_repeat","s2_p","s2_o", "s2_po","s2_tot",
                "s2_repro", "s2_gauge", "pg_ratio","gt_ratio", "pr_ratio")
  estimate <- c(s_e, s2_p, s2_o, s2_po, s2_tot,  s2_repro, s2_gauge,
                pg_ratio, gt_ratio, pr_ratio)
  estimators <- data.frame(estimate) %>% pivot_longer(cols = everything(),
                names_to = "name", values_to = "estimate") %>% select(2)
  est.quant <- data.frame(quantity, estimators)
  limits <- bind_rows(stats::quantile(gpq_repeat, probs, na.rm = TRUE),
                      stats::quantile(gpq_part, probs, na.rm = TRUE),
                      stats::quantile(gpq_oper, probs, na.rm = TRUE),
                      stats::quantile(gpq_po, probs, na.rm = TRUE),
                      stats::quantile(gpq_total, probs, na.rm = TRUE),
                      stats::quantile(gpq_repro, probs, na.rm = TRUE),
                      stats::quantile(gpq_gauge, probs, na.rm = TRUE),
                      stats::quantile(gpq_part_gauge, probs, na.rm = TRUE),
                      stats::quantile(gpq_gauge_total, probs, na.rm = TRUE),
                      stats::quantile(gpq_part_repeat, probs, na.rm = TRUE)
  )
  colnames(limits) <- c("lower", "upper")

  # building the data frame for the estimate, upper and lower limits of the CI

  # Returning estimates and lims

  bind_cols(est.quant, limits)
}
