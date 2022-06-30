#' Modified large-sample (MLS) Confidence Interval Calculation
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
#'
#' @return a data frame with values of the point estimate and the upper and lower
#' bounds of the mls confidence interval.
#' @export
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' mydata <- data.frame(P=c(2,2,4,4,5,5),O=c(4,4,4,4,4,4),Y=c(5.3, 6.5, 5.4, 6.4, 6.9, 5.8))
#' conf_intervals(mydata, alpha=0.1)
#' conf_intervals(mydata, part=P, operator=O, measurement=Y, alpha=0.01)
conf_intervals <- function(data, part=P, operator=O, measurement=Y, alpha = 0.05) {
  #calculations for the constants n,p,o and r that depend on the data frame
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
    summarize(varT = var({{measurement}}))%>%
    summarize(MSPO = .data$varT*(n-1)) # end SST
  #SSPO: total - sum of the sum of squares for part, operator and equipment (part/operator interaction)
  SSPO <- SST-sum(SSP, SSO, SSE)

  #calculations for MSP, MSO, MSE and MSPO
  MSP <- SSP/(p - 1)
  MSO <- SSO/(o - 1)
  MSE <- SSE/(p * o * (r - 1))
  MSPO <- SSPO/((p-1)*(o-1))

  #calculations for the point estimate values
  s2_p <-pmax(0,(MSP-MSPO)/(o*r))
  s2_o <- pmax(0,(MSO-MSPO)/(p*r))
  s2_po <- pmax(0,(MSPO-MSE)/r)
  s2_tot <- pmax(0,(p*MSP+o*MSO+(p*o-p-o)*MSPO+p*o*(r-1)*MSE)/(p*o*r))
  s2_repro <- pmax(0,(MSO+(p-1)*MSPO-p*MSE)/(p*r))
  s2_gauge <- pmax(0,(MSO + (p-1)*MSPO + p*(r - 1)*MSE) / (p*r))
  pg_ratio <- pmax(0,s2_p / s2_gauge)
  gt_ratio <- pmax(0,s2_gauge/s2_tot)
  pr_ratio <- pmax(0,s2_p/MSE)

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

  # CI upper and lower for s2_repeat
  repeat.lower <- pmax(0,(1 - G4) * MSE)
  repeat.upper <- (1 + H4) * MSE

  # CI upper and lower for s2_p
  v_lp <- G1^2 * MSP^2 + H3^2 * MSPO^2 + G13 * MSP * MSPO
  v_up <- H1^2 * MSP^2 + G3^2 * MSPO^2 + H13 * MSP * MSPO
  parts.lower <- pmax(0, s2_p - sqrt(v_lp) / (o * r))
  parts.upper <- s2_p + sqrt(v_up) / (o * r)

  # CI upper and lower for s2_o
  v_lo <- G2^2 * MSO^2 + H3^2 * MSPO^2 + G23 * MSO * MSPO
  v_uo <- H2^2 * MSO^2 + G3^2 * MSPO^2 + H23 * MSO * MSPO
  oper.lower <- pmax(0, s2_o - sqrt(v_lo) / (p * r))
  oper.upper <- s2_o + sqrt(v_uo) / (p * r)

  # CI upper and lower for s2_po
  v_lpo <- G3^2 * MSPO^2 + H4^2 * MSE^2 + G34 * MSPO * MSE
  v_upo <- H3^2 * MSPO^2 + G4^2 * MSE^2 + H34 * MSPO * MSE
  po.lower <- pmax(0, s2_po - sqrt(v_lpo) / r)
  po.upper <- s2_po + sqrt(v_upo) / r

  # CI upper and lower for s2_tot
  v_lt <- G1^2 * p^2 * MSP^2 +
      G2^2 * o^2 * MSO^2 +
      G3^2 * (p * o - p - o)^2 * MSPO^2 +
      G4^2 * (p * o)^2 * (r - 1)^2 * MSE^2
  v_ut <- H1^2 * p^2 * MSP^2 +
      H2^2 * o^2 * MSO^2 +
      H3^2 * (p * o - p - o)^2 * MSPO^2 +
      H4^2 * (p * o)^2 * (r - 1)^2 * MSE^2
  total.lower <- pmax(0, s2_tot - sqrt(v_lt) / (p * o * r))
  total.upper <- s2_tot + sqrt(v_ut) / (p * o * r)

  # CI upper and lower for s2_repro
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
  repro.lower <- pmax(0, s2_repro - sqrt(v_lrepro))
  repro.upper <- s2_repro + sqrt(v_urepo)

  # CI upper and lower for s2_gauge
  v_lm <- G2^2 * MSO^2 + G3^2 * (p - 1)^2 * MSPO^2 + G4^2 * p^2 * (r - 1)^2 * MSE^2
  v_um <- H2^2 * MSO^2 + H3^2 * (p - 1)^2 * MSPO^2 + H4^2 * p^2 * (r - 1)^2 * MSE^2
  gauge.lower <- pmax(0, s2_gauge - sqrt(v_lm) / (p * r))
  gauge.upper <- s2_gauge + sqrt(v_um) / (p * r)

  # CI upper and lower for pg_ratio
  pg_ratio_lower <- pmax(0, (p * (1 - G1) * (MSP - F1 * MSPO)) /
                             (p * o * (r - 1) * MSE + o * (1 - G1) * F3 * MSO + o * (p - 1) * MSPO))
  pg_ratio_upper <- (p * (1 + H1) * (MSP - F2 * MSPO)) /
      (p * o * (r - 1) * MSE + o * (1 + H1) * F4 * MSO + o * (p - 1) * MSPO)

  # CI upper and lower for tg_ratio
  tg_ratio_lower <- 1 + pg_ratio_lower
  tg_ratio_upper <- 1 + pg_ratio_upper

  # CI upper and lower for pr_ratio
  pr_ratio_lower <- pmax(0, (MSPO / (o * r * MSE * F9)) *
                             ((MSP / MSPO) - (1 / (1 - G1)) + (MSPO * F1 * (1 - F1 * (1 - G1))) / (MSP * (1 - G1))))
  pr_ratio_upper <- (MSPO / (o * r * MSE * F10)) *
      ((MSP / MSPO) - (1 / (1 + H1)) + (MSPO * F2 * (1 - F2 * (1 + H1))) / (MSP * (1 + H1)))

  # the columns for the data frame
  quantity <- c("s2_repeat","s2_p","s2_o", "s2_po","s2_tot",
                  "s2_repro", "s2_gauge", "pg_ratio","tg_ratio", "pr_ratio")
  estimate <- c(MSE, s2_p, s2_o, s2_po, s2_tot,  s2_repro, s2_gauge,
                  pg_ratio, gt_ratio, pr_ratio)
  lower <- c(repeat.lower, parts.lower, oper.lower, po.lower, total.lower,
               repro.lower, gauge.lower, pg_ratio_lower, 1/tg_ratio_upper, pr_ratio_lower)
  upper <- c(repeat.upper, parts.upper, oper.upper, po.upper, total.upper,
               repro.upper, gauge.upper,pg_ratio_upper, 1/tg_ratio_lower, pr_ratio_upper)

  # cleaning the data for the data frame output using tidyr techniques
  upper.bounds <- data.frame(upper) %>% pivot_longer(cols = everything(),
                                                       names_to = "estimate", values_to = "upper")%>%
      select(2)
  lower.bounds <- data.frame(lower) %>% pivot_longer(cols = everything(),
                                                     names_to = "estimate", values_to = "lower") %>%
      select(2)
  estimate.value <- data.frame(estimate) %>% pivot_longer(cols = everything(),
                                                            names_to = "measure", values_to = "estimate")%>%
    select(2)

  #return statement for the data frame with estimate, lower and upper bounds of the CI
  return(cbind(quantity, estimate.value, lower.bounds, upper.bounds))
}
