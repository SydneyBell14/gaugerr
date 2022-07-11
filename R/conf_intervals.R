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
    pull()# end SST
  #SSPO: total - sum of the sum of squares for part, operator and equipment (part/operator interaction)
  SSPO <- SST-sum(SSP, SSO, SSE)

  #calculations for MSP, MSO, MSE and MSPO
  s_p <- SSP/(p - 1)
  s_o <- SSO/(o - 1)
  s_e <- SSE/(p * o * (r - 1))
  s_po <- SSPO/((p-1)*(o-1))

  #calculations for the point estimate values
  s2_p <-pmax(0,(s_p-s_po)/(o*r))
  s2_o <- pmax(0,(s_o-s_po)/(p*r))
  s2_po <- pmax(0,(s_po-s_e)/r)
  s2_tot <- pmax(0,(p*s_p+o*s_o+(p*o-p-o)*s_po+p*o*(r-1)*s_e)/(p*o*r))
  s2_repro <- pmax(0,(s_o+(p-1)*s_po-p*s_e)/(p*r))
  s2_gauge <- pmax(0,(s_o + (p-1)*s_po + p*(r - 1)*s_e) / (p*r))
  pg_ratio <- pmax(0,s2_p / s2_gauge)
  gt_ratio <- pmax(0,s2_gauge/s2_tot)
  pr_ratio <- pmax(0,s2_p/s_e)

  #calculation for the point estimates from book
  mu_y <- data %>%
    summarize(ybar = mean({{measurement}})) %>%
    pull()
  gamma_p <- (s_p - s_po)/(o*r)

  gamma_m <- (s_o + (p-1)*s_po + p*(r-1)*s_e)/(p*r)

  gamma_r <- gamma_p/gamma_m

  # coefficients
  G1 <- 1 - stats::qf(alpha/2, Inf, p - 1)
  G2 <- 1 - stats::qf(alpha/2, Inf, o - 1)
  G3 <- 1 - stats::qf(alpha/2, Inf, (p-1) * (o-1))
  G4 <- 1 - stats::qf(alpha/2, Inf, p * o * (r - 1))
  H1 <- stats::qf(1 - alpha/2, Inf, p - 1) - 1
  H2 <- stats::qf(1 - alpha/2, Inf, o - 1) - 1
  H3 <- stats::qf(1 - alpha/2, Inf, (p-1) * (o-1)) - 1
  H4 <- stats::qf(1 - alpha/2, Inf, p * o * (r - 1)) - 1
  F1 <- stats::qf(1 - alpha/2, p - 1, (p - 1) * (o - 1))
  F2 <- stats::qf(alpha/2, p - 1, (p - 1) * (o - 1))
  F3 <- stats::qf(1 - alpha/2, p - 1, o - 1)
  F4 <- stats::qf(alpha/2, p - 1, o - 1)
  F5 <- stats::qf(1 - alpha/2, o - 1, (p - 1) *(o - 1))
  F6 <- stats::qf(alpha/2, o - 1, (p - 1) *(o - 1))
  F7 <- stats::qf(1 - alpha/2, (p - 1) * (o - 1), p * o * (r - 1))
  F8 <- stats::qf(alpha/2, (p - 1) * (o - 1), p * o * (r - 1))
  F9 <- stats::qf(1 - alpha/2, p - 1, p * o * (r - 1))
  F10 <- stats::qf(alpha/2, p - 1, p * o * (r - 1))
  F11 <- stats::qf(1 - alpha/2, o - 1, p * o * (r - 1))
  F12 <- stats::qf(alpha/2, o - 1, p * o * (r - 1))
  G13 <- ((F1 - 1)^2 - G1^2 * F1^2 - H3^2) / F1
  G23 <- ((F5 - 1)^2 - G2^2 * F5^2 - H3^2) / F5
  G23.star <- (1 - 1/stats::qf(1-alpha/2, p * (o - 1), Inf))^2 * (p^2 / (p - 1)) -
      G2^2 / (p-1) - G3^2 * (p-1)
  G24 <- ((1 - F11)^2 - G2^2 * F11^2 - H4^2) / F11
  G34 <- ((1 - F7)^2 - G3^2 * F7^2 - H4^2) / F7
  H13 <- ((1 - F2)^2 - H1^2 * F2^2 - G3^2) / F2
  H23 <- ((1 - F6)^2 - H2^2 * F6^2 - G3^2) / F6
  H24 <- ((1 - F12)^2 - H2^2 * F12^2 - G4^2) / F12
  H34 <- ((1 - F8)^2 - H3^2 * F8^2 - G4^2) / F8


  # CI upper and lower for mu_y
  K <- s_p + s_o - s_po
  C <- (s_p * sqrt(stats::qf(1-alpha, 1, p-1)) + s_o * sqrt(stats::qf(1-alpha, 1, o-1)) -
          s_po * (sqrt(stats::qf(1-alpha, 1, (p-1)*(o-1))))/K
  #if (K<0) {
    #K <- s_po
    #C <- sqrt(stats::qf(1-alpha, 1, (p-1)*(o-1))) }
  mu_lower <- mu_y - (C*sqrt(K/(p*o*r)))
  mu_upper <- mu_y + (C*sqrt(K/(p*o*r)))

  #CI upper and lower for gamma_p (same thing as parts.lower and parts.upper)
  v_lp <- G1^2 * s_p^2 + H3^2 * s_po^2 + G13 * s_p * s_po
  v_up <- H1^2 *s_p^2 + G3^2 * s_po^2 + H13 * s_p * s_po
  gamma_p_lower <- gamma_p - (sqrt(v_lp))/(o*r)
  gamma_p_upper <- gamma_p + (sqrt(v_up))/(o*r)

  # CI upper and lower for gamma_m
  v_lm <- G2^2 *s_o^2 + G3^2 * (p-1)^2 * s_po^2 + G4^2 * p^2 * (r-1)^2 * s_e^2
  v_um <- H2^2 * s_o^2 + H3^2 * (p-1)^2 * s_po^2 + H4^2 * p^2 * (r-1)^2 * s_e^2
  gamma_m_lower <- gamma_m - (sqrt(v_lm))/(p*r)
  gamma_m_upper <- gamma_m + (sqrt(v_um))/(p*r)

  #CI upper and lower for gamma_r
  gamma_r_lower <- (p*(1-G1)*(s_p-F1*s_po))/(p*o*(r-1)*s_e + o*(1-G1)*F3*s_o + o*(p-1)*s_po)
  gamma_r_upper <- (p*(1+H1)*(s_p-F2*s_po))/(p*o*(r-1)*s_e + o*(1+H1)*F4*s_o + o*(p-1)*s_po)

  #interval for PTR (requires USL and LSL)

  #interval for SNR
  SNR_estimate <- sqrt(2*gamma_r)
  SNR_lower <- sqrt(2*gamma_r_lower)
  SNR_upper <- sqrt(2*gamma_r_upper)

  #interval for c_p (requires LSL and USL)

  # CI upper and lower for s2_repeat
  repeat.lower <- pmax(0,(1 - G4) * s_e)
  repeat.upper <- (1 + H4) * s_e

  # CI upper and lower for s2_p (note: this is gamma_p in book)
  v_lp <- G1^2 * s_p^2 + H3^2 * s_po^2 + G13 * s_p * s_po
  v_up <- H1^2 * s_p^2 + G3^2 * s_po^2 + H13 * s_p * s_po
  parts.lower <- pmax(0, s2_p - sqrt(v_lp) / (o * r))
  parts.upper <- s2_p + sqrt(v_up) / (o * r)

  # CI upper and lower for s2_o
  v_lo <- G2^2 * s_o^2 + H3^2 * s_po^2 + G23 * s_o * s_po
  v_uo <- H2^2 * s_o^2 + G3^2 * s_po^2 + H23 * s_o * s_po
  oper.lower <- pmax(0, s2_o - sqrt(v_lo) / (p * r))
  oper.upper <- s2_o + sqrt(v_uo) / (p * r)

  # CI upper and lower for s2_po
  v_lpo <- G3^2 * s_po^2 + H4^2 * s_e^2 + G34 * s_po * s_e
  v_upo <- H3^2 * s_po^2 + G4^2 * s_e^2 + H34 * s_po * s_e
  po.lower <- pmax(0, s2_po - sqrt(v_lpo) / r)
  po.upper <- s2_po + sqrt(v_upo) / r

  # CI upper and lower for s2_tot
  v_lt <- G1^2 * p^2 * s_p^2 +
      G2^2 * o^2 * s_o^2 +
      G3^2 * (p * o - p - o)^2 * s_po^2 +
      G4^2 * (p * o)^2 * (r - 1)^2 * s_e^2
  v_ut <- H1^2 * p^2 * s_p^2 +
      H2^2 * o^2 * s_o^2 +
      H3^2 * (p * o - p - o)^2 * s_po^2 +
      H4^2 * (p * o)^2 * (r - 1)^2 * s_e^2
  total.lower <- pmax(0, s2_tot - sqrt(v_lt) / (p * o * r))
  total.upper <- s2_tot + sqrt(v_ut) / (p * o * r)

  # CI upper and lower for s2_repro
  v_lrepro <- (G2^2 * s_o^2 +
                   G3^2 * (p - 1)^2 * s_po^2 +
                   H4^2 * p^2 * s_e^2 +
                   G24 * p * s_o * s_e +
                   G34 * (p - 1) * p * s_po * s_e +
                   G23.star * (p - 1) * s_o * s_po) / (p * r)^2
  v_urepo <- (H2^2 * s_o^2 +
                  H3^2 * (p - 1)^2 * s_po^2 +
                  G4^2 * p^2 * s_e^2 +
                  H24 * p * s_o * s_e +
                  H34 * (p - 1) * p * s_po * s_e) / (p * r)^2
  repro.lower <- pmax(0, s2_repro - sqrt(v_lrepro))
  repro.upper <- s2_repro + sqrt(v_urepo)

  # CI upper and lower for s2_gauge (note: this is gamma_m in the book)
  v_lm <- G2^2 * s_o^2 + G3^2 * (p - 1)^2 * s_po^2 + G4^2 * p^2 * (r - 1)^2 * s_e^2
  v_um <- H2^2 * s_o^2 + H3^2 * (p - 1)^2 * s_po^2 + H4^2 * p^2 * (r - 1)^2 * s_e^2
  gauge.lower <- pmax(0, s2_gauge - sqrt(v_lm) / (p * r))
  gauge.upper <- s2_gauge + sqrt(v_um) / (p * r)

  # CI upper and lower for pg_ratio
  pg_ratio_lower <- pmax(0, (p * (1 - G1) * (s_p - F1 * s_po)) /
                             (p * o * (r - 1) * s_e + o * (1 - G1) * F3 * s_o + o * (p - 1) * s_po))
  pg_ratio_upper <- (p * (1 + H1) * (s_p - F2 * s_po)) /
      (p * o * (r - 1) * s_e + o * (1 + H1) * F4 * s_o + o * (p - 1) * s_po)

  # CI upper and lower for tg_ratio
  tg_ratio_lower <- 1 + pg_ratio_lower
  tg_ratio_upper <- 1 + pg_ratio_upper

  # CI upper and lower for pr_ratio
  pr_ratio_lower <- pmax(0, (s_po / (o * r * s_e * F9)) *
                             ((s_p / s_po) - (1 / (1 - G1)) + (s_po * F1 * (1 - F1 * (1 - G1))) / (s_p * (1 - G1))))
  pr_ratio_upper <- (s_po / (o * r * s_e * F10)) *
      ((s_p / s_po) - (1 / (1 + H1)) + (s_po * F2 * (1 - F2 * (1 + H1))) / (s_p * (1 + H1)))

  # the columns for the data frame
  quantity <- c("s2_repeat","s2_p","s2_o", "s2_po","s2_tot",
                  "s2_repro", "s2_gauge", "pg_ratio","tg_ratio", "pr_ratio")
  estimate <- c(s_e, s2_p, s2_o, s2_po, s2_tot,  s2_repro, s2_gauge,
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
