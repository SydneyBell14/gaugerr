balanced_no_interaction <- function(data, part=P, operator=O, measurement=Y, alpha=0.05) {
  # the model we are using Y_{ijk}=mu_Y + P_i + O_j + E_{ijk}

  #calculation for n, p, o, and r
  n <- nrow(data)
  p <- nrow(data %>%
              group_by({{part}}) %>%
              summarize(ybar = mean({{measurement}})))
  o <- nrow(data %>%
              group_by({{operator}}) %>%
              summarize(ybar = mean({{measurement}})))
  r <- n/(o*p)

  # calculation for s_p^2, s_o^2, s_e^2 and ybar_star
  s_p <- data %>%
    group_by({{part}}) %>%
    mutate(ybarI = mean({{measurement}})) %>%
    ungroup() %>%
    mutate(ybar = mean({{measurement}})) %>%
    summarize(s_p = ((sum((.data$ybarI-.data$ybar)^2))/(p-1))) %>%
    pull()

  s_o <- data %>%
    group_by({{operator}}) %>%
    mutate(ybarJ = mean({{measurement}})) %>%
    ungroup() %>%
    mutate(ybar = mean({{measurement}})) %>%
    summarize(s_o = ((sum((.data$ybarJ-.data$ybar)^2))/(o-1))) %>%
    pull()

  s_e <- data %>%
    group_by({{operator}}) %>%
    mutate(ybarJ = mean({{measurement}})) %>%
    ungroup() %>%
    group_by({{part}}) %>%
    mutate(ybarI = mean({{measurement}})) %>%
    ungroup() %>%
    mutate(ybar_star = (mean({{measurement}}))/(p*o*r)) %>%
    mutate(ybar = mean({{measurement}})) %>%
    summarize(s_e = ((sum((.data$ybar-.data$ybarI-.data$ybarJ+.data$ybar_star)))/((p*r*o)-p-o+1)))%>%
    pull()

  ybar_star <- data %>%
    summarize(ybar_star = (mean({{measurement}}))/(p*o*r))

  #constants use in confidence intervals for the model
  G1 <- 1 - stats::qf(alpha/2, Inf, p - 1)
  G2 <- 1 - stats::qf(alpha/2, Inf, o - 1)
  G3 <- 1 - stats::qf(alpha/2, Inf, (p*o*r)-p-o+1)
  H1 <- stats::qf(1 - alpha/2, Inf, p-1) - 1
  H2 <- stats::qf(1 - alpha/2, Inf, o-1) - 1
  H3 <- stats::qf(1 - alpha/2, Inf, (p*o*r)-p-o+1) - 1
  F1 <- stats::qf(1 - alpha/2, p- 1, (p*o*r)-p-o+1)
  F2 <- stats::qf(alpha/2, p-1, (p*o*r)-p-o+1)
  F3 <- stats::qf(1- alpha/2, p-1, o-1)
  F4 <- stats::qf(alpha/2, p-1, o-1)
  G13 <- ((F1 - 1)^2 - G1^2 * F1^2 - H3^2)/F1
  H13 <- ((1-F2)^2 - H1^2 * F2^2 - G3^2)/F2

  #confidence interval for mu_Y
  C <- (s_p * sqrt(stats::qf(1-alpha, 1, p-1)) +
          s_o * sqrt(stats::qf(1-alpha, 1, o-1)) -
          s_e * sqrt(stats::qf(1-alpha, 1, (p*o*r)-p-o+1)))
  K <- s_p + s_o - s_e
  if (K < 0){
    K <- s_e
    C <- sqrt(stats::qf(1-alpha, 1, (p*o*r)-p-o+1))
  }
  mu_Y_lower <- ybar_star - (C * sqrt((K)/(p*o*r)))
  mu_Y_upper <- ybar_star + (C * sqrt((K)/(p*o*r)))

  #confidence interval for gamma_p
  v_lp <- G1^2 * s_p^2 + H3^2 * s_e^2 + G13 * s_p * s_e
  v_up <- H1^2 * s_p^2 + G3^2 * s_e^2 + H13 * s_p * s_e
  gamma_p <- (s_p - s_e)/(o*r)

  gamma_p_lower <- gamma_p - (sqrt(v_lp))/(o*r)
  gamma_p_upper <- gamma_p + (sqrt(v_up))/(o*r)

  #confidence interval for gamma_m
  v_lm <- G2^2 * s_o^2 + G3^2 * (p*r - 1)^2 * s_e^2
  v_um <- H2^2 * s_o^2 + H3^2 * (p*r - 1)^2 * s_e^2
  gamma_m <- (s_o + (p*r -1)* s_e)/(p*r)

  gamma_m_lower <- gamma_m - (sqrt(v_lm))/(p*r)
  gamma_m_upper <- gamma_m + (sqrt(v_um))/(p*r)

  #confidence interval for gamma_r
  gamma_r_lower <- (p*(1-G1)*s_p^2 - p * s_p * s_e +
                      p* (F1 - (1 - G1) * F1^2) * s_e^2)/(
                        (o*(p * r -1)*s_p * s_e) + o*(1-G1)*F3*s_p*s_o)
  gamma_r_upper <- (p*(1+H1)*s_p^2 - p* s_p * s_e +
                      p*(F2-(1+H1)*F2^2)*s_e^2)/(
                        (o*(p*r -1)*s_p*s_e) + o*(1+H1)*F4*s_p*s_o)

  #confidence interval for PTR

  #confidence interval for SNR

  #confidence interval for c_p

  #confidence interval for sigma_o
  sigma_o <- (s_o - s_e)/(p*r)
  v_lo <- pmax(0, G2^2 * s_o^2 + H3^2 * s_e^2 + G23*s_o*s_e)
  v_uo <- pmax(0, H2^2 * s_o^2 + G3^2 * s_e^2 + H23*s_o*s_e)
  sigma_o_lower <- sigma_o - (sqrt(v_lo))/(p*r)
  sigma_o_upper <- sigma_o + (sqrt(v_uo))/(p*r)

  #confidence interval for sigma_e
  sigma_e_lower <- (1-G3)*s_e
  sigma_e_upper <- (1+H3)*s_e

  #confidence interval for gamma_y
  gamma_y <- (p*s_p + o*s_o + (p*o*r-p-o)*s_e)/(p*o*r)
  v_lt <- G1^2 * p^2 * s_p^2 + G2^2 * o^2 * s_o^2 + G3^2*(p*o*r - p -o)^2 * s_e^2
  v_ut <- H1^2 * p^2 * s_p^2 + H2^2 * o^2 * s_o^2 + H3^2 * (p*r*o - p -o)^2 * s_e^2
  gamma_y_lower <- gamma_y - (sqrt(v_lt))/(p*o*r)
  gamma_y_upper <- gamma_y + (sqrt(v_ut))/(p*o*r)

  #confidence interval for sigma_p/sigma_e
  sigma_pe_lower <- (1/(o*r))*((s_p/(s_e*F1)) - 1)
  sigma_pe_upper <- (1/(o*r))*((s_p/(s_e*F2)) - 1)

  #confidence interval for sigma_o/sigma_e
  sigma_oe_lower <- (1/(p*r))*((s_o/(s_e * F5)) - 1)
  sigma_oe_upper <- (1/(p*r))*((s_o/(s_e * F5)) - 1)

  #confidence interval for sigma_o/gamma_y
  l_star <- (o*(1-G2)*s_o^2 - o*s_o*s_e + o*(F5-(1-G2)*F5^2)*s_e^2)/
    (p*(o*r -1)*s_o*s_e+(p*(1-G2)*s_o*s_p)/F4)
  u_star <- (o*(1+H2)*s_o^2 - o*s_o*s_e + o*(F6-(1+H2)*F6^2)*s_e^2)/
    (p(o*r -1)*s_o*s_e+(p*(1+H2)*s_o*s_p)/F3)
  sigmaO_gammaY_lower <- l_star/(1+l_star)
  sigmaO_gammaY_upper <- u_star/(1+u_star)

  #graphing the intervals of PTR and SNR


  #graphing residual plots for the data



  #output for this function, not sure what form that will take


}
