#' Balanced Without Interaction
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
#' @return a data frame of the values of the point estimates and the upper and
#' lower bounds for each estimate
#' @export
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
balanced_no_interaction <- function(data, part=P, operator=O, measurement=Y, alpha=0.05) {
  # the model we are using Y_{ijk}=mu_Y + P_i + O_j + E_{ijk}

  #calculations for the constants n,p,o and r that depend on the data frame
  n <- nrow(data)
  p <- nrow(data %>%
              group_by({{part}}) %>%
              summarize(ybar = mean({{measurement}})))
  o <- nrow(data %>%
              group_by({{operator}}) %>%
              summarize(ybar = mean({{measurement}})))
  r <- n/(o*p)

  #ybar calculations
  ybar <- data %>%
    summarize(ybar_star = (mean({{measurement}}))/(p*o*r))

  ybarI <- data %>%
    group_by({{part}}) %>%
    summarise(sum = sum({{measurement}})) %>%
    summarise(ybarI = (.data$sum)/(o*r))

  ybarJ <- data %>%
    group_by({{operator}}) %>%
    summarise(sum = sum({{measurement}})) %>%
    summarise(ybarJ = (.data$sum)/(p*r))

  ybarIJ <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(sum = sum({{measurement}})) %>%
    summarise(ybarIJ = (.data$sum)/(r))

  #s_p calculation
  s_p <- data %>%
    mutate(ybar = mean({{measurement}})) %>%
    group_by({{part}}) %>%
    summarise(ybarI = mean({{measurement}})) %>%
    ungroup()%>%
    summarise((o*r*sum((ybarI-ybar)^2))/(p-1))

  #s_o calculation
  s_o <- data %>%
    mutate(ybar = mean({{measurement}})) %>%
    group_by({{operator}}) %>%
    summarise(ybarJ = mean({{measurement}})) %>%
    ungroup()%>%
    summarise((p*r*sum((ybarJ - ybar)^2))/(o-1))

  #s_e calculation
  s_e <- data %>%
    mutate(ybar = mean({{measurement}})) %>%
    group_by({{part}}) %>%
    mutate(ybarI = mean({{measurement}})) %>%
    ungroup() %>%
    group_by({{operator}}) %>%
    mutate(ybarJ = mean({{measurement}})) %>%
    ungroup() %>%
    summarise((sum(({{measurement}} - ybarI - ybarJ + ybar)^2))/(p*o*r - p - o + 1))

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
  F5 <- stats::qf(1 - alpha/2, o - 1, (p - 1) *(o - 1))
  F6 <- stats::qf(alpha/2, o - 1, (p - 1) *(o - 1))
  G13 <- ((F1 - 1)^2 - G1^2 * F1^2 - H3^2)/F1
  G23 <- ((F5 - 1)^2 - G2^2 * F5^2 - H3^2) / F5
  H13 <- ((1-F2)^2 - H1^2 * F2^2 - G3^2)/F2
  H23 <- ((1 - F6)^2 - H2^2 * F6^2 - G3^2) / F6

  #confidence interval for mu_Y
  C <- (s_p * sqrt(stats::qf(1-alpha, 1, p-1)) +
          s_o * sqrt(stats::qf(1-alpha, 1, o-1)) -
          s_e * sqrt(stats::qf(1-alpha, 1, (p*o*r)-p-o+1)))
  K <- s_p + s_o - s_e
  if (K < 0){
    K <- s_e
    C <- sqrt(stats::qf(1-alpha, 1, (p*o*r)-p-o+1))
  }
  mu_Y_lower <- ybar - (C * sqrt((K)/(p*o*r)))
  mu_Y_upper <- ybar + (C * sqrt((K)/(p*o*r)))

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

  gamma_r <- gamma_p/gamma_m

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
  sigma_e <- gamma_m - sigma_o
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
    (p*(o*r -1)*s_o*s_e+(p*(1+H2)*s_o*s_p)/F3)
  sigmaO_gammaY_lower <- l_star/(1+l_star)
  sigmaO_gammaY_upper <- u_star/(1+u_star)

  #table of values for the confidence intervals
  quantity <- c("repeat","part","operator", "total",
                "gauge", "pr_ratio")
  estimate <- c(sigma_e, gamma_p, sigma_o, gamma_y, gamma_m,
                 gamma_p/sigma_e)
  lower <- c(sigma_e_lower, gamma_p_lower, sigma_o_lower, gamma_y_lower,
            gamma_m_lower, sigma_pe_lower)
  upper <- c(sigma_e_upper, gamma_p_upper, sigma_o_upper, gamma_y_upper,
            gamma_m_upper, sigma_pe_upper)

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
