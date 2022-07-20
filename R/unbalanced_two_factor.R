#' Unbalanced Two Factor Model
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
#' mydata <- data.frame(P=c(1,1,2,2,3,3),O=c(1,2,1,2,1,2),Y=c(5.3, 6.5, 5.4, 6.4, 6.9, 5.8))
#' unbalanced_two_factor(mydata, alpha=0.01)
unbalanced_two_factor <- function(data, part=P, operator=O, measurement=Y, alpha=0.05) {
  # the model that we will be using for this study
  # Y_{ijk} = mu_Y + P_i + O_j + (PO)_{ij} + E_{ijk}

  #calculation for n, p, o, and r
  n <- nrow(data) #in the book this is N
  p <- nrow(data %>%
              group_by({{part}}) %>%
              summarize(ybar = mean({{measurement}})))
  o <- nrow(data %>%
              group_by({{operator}}) %>%
              summarize(ybar = mean({{measurement}})))
  r <- n - (p*o)

  r_h <- data %>%
    group_by({{part}},{{operator}}) %>%
    summarise(reps = n()) %>%
    mutate(frac = 1/.data$reps) %>%
    ungroup() %>%
    summarize(r_h = (p*o)/(sum(.data$frac))) %>%
    pull()

  ybarIJ <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarise(total = sum({{measurement}}),
              reps = n()) %>%
    summarise(mean = .data$total/.data$reps)

  ybarI_star <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarise(total = sum({{measurement}}),
              reps = n()) %>%
    mutate(mean = .data$total/.data$reps)%>%
    ungroup() %>%
    group_by({{operator}}) %>%
    summarize(ybarI_star = (sum(.data$mean))/o)

  ybarJ_star <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarise(total = sum({{measurement}}),
              reps = n()) %>%
    mutate(mean = .data$total/.data$reps)%>%
    ungroup() %>%
    group_by({{part}}) %>%
    summarise(ybarJ_star = (sum(.data$mean))/p)

  ybar_star <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarise(total = sum({{measurement}}),
              reps = n()) %>%
    mutate(mean = .data$total/.data$reps)%>%
    ungroup() %>%
    summarise(ybar_star = (sum(.data$mean))/(p*o))

  s_p_star <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarise(total = sum({{measurement}}),
              reps = n()) %>%
    mutate(mean = .data$total/.data$reps)%>%
    ungroup() %>%
    mutate(ybar_star = (sum(.data$mean))/(p*o)) %>%
    group_by({{operator}}) %>%
    mutate(ybarI_star = (sum(.data$mean))/o) %>%
    ungroup() %>%
    summarise(s_p_star = (o*r_h*sum((.data$ybar_star - .data$ybarI_star)^2))/(p-1)) %>%
    pull()

  s_o_star <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarise(total = sum({{measurement}}),
              reps = n()) %>%
    mutate(mean = .data$total/.data$reps) %>%
    ungroup() %>%
    mutate(ybar_star = (sum(.data$mean))/(p*o)) %>%
    group_by({{part}}) %>%
    mutate(ybarJ_star = (sum(.data$mean))/p) %>%
    ungroup() %>%
    summarize(s_o_star = (p*r_h*(sum((.data$ybarJ_star-.data$ybar_star)^2)))/(o-1))%>%
    pull()

  s_po_star <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarise(total = sum({{measurement}}),
              reps = n()) %>%
    mutate(mean = .data$total/.data$reps)%>%
    ungroup() %>%
    mutate(ybar_star = (sum(.data$mean))/(p*o)) %>%
    group_by({{part}}) %>%
    mutate(ybarJ_star = (sum(.data$mean))/p) %>%
    ungroup() %>%
    group_by({{operator}}) %>%
    mutate(ybarI_star = (sum(.data$mean))/o) %>%
    ungroup() %>%
    summarize(s_po_star = (r_h * sum((ybarIJ - .data$ybarI_star - .data$ybarJ_star + .data$ybar_star)))/((p-1)*(o-1))) %>%
    pull()

  s_e <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(total = sum({{measurement}}),
              reps = n()) %>%
    mutate(mean = .data$total/.data$reps) %>%
    ungroup() %>%
    summarize(s_e = (sum(.data$total - .data$mean)^2)/(n-(p*o))) %>%
    pull()

  #constants used for confidence interval calculation
  G1 <- 1-stats::qf(alpha/2, Inf, p-1)
  G2 <- 1-stats::qf(alpha/2, Inf, o-1)
  G3 <- 1-stats::qf(alpha/2, Inf, (p-1)*(o-1))
  G4 <- 1-stats::qf(alpha/2, Inf, n-(p*o))
  H1 <- stats::qf(1-alpha/2, Inf, p-1) - 1
  H2 <- stats::qf(1-alpha/2, Inf, o-1) - 1
  H3 <- stats::qf(1-alpha/2, Inf, (p-1)*(o-1)) - 1
  H4 <- stats::qf(1-alpha/2, Inf, n-(p*o)) - 1
  F1 <- stats::qf(1-alpha/2, p-1,(p-1)*(o-1))
  F2 <- stats::qf(alpha/2, p-1, (p-1)*(o-1))
  F3 <- stats::qf(1-alpha/2, p-1, o-1)
  F4 <- stats::qf(alpha/2, p-1, o-1)
  G12 <- ((F1-1)^2 - G1^2 * F1^2 - H2^2)/F1
  H12 <- ((1-F2)^2 - H1^2 * F2^2 - G2^2)/F2
  G13 <- ((F1-1)^2 - G1^2*F1^2 - H3^2)/F1
  H13 <- ((1-F2)^2 - H1^2*F2^2 - G3^2)/F2

  #terms used for the two factor analysis
  K <- s_p_star + s_o_star - s_po_star
  C <- (s_p_star * sqrt(stats::qf(1-alpha, 1, p-1)) + s_o_star * sqrt(stats::qf(1-alpha, 1, (p-1)*(o-1))))/K
  if(K<0){
    K <- s_po_star
    C <- sqrt(stats::qf(1-alpha, 1, (p-1)*(o-1)))
  }

  gamma_p <- (s_p_star - s_po_star)/(o*r_h)
  v_lp <- G1^2 * s_p_star^2 + H3^2 * s_po_star^2 + G13 * s_p_star * s_po_star
  v_up <- H1^2 * s_p_star^2 + G3^2 * s_po_star^2 + H13 * s_p_star * s_po_star

  gamma_m <- (s_o_star + (p-1)*s_po_star + p*(r_h-1)*s_e)/(p*r_h)

  v_lm <- G2^2 * s_o_star^2 + G3^2 * (p-1)^2 * s_po_star^2 + G4^2 * p^2 * (r_h -1)^2 * s_e^2
  v_um <- H2^2 * s_o_star^2 + H3^2 * (p-1)^2 * s_po_star^2 + H4^2 * p^2 * (r_h - 1)^2 * s_e^2

  v_lr <- p*o*(r_h-1)*s_e + o*(1-G1)*F3*s_o_star + o*(p-1)*s_po_star
  v_ur <- p*o*(r_h -1)*s_e + o*(1+H1)*F4*s_o_star + o*(p-1)*s_po_star

  gamma_r <- ((s_p_star/s_e) - 1)/(r_h)

  # interval for mu_y
  mu_y <- ybar_star
  mu_y_lower <- mu_y - C * sqrt(K)/sqrt(p*o*r_h)
  mu_y_upper <- mu_y + C * sqrt(K)/sqrt(p*o*r_h)

  # interval for gamma_p
  gamma_p_lower <- gamma_p - sqrt(v_lp)/(o*r_h)
  gamma_p_upper <- gamma_p + sqrt(v_up)/(o*r_h)

  #interval for gamma_m
  gamma_m_lower <- gamma_m - sqrt(v_lm)/(p*r_h)
  gamma_m_upper <- gamma_m + sqrt(v_um)/(p*r_h)

  #interval for gamma_r
  gamma_r_lower <- (p*(1-G1)*(s_p_star - F1*s_po_star))/(v_lr)
  gamma_r_upper <- (p*(1+H1)*(s_p_star - F2*s_po_star))/(v_ur)

  #interval for PTR
  #PTR_lower <- (k*sqrt(gamma_m_lower))/(USL-LSL)
  #PTR_upper <- (k*sqrt(gamma_m_upper))/(USL-LSL)

  #interval for SNR
  #SNR_lower <- sqrt(2*gamma_r_lower)
  #SNR_upper <- sqrt(2*gamma_r_upper)


  #interval for C_p
  # C_p_lower <- (USL-LSL)/(6*sqrt(gamma_p_upper))
  # C_p_upper <- (USL-LSL)/(6*sqrt(gamma_p_lower))

  quantity <- c("part", "gauge", "mu", "gamma_r")
  estimate <- c(gamma_p, gamma_m, mu_y, gamma_r)
  lower <- c(gamma_p_lower, gamma_m_lower, mu_y_lower, gamma_r_lower)
  upper <- c(gamma_p_upper, gamma_m_upper, mu_y_upper, gamma_r_upper)

  # cleaning the data for the data frame output using tidyr techniques
  upper.bounds <- data.frame(upper) %>%
    pivot_longer(cols = everything(),names_to = "estimate", values_to = "upper")%>%
    select(2)
  lower.bounds <- data.frame(lower) %>%
    pivot_longer(cols = everything(), names_to = "estimate", values_to = "lower") %>%
    select(2)
  estimate.value <- data.frame(estimate) %>%
    pivot_longer(cols = everything(), names_to = "measure", values_to = "estimate")%>%
    select(2)

  #return statement for the data frame with estimate, lower and upper bounds of the CI
  table <- cbind(quantity, estimate.value, lower.bounds, upper.bounds)
  unbal_two_factor <- structure(table, class = "intervals_table")
  return(table)

}
