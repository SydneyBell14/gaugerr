unbalanced_one_factor <- function(data, part=P, operator=O,
                                  measurement=Y, alpha=0.05){
  #model for this Y_{ij} = mu_Y + P_i + E_{ij}

  #calculation for n, p, o, and r
  n <- nrow(data)
  p <- nrow(data %>%
              group_by({{part}}) %>%
              summarize(ybar = mean({{measurement}})))
  o <- nrow(data %>%
              group_by({{operator}}) %>%
              summarize(ybar = mean({{measurement}})))
  r <- n/(o*p) # change to r_h
  N <- data %>%
    group_by({{part}}) %>%
    summarize(N = sum({{operator}})) %>%
    ungroup() %>%
    distinct() %>%
    summarize(N = sum(N)) %>%
    pull()
  r_O <- data %>%
    group_by({{part}}) %>%
    summarise(summation = sum({{operator}}^2)) %>%
    ungroup()%>%
    distinct() %>%
    summarize(sum = sum(summation)) %>%
    summarise(r_O = (N-.data$sum/N)/(p-1)) %>%
    pull()
  r_h <- data %>%
    group_by({{part}}) %>%
    summarize(summation = sum(1/{{operator}})) %>%
    ungroup()%>%
    distinct() %>%
    summarize(sum = sum(summation)) %>%
    summarize(r_h = p/(.data$sum)) %>%
    pull()
  #i=1,...,p
  #j=1,...,r_i

  #calculation for s_p, s_e, ybarI, ybar, s_p_star, and ybar_star
  ybarI <- data %>%
    group_by({{part}}) %>%
    summarise(ybarI = (sum({{measurement}}))/o) %>%
    ungroup()%>%
    distinct() %>%
    summarize(ybarI = mean(.data$ybarI)) %>%
    pull()

  ybar <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(ybar = (sum({{measurement}}))/N) %>%
    ungroup()%>%
    distinct() %>%
    summarize(ybar = mean(.data$ybar)) %>%
    pull()

  s_p <- data %>%
    group_by({{part}}) %>%
    summarize(s_p1 = (sum({{operator}})*(ybarI-ybar)^2)/(p-1)) %>%
    ungroup() %>%
    summarize(s_p = sum(.data$s_p1)) %>%
    pull()

  s_e <- data %>%
    group_by({{part}}, {{operator}}) %>%
    summarize(s_e1 = (sum(({{measurement}}-ybar)^2))/(N-p)) %>%
    ungroup() %>%
    summarize(s_e = sum(.data$s_e1)) %>%
    pull()

  ybar_star <- data %>%
    group_by({{part}}) %>%
    summarize(ybar_star1 = (sum({{part}}))/p) %>%
    ungroup() %>%
    summarize(ybar_star = sum(.data$ybar_star1)) %>%
    pull()

  s_p_star <- data %>%
    summarize(((sum((ybarI - ybar_star)^2))*r_h)/(p-1)) %>%
    pull()

  #constants for the confidence interval calculation
  G1 <- 1- stats::qf(alpha/2, Inf, p-1)
  G2 <- 1- stats::qf(alpha/2, Inf, N-p)
  H1 <- stats::qf(1-alpha/2, Inf, p-1) -1
  H2 <- stats::qf(1- alpha/2, Inf, N-p) -1
  F1 <- stats::qf(1- alpha/2, p-1, N-p)
  F2 <- stats::qf(alpha/2, p-1, N-p)
  G12 <- ((F1-1)^2 -G1^2 * F1^2 - H2^2)/F1
  H12 <- ((1-F2)^2 - H1^2 * F2^2 - G2^2)/F2

  #confidence interval for mu_y
  mu_lower <- ybar_star - sqrt((s_p_star*stats::qf(1-alpha, 1, p-1))/(p*r_h))
  mu_upper <- ybar_star + sqrt((s_p_star*stats::qf(1-alpha, 1, p-1))/(p*r_h))

  #confidence interval for gamma_p (USS modification)
  gamma_p <- (s_p_star - s_e)/r_h
  v_lp <- G1^2*s_p_star^2 + H2^2*s_e^2 + G12*s_p_star*s_e
  v_up <- H1^2*s_p_star^2 + G2^2*s_e^2 + H12*s_p_star*s_e
  gamma_p_lower <- gamma_p - (sqrt(v_lp))/r_h
  gamma_p_upper <- gamma_p + (sqrt(v_up))/r_h

  #confidence interval for gamma_m
  # gamma_m <- s_e
  gamma_m_lower <- (1-G2)*s_e
  gamma_m_upper <- (1+H2)*s_e

  #confidence interval for gamma_r
  gamma_r_lower <- s_p_star/(r_h * s_e * F1) - 1/r_h
  gamma_r_upper <- s_p_star/(r_h * s_e * F2) - 1/r_h

  #confidence interval for PTR

  #confidence interval for SNR

  #confidence interval for C_p


  return("plotting works")


}
