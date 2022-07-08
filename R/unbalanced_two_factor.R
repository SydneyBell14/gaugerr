unbalanced_two_factor <- function(data, part=P, operator=O,
                                  measurement=Y, alpha=0.05) {
  # the model that we will be using for this study
  # Y_{ijk} = mu_Y + P_i + O_j + (PO)_{ij} + E_{ijk}

  #calculation for n, p, o, and r
  n <- nrow(data)
  p <- nrow(data %>%
              group_by({{part}}) %>%
              summarize(ybar = mean({{measurement}})))
  o <- nrow(data %>%
              group_by({{operator}}) %>%
              summarize(ybar = mean({{measurement}})))
  r <- n/(o*p)


  #constants used for confidence interval calculation
  G1 <- 1-stats::qf(alpha/2, Inf, p-1)
  G2 <- 1-stats::qf(alpha/2, Inf, o-1)
  G3 <- 1-stats::qf(alpha/2, Inf, (p-1)*(o-1))
  G4 <- 1-stats::qf(alpha/2, Inf, N-(p*o))
  H1 <- stats::qf(1-alpha/2, Inf, p-1) - 1
  H2 <- stats::qf(1-alpha/2, Inf, o-1) - 1
  H3 <- stats::qf(1-alpha/2, Inf, (p-1)(o-1)) - 1
  H4 <- stats::qf(1-alpha/2, Inf, N-(p*o)) - 1
  F1 <- stats::qf(1-alpha/2, p-1,(p-1)*(o-1))
  F2 <- stats::qf(alpha/2, p-1, (p-1)*(o-1))
  F3 <- stats::qf(1-alpha/2, p-1, o-1)
  F4 <- stats::qf(alpha/2, p-1, o-1)
  G13 <- ((F1-1)^2 - G1^2*F1^2 - H3^2)/F1
  H13 <- ((1-F2)^2 - H1^2*F2^2 - G3^2)/F2

}
