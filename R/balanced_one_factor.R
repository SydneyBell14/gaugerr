#' Unbalanced One Factor
#'
#' @param data a data frame that contains measurements, operators and parts
#' for a Gauge R&R analysis
#' @param part a column of the data frame that has the part labels for the
#' measurements
#' @param measurement a column of the data frame that has measurements of the
#' object collected
#' @param alpha the value for the confidence interval calculation (i.e. 95% CI
#' would have alpha=0.05)
#'
#' @return a data frame of the values of the point estimates and the upper and
#' lower bounds for each estimate
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' mydata <- data.frame(P=c(1,1,2,2,3,3),
#' O=c(1,1,1,1,1,1),Y=c(5.3, 6.5, 5.4, 6.4, 6.9, 5.8))
#' balanced_one_factor(mydata, alpha=0.01)
#'
#' @references Burdick, Richard K., Connie M. Borror, and Douglas C. Montgomery. Design and Analysis of Gauge R&R Studies: Making Decisions with Confidence Intervals in Random and Mixed ANOVA Models. Society for Industrial and Applied Mathematics, 2005.
balanced_one_factor <- function(data, part=P, measurement=Y, alpha=0.05) {
  # the model for this is Y_{ij} = mu_y + P_i + E_{ij}

  #calculation for n, p, and r
  n <- nrow(data)
  p <- nrow(data |>
              group_by({{part}}) |>
              summarize(ybar = mean({{measurement}})))
  r <- n/(p)


  #calculations of ybar, ybarI, s_p and s_e
  y_barI <- data |>
    group_by({{part}}) |>
    summarise(ybarI = mean({{measurement}})) |>
    ungroup() |>
    summarise(y_barI = (sum(.data$ybarI))/r) |>
    pull()

  y_bar <- data |>
    group_by({{part}}, {{measurement}}) |>
    summarise(y_bar = mean({{measurement}})) |>
    ungroup() |>
    summarise(y_bar = (sum(.data$y_bar))/(p*r)) |>
    pull()

  s_p <- data |>
    group_by({{part}}) |>
    mutate(ybarI = mean({{measurement}})) |>
    ungroup() |>
    mutate(ybar = mean({{measurement}})) |>
    summarise(sp = (.data$ybarI-.data$ybar)^2) |>
    distinct() |>
    summarise(s_p = r*sum(.data$sp)/(p-1))

  s_e <- data |>
    group_by({{part}}) |>
    mutate(ybarI = mean({{measurement}})) |>
    ungroup() |>
    group_by({{part}}, {{measurement}}) |>
    mutate(yIJ = mean({{measurement}})) |>
    ungroup() |>
    summarise(se = (.data$yIJ - .data$ybarI)^2) |>
    distinct()|>
    summarise(s_e = sum(.data$se)/(p*(r-1))) |>
    pull()

  #constants
  G1 <- 1-stats::qf(alpha/2, Inf, p-1)
  G2 <- 1-stats::qf(alpha/2, Inf, p*(r-1))
  H1 <- stats::qf(1-alpha/2, Inf, p-1) - 1
  H2 <- stats::qf(1-alpha/2, Inf, p*(r-1)) - 1
  F1 <- stats::qf(1-alpha/2, p-1, p*(r-1))
  F2 <- stats::qf(alpha/2, p-1, p*(r-1))
  G12 <- ((F1-1)^2 - (G1^2 * F1^2) - H2^2)/(F1)
  H12 <- ((1-F2)^2 - (H1^2 * F2^2) - G2^2)/(F2)

  #interval for mu_y
  mu_y <- y_bar
  mu_y_lower <- mu_y - (sqrt((s_p*stats::qf(1-alpha, 1, p-1))/(p*r)))
  mu_y_upper <- mu_y + (sqrt((s_p*stats::qf(1-alpha, 1, p-1))/(p*r)))

  #interval for gamma_p
  gamma_p <- (s_p - s_e)/r
  v_lp <- G1^2 * s_p^2 + H2^2 * s_e^2 + G12*s_p*s_e
  v_up <- H1^2 * s_p^2 + G2^2 * s_e^2 + H12*s_p*s_e
  gamma_p_lower <- gamma_p - (sqrt(v_lp))/r
  gamma_p_upper <- gamma_p + (sqrt(v_up))/r

  #interval for gamma_m
  gamma_m <- s_e
  gamma_m_lower <- (1-G2)*s_e
  gamma_m_upper <- (1+H2)*s_e

  #interval for gamma_r
  gamma_r <- ((s_p/s_e)-1)/r
  gamma_r_lower <- ((s_p)/(r*s_e*F1))-(1/r)
  gamma_r_upper <- ((s_p)/(r*s_e*F2))-(1/r)


  #putting it together in a data frame

  #the columns
  quantity <- c("mu", "part", "gamma_m", "gamma_r")
  estimate <- c(mu_y, gamma_p, gamma_m, gamma_r)
  lower <- c(mu_y_lower, gamma_p_lower, gamma_m_lower, gamma_r_lower)
  upper <- c(mu_y_upper, gamma_p_upper, gamma_m_upper, gamma_r_upper)

  # cleaning the data for the data frame output using tidyr techniques
  upper.bounds <- data.frame(upper) |> pivot_longer(cols = everything(),
                                                    names_to = "estimate", values_to = "upper")|>
    select(2)
  lower.bounds <- data.frame(lower) |> pivot_longer(cols = everything(),
                                                    names_to = "estimate", values_to = "lower") |>
    select(2)
  estimate.value <- data.frame(estimate) |> pivot_longer(cols = everything(),
                                                         names_to = "measure", values_to = "estimate")|>
    select(2)

  #return statement for the data frame with estimate, lower and upper bounds of the CI
  table <- cbind(quantity, estimate.value, lower.bounds, upper.bounds)
  bal_one_factor <- structure(table, class = c("intervals_table", "data.frame"))
  return(bal_one_factor)

}
