#' Unbalanced One Factor Model
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
#'
#' @export
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' mydata <- data.frame(P=c(1,1,2,2,3,3),O=c(1,2,1,2,1,2),Y=c(5.3, 6.5, 5.4, 6.4, 6.9, 5.8))
#' unbalanced_one_factor(mydata, alpha=0.01)
#'
#' @references Burdick, Richard K., Connie M. Borror, and Douglas C. Montgomery. Design and Analysis of Gauge R&R Studies: Making Decisions with Confidence Intervals in Random and Mixed ANOVA Models. Society for Industrial and Applied Mathematics, 2005.
unbalanced_one_factor <- function(data, part=P, operator=O,
                                  measurement=Y, alpha=0.05){
  #model for this Y_{ij} = mu_Y + P_i + E_{ij}

  #calculation for n, p, o, and r
  n <- nrow(data)
  p <- nrow(data |>
              group_by({{part}}) |>
              summarize(ybar = mean({{measurement}})))
  r_h <- data |>
    group_by({{part}}) |>
    summarize(reps = n()) |>
    mutate(frac = 1/.data$reps) |>
    ungroup()|>
    summarize(r_h = p/(sum(.data$frac))) |>
    pull()

  #i=1,...,p
  #j=1,...,r_i

  #calculation for s_p, s_e, ybarI, ybar, s_p_star, and ybar_star
  ybarI <- data |>
    group_by({{part}}) |>
    summarise(ybarI = mean({{measurement}}))

  ybar <- data |>
    summarize(ybar = mean({{measurement}})) |>
    pull()

  s_p <- data |>
    group_by({{part}}) |>
    mutate(ybarI = mean({{measurement}}),
              num = n()) |>
    ungroup() |>
    mutate(ybar = mean({{measurement}})) |>
    distinct() |>
    summarize(s_p1 = sum(.data$num*(.data$ybarI-.data$ybar)^2)/(p-1)) |>
    pull()

  s_e <- data |>
    group_by({{part}}) |>
    mutate(ybarI = mean({{measurement}})) |>
    ungroup() |>
    group_by({{part}}, {{measurement}}) |>
    summarize(s_e1 = (({{measurement}}-.data$ybarI)^2)) |>
    ungroup() |>
    distinct() |>
    summarize(s_e = sum(.data$s_e1)/(n-p)) |>
    pull()

  ybar_star <- data |>
    group_by({{part}}) |>
    summarize(ybar_star1 = (mean({{measurement}}))) |>
    ungroup() |>
    summarize(ybar_star = (sum(.data$ybar_star1))/p) |>
    pull()

  s_p_star <- data |>
    group_by({{part}}) |>
    summarize(ybar_star1 = (mean({{measurement}})),
              ybarI = mean({{measurement}})) |>
    ungroup() |>
    mutate(ybar_star = (sum(.data$ybar_star1))/p) |>
    summarize(s_p_star = (r_h * sum((.data$ybarI-.data$ybar_star)^2))/(p-1)) |>
    pull()


  #constants for the confidence interval calculation
  G1 <- 1- stats::qf(alpha/2, Inf, p-1)
  G2 <- 1- stats::qf(alpha/2, Inf, n-p)
  H1 <- stats::qf(1-alpha/2, Inf, p-1) -1
  H2 <- stats::qf(1-alpha/2, Inf, n-p) -1
  F1 <- stats::qf(1-alpha/2, p-1, n-p)
  F2 <- stats::qf(alpha/2, p-1, n-p)
  G12 <- ((F1-1)^2 -G1^2 * F1^2 - H2^2)/F1
  H12 <- ((1-F2)^2 - H1^2 * F2^2 - G2^2)/F2

  #confidence interval for mu_y
  mu_y <- ybar_star
  mu_lower <- ybar_star - sqrt((s_p_star*stats::qf(1-alpha, 1, p-1))/(p*r_h))
  mu_upper <- ybar_star + sqrt((s_p_star*stats::qf(1-alpha, 1, p-1))/(p*r_h))

  #confidence interval for gamma_p (USS modification)
  gamma_p <- pmax(0,(s_p_star - s_e)/r_h)
  v_lp <- G1^2*s_p_star^2 + H2^2*s_e^2 + G12*s_p_star*s_e
  v_up <- H1^2*s_p_star^2 + G2^2*s_e^2 + H12*s_p_star*s_e
  gamma_p_lower <- gamma_p - (sqrt(v_lp))/r_h
  gamma_p_upper <- gamma_p + (sqrt(v_up))/r_h

  #confidence interval for gamma_m
  gamma_m <- s_e
  gamma_m_lower <- (1-G2)*s_e
  gamma_m_upper <- (1+H2)*s_e

  #confidence interval for gamma_r
  gamma_r <- ((s_p_star-s_e)-1)/r_h
  gamma_r_lower <- s_p_star/(r_h * s_e * F1) - (1/r_h)
  gamma_r_upper <- s_p_star/(r_h * s_e * F2) - (1/r_h)

  #confidence interval for PTR

  #confidence interval for SNR

  #confidence interval for C_p

  quantity <- c("part", "gauge", "mu", "gamma_r")
  estimate <- c(gamma_p, gamma_m, mu_y, gamma_r)
  lower <- c(gamma_p_lower, gamma_m_lower, mu_lower, gamma_r_lower)
  upper <- c(gamma_p_upper, gamma_m_upper, mu_upper, gamma_r_upper)

  # cleaning the data for the data frame output using tidyr techniques
  upper.bounds <- data.frame(upper) |>
    pivot_longer(cols = everything(), names_to = "estimate", values_to = "upper") |>
    select(2)
  lower.bounds <- data.frame(lower) |>
    pivot_longer(cols = everything(), names_to = "estimate", values_to = "lower") |>
    select(2)
  estimate.value <- data.frame(estimate) |>
    pivot_longer(cols = everything(), names_to = "measure", values_to = "estimate")|>
    select(2)

  table <- cbind(quantity, estimate.value, lower.bounds, upper.bounds)
  unbal_one_factor <- structure(table, class = c("intervals_table", "data.frame"))

  #return statement for the data frame with estimate, lower and upper bounds of the CI
  return(unbal_one_factor)
}
