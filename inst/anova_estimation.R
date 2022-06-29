# -------------------------------------------------------------------
# Functions for ANOVA-based random-effects estimation
# this includes MLS and GPQ confidence intervals
# -------------------------------------------------------------------

# Function to calculate mean squares for two-way model with intercept
# data must have columns Y, O, P
#' @import data.table
#'
#'
library(readr)
library(data.table)

data1 <- read_csv("long_houf_berman.csv")
data1$P <- data1$Part
data1$O <- data1$Operator
data1$Y <- data1$y


nrow(data.frame(data1))


data2<- read_delim("kf_rr.csv", delim="\t")
balanced_mean_squares <- function(data) {
  DT <- as.data.table(data)

  n <- nrow(DT)

  ybar... <- DT[, .(ybar = mean(Y))]
  ybari.. <- DT[, .(ybar = mean(Y)), keyby = P]
  ybar.j. <- DT[, .(ybar = mean(Y)), keyby = O]
  # reason why this is dublicated??
  ss_e <- DT[, .(ss = sum(((mean(Y) - Y))^2)), keyby = .(O, P)]

  #ss_p <- DT[, .(ss = sum(((mean(Y) - Y))^2)), keyby = P]
  #ss_o <- DT[, .(ss = sum(((mean(Y) - Y))^2)), keyby = O]
  # here is the second time
  ss_e <- DT[, .(ss = sum(((mean(Y) - Y))^2)), keyby = .(O, P)]

  p <- nrow(ybari..)
  o <- nrow(ybar.j.)
  r <- n / (p * o)

  SSP <- (o * r * sum((ybari..$ybar - ybar...$ybar)^2))
  SSO <- (p * r * sum((ybar.j.$ybar - ybar...$ybar)^2))
  SSE <- sum(ss_e$ss)
  SST <- (n - 1) * var(DT$Y)
  SSPO <- SST - sum(SSP, SSO, SSE)

  MSP <- SSP / (p - 1)
  MSO <- SSO / (o - 1)
  MSE <- SSE / (p * o * (r - 1))
  MSPO <- SSPO / ((p - 1) * (o - 1))

  list(msp = MSP, mso = MSO, mspo = MSPO, mse = MSE, p = p, o = o, r = r)
}
balanced_mean_squares(data2)
balanced_mean_squares("kf_rr.csv")$mse



# ANOVA point estimates and REML estimates are equal in this setting
rr_anova_estimates <- function(data) {
  ms <- balanced_mean_squares(data1)

  mse  <- ms$mse
  msp  <- ms$msp
  mso  <- ms$mso
  mspo <- ms$mspo

  p <- ms$p
  o <- ms$o
  r <- ms$r

  # Point estimates
  s2_repeat <- mse
  s2_parts <- max(0, (msp - mspo) / (o * r)) #gamma p
  s2_oper  <- max(0, (mso - mspo) / (p * r)) # gamma o
  s2_po    <- max(0, (mspo - mse) / r) #gamma op
  s2_total <- (p * msp + o * mso + (p*o - p - o) * mspo + p * o * (r - 1) * mse) / (p * o * r) #gamma r
  s2_repro <- max(0, (mso + (p - 1) * mspo - p * mse) / (p * r)) # gamma m
  s2_gauge <- (mso + (p - 1) * mspo + p * (r - 1) * mse) / (p * r) #gamma gauge?

  pg_ratio  <- s2_parts / s2_gauge #pg ratio

  eta <- (s2_oper + s2_po) / s2_repeat #opo ratio

  gt_ratio  <- s2_gauge / s2_total #tg ratio
  rg_ratio  <- s2_repeat / s2_gauge #tg ratio
  pr_ratio  <- s2_parts / s2_repeat #tg ratio
  or_ratio  <- s2_oper / s2_repeat #tg ratio
  por_ratio <- s2_po  / s2_repeat #tg ratio

  RES <- c(s2_parts, s2_oper, s2_po, s2_repeat,
           s2_repro, s2_gauge, s2_total,
           gt_ratio, pg_ratio,
           pr_ratio)
  names(RES) <- c("s2_parts", "s2_oper", "s2_po", "s2_repeat",
                  "s2_repro", "s2_gauge", "s2_total",
                  "s2_gauge / s2_total", "s2_parts / s2_gauge",
                  "s2_parts / s2_repeat")

  return(RES)
}
rr_anova_estimates(data1)


# Using estimates and CIs from Burdick, Borror & Montgomery
# See chapter 3 for balanced two-way designs
rr_anova_cis <- function(data, level = 0.95, ci.type = "mls", N = 1e5){
  ms <- balanced_mean_squares(data2)

  mse  <- ms$mse
  msp  <- ms$msp
  mso  <- ms$mso
  mspo <- ms$mspo

  p <- ms$p
  o <- ms$o
  r <- ms$r

  # Point estimates
  s2_repeat <- mse
  s2_parts <- max(0, (msp - mspo) / (o * r))
  s2_oper  <- max(0, (mso - mspo) / (p * r))
  s2_po    <- max(0, (mspo - mse) / r)
  s2_total <- (p * msp + o * mso + (p*o - p - o) * mspo + p * o * (r - 1) * mse) / (p * o * r)
  s2_repro <- max(0, (mso + (p - 1) * mspo - p * mse) / (p * r))
  s2_gauge <- (mso + (p - 1) * mspo + p * (r - 1) * mse) / (p * r)

  pg_ratio  <- s2_parts / s2_gauge

  eta <- (s2_oper + s2_po) / s2_repeat

  tg_ratio  <- s2_total / s2_gauge
  rg_ratio  <- s2_repeat / s2_gauge
  pr_ratio  <- s2_parts / s2_repeat
  or_ratio  <- s2_oper / s2_repeat
  por_ratio <- s2_po  / s2_repeat

  alpha <- 1 - level
  if(ci.type == "mls") {
    # Modified large-sample (MLS) Intervals
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

    # Parts CI
    v_lp <- G1^2 * msp^2 + H3^2 * mspo^2 + G13 * msp * mspo
    v_up <- H1^2 * msp^2 + G3^2 * mspo^2 + H13 * msp * mspo
    parts.lower <- s2_parts - sqrt(v_lp) / (o * r)
    parts.upper <- s2_parts + sqrt(v_up) / (o * r)

    # Operator CI
    v_lo <- G2^2 * mso^2 + H3^2 * mspo^2 + G23 * mso * mspo
    v_uo <- H2^2 * mso^2 + G3^2 * mspo^2 + H23 * mso * mspo
    oper.lower <- s2_oper - sqrt(v_lo) / (p * r)
    oper.upper <- s2_oper + sqrt(v_uo) / (p * r)

    # Interaction CI
    v_lpo <- G3^2 * mspo^2 + H4^2 * mse^2 + G34 * mspo * mse
    v_upo <- H3^2 * mspo^2 + G4^2 * mse^2 + H34 * mspo * mse
    po.lower <- s2_po - sqrt(v_lpo) / r
    po.upper <- s2_po + sqrt(v_upo) / r

    # Total CI
    v_lt <- G1^2 * p^2 * msp^2 +
      G2^2 * o^2 * mso^2 +
      G3^2 * (p * o - p - o)^2 * mspo^2 +
      G4^2 * (p * o)^2 * (r - 1)^2 * mse^2
    v_ut <- H1^2 * p^2 * msp^2 +
      H2^2 * o^2 * mso^2 +
      H3^2 * (p * o - p - o)^2 * mspo^2 +
      H4^2 * (p * o)^2 * (r - 1)^2 * mse^2
    total.lower <- s2_total - sqrt(v_lt) / (p * o * r)
    total.upper <- s2_total + sqrt(v_ut) / (p * o * r)

    # Reapeatibility CI
    repeat.lower <- (1 - G4) * mse
    repeat.upper <- (1 + H4) * mse

    # Reproducibility CI
    v_lrepro <- (G2^2 * mso^2 +
                   G3^2 * (p - 1)^2 * mspo^2 +
                   H4^2 * p^2 * mse^2 +
                   G24 * p * mso * mse +
                   G34 * (p - 1) * p * mspo * mse +
                   G23.star * (p - 1) * mso * mspo) / (p * r)^2
    v_urepo <- (H2^2 * mso^2 +
                  H3^2 * (p - 1)^2 * mspo^2 +
                  G4^2 * p^2 * mse^2 +
                  H24 * p * mso * mse +
                  H34 * (p - 1) * p * mspo * mse) / (p * r)^2
    repro.lower <- s2_repro - sqrt(v_lrepro)
    repro.upper <- s2_repro + sqrt(v_urepo)

    # Gauge (measurement process) CI
    v_lm <- G2^2 * mso^2 + G3^2 * (p - 1)^2 * mspo^2 + G4^2 * p^2 * (r - 1)^2 * mse^2
    v_um <- H2^2 * mso^2 + H3^2 * (p - 1)^2 * mspo^2 + H4^2 * p^2 * (r - 1)^2 * mse^2
    gauge.lower <- s2_gauge - sqrt(v_lm) / (p * r)
    gauge.upper <- s2_gauge + sqrt(v_um) / (p * r)

    # Parts / Gauge CI
    pg_ratio_lower <- (p * (1 - G1) * (msp - F1 * mspo)) /
      (p * o * (r - 1) * mse + o * (1 - G1) * F3 * mso + o * (p - 1) * mspo)
    pg_ratio_upper <- (p * (1 + H1) * (msp - F2 * mspo)) /
      (p * o * (r - 1) * mse + o * (1 + H1) * F4 * mso + o * (p - 1) * mspo)

    # Total / Gauge CI
    tg_ratio_lower <- 1 + pg_ratio_lower
    tg_ratio_upper <- 1 + pg_ratio_upper

    # Parts / Repeat CI
    pr_ratio_lower <- (mspo / (o * r * mse * F9)) *
      ((msp / mspo) - (1 / (1 - G1)) + (mspo * F1 * (1 - F1 * (1 - G1))) / (msp * (1 - G1)))

    pr_ratio_upper <- (mspo / (o * r * mse * F10)) *
      ((msp / mspo) - (1 / (1 + H1)) + (mspo * F2 * (1 - F2 * (1 + H1))) / (msp * (1 + H1)))

    # Repeat / Gauge CI
    #aL <- (1 - 2 / (p * o * (r - 1)) - 1 / qf(1 - alpha / 2, o - 1, p * o * (r - 1)))^2
    #bL <- (1 - 2 / (p * o * (r - 1)) - 1 / qf(1 - alpha / 2, (p - 1) * (o - 1), p * o * (r - 1)))^2
    #cL <- (1 - 2 / (p * o * (r - 1)) - 1 / qf(1 - alpha / 2, p * (o - 1), p * o * (r - 1)))^2 * p^2 / (p - 1) -
      #aL / (p - 1) - (p - 1) * bL
    #aU <- (1 / qf(alpha / 2, o - 1, p * o * (r - 1)) - 1 + 2 / (p * o * (r - 1)))^2
    #bU <- (1 / qf(alpha / 2, (p - 1) * (o - 1), p * o * (r - 1)) - 1 + 2 / (p * o * (r - 1)))^2
    #cU <- (1 / qf(alpha / 2, p * (o - 1), p * o * (r - 1)) - 1 + 2 / (p * o * (r - 1)))^2 * p^2 / (p - 1) -
      #aU / (p - 1) - (p - 1) * bU

    #v_leta <- aL * mso^2 + bL * (p - 1)^2 * mspo^2 + cL * (p - 1) * mso * mspo
    #v_ueta <- aU * mso^2 + bU * (p - 1)^2 * mspo^2 + cU * (p - 1) * mso * mspo
    #if(v_ueta < 0) v_ueta <- 0

    #eta.lower <- (1 / (p * r * mse)) * ((1 - 2 / ((p * o * (r - 1)))) * (mso + (p - 1) * mspo) - sqrt(v_leta)) - 1 / r
    #eta.upper <- (1 / (p * r * mse)) * ((1 - 2 / ((p * o * (r - 1)))) * (mso + (p - 1) * mspo) + sqrt(v_ueta)) - 1 / r

    #rg_ratio_lower <- 1 / (eta.upper + 1)
    #rg_ratio_upper <- 1 / (eta.lower + 1)

    RES <- data.frame(
      quantity = c("s2_parts", "s2_oper", "s2_po", "s2_repeat",
                   "s2_repro", "s2_gauge", "s2_total",
                   "s2_gauge / s2_total", "s2_parts / s2_gauge",
                   "s2_parts / s2_repeat"),
      estimate = c(s2_parts, s2_oper, s2_po, s2_repeat,
                   s2_repro, s2_gauge, s2_total,
                   1/tg_ratio, pg_ratio,
                   pr_ratio),
      lower = c(parts.lower, oper.lower, po.lower, repeat.lower,
                repro.lower, gauge.lower, total.lower,
                1/tg_ratio_upper, pg_ratio_lower,
                pr_ratio_lower),
      upper = c(parts.upper, oper.upper, po.upper, repeat.upper,
                repro.upper, gauge.upper, total.upper,
                1/tg_ratio_lower, pg_ratio_upper,
                pr_ratio_upper)
    ) %>%
      mutate(
        lower = pmax(0, lower)
      )
  }

  if(ci.type == "gpq") {
    W1 <- rchisq(N, p - 1)
    W2 <- rchisq(N, o - 1)
    W3 <- rchisq(N, (o - 1) * (p - 1))
    W4 <- rchisq(N, p * o * (r - 1))

    gpq_part  <- pmax(0, ((p - 1) * msp) / (o * r * W1) -
                        ((p - 1) * (o - 1) * mspo) / (o * r * W3))

    gpq_oper <- pmax(0, ((o - 1) * mso) / (p * r * W2) -
                       ((p - 1) * (o - 1) * mspo) / (p * r * W3))

    gpq_po <- pmax(0, ((p - 1) * (o - 1) * mspo) / (r * W3) -
                     (p * o * (r - 1) * mse) / (r * W4))

    gpq_total <- ((p - 1) * msp) / (o * r * W1) + ((o - 1) * mso) / (p * r * W2) +
      (((p * o - p - o) * (p - 1) * (o - 1)) * mspo) / (p * o * r * W3) +
      (p * o * (r - 1)^2 * mse) / (r * W4)

    gpq_repro <- pmax(0,
                      ((o - 1) * mso) / (p * r * W2) +
                        (1 / r) * (1 - 1/ p) * ((p - 1) * ((o - 1) * mspo) / W3) -
                        (p * o * (r - 1) * mse) / (r * W4) )

    gpq_gauge <- ((o - 1) * mso) / (p * r * W2) +
      ((p - 1)^2 * (o - 1) * mspo) / (p * r * W3) +
      (p * o * (r - 1)^2 * mse) / (r * W4)

    # CI for repeatibility exact according to SIAM book, so no GPQ
    G4 <- 1 - qf(alpha/2, Inf, p * o * (r - 1))
    H4 <- qf(1 - alpha/2, Inf, p * o * (r - 1)) - 1

    repeat.lower <- (1 - G4) * mse
    repeat.upper <- (1 + H4) * mse

    gpq_repeat <- (p * o * (r - 1) * mse) / W4

    gpq_gauge_total <- gpq_gauge / gpq_total
    gpq_repeat_total <- gpq_repeat / gpq_total
    gpq_repro_total <- gpq_repro / gpq_total

    gpq_part_repeat <- gpq_part / gpq_repeat
    gpq_part_repro <- gpq_part / gpq_repro
    gpq_part_gauge <- gpq_part / gpq_gauge

    gpq_repeat_gauge <- gpq_repeat / gpq_gauge


    probs <- c(alpha/2, 1 - alpha/2)
    RES <- data.frame(
      quantity = c("s2_parts", "s2_oper", "s2_po", "s2_repeat", "s2_repro",
                   "s2_gauge", "s2_total","s2_gauge / s2_total",
                   "s2_parts / s2_gauge", "s2_parts / s2_repeat"),
      estimate = c(s2_parts, s2_oper, s2_po, s2_repeat, s2_repro,
                   s2_gauge, s2_total, s2_gauge / s2_total,
                   s2_parts / s2_gauge , s2_parts / s2_repeat)
    )

    lims <- bind_rows(
      quantile(gpq_part, probs, na.rm = TRUE),
      quantile(gpq_oper, probs, na.rm = TRUE),
      quantile(gpq_po, probs, na.rm = TRUE),
      quantile(gpq_repeat, probs, na.rm = TRUE),
      quantile(gpq_repro, probs, na.rm = TRUE),
      quantile(gpq_gauge, probs, na.rm = TRUE),
      quantile(gpq_total, probs, na.rm = TRUE),
      quantile(gpq_gauge_total, probs, na.rm = TRUE),
      quantile(gpq_part_gauge, probs, na.rm = TRUE),
      quantile(gpq_part_repeat, probs, na.rm = TRUE),
    )
    colnames(lims) <- c("lower", "upper")

    RES <- bind_cols(RES, lims)
  }

  RES
}

rr_anova_cis(data2, ci.type = "gpq", level=0.95)
