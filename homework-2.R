############################################################
# Homework 2 — Olta Recica and Olsa Berani (DSDM)
############################################################

# Run these once to install, then you can comment them out
# install.packages("vars",   repos = "https://cloud.r-project.org")
# install.packages("readxl", repos = "https://cloud.r-project.org")
# install.packages("dplyr",  repos = "https://cloud.r-project.org")

rm(list = ls())

library(vars)
library(readxl)
library(dplyr)

# ── Load raw data ──────────────────────────────────────────

epu_raw    <- read_excel("DATA/US_Policy_Uncertainty_Data.xlsx")
payems_raw <- read.csv("DATA/PAYEMS.csv")
indpro_raw <- read.csv("DATA/INDPRO.csv")
pcepi_raw  <- read.csv("DATA/PCEPI.csv")

# ── Clean and prepare EPU ──────────────────────────────────

epu <- epu_raw %>%
  rename(year = Year, month = Month, epu = News_Based_Policy_Uncert_Index) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  filter(year > 1985 | (year == 1985 & month >= 1)) %>%
  filter(year < 2023  | (year == 2023  & month <= 3)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  arrange(date) %>%
  select(date, epu)

# ── Clean PAYEMS, INDPRO, PCEPI and compute mom growth rates ──

payems <- payems_raw %>%
  mutate(date = as.Date(observation_date)) %>%
  filter(date >= as.Date("1985-01-01") & date <= as.Date("2023-03-01")) %>%
  arrange(date) %>%
  mutate(payems_g = 100 * (PAYEMS / lag(PAYEMS) - 1)) %>%
  select(date, payems_g)

indpro <- indpro_raw %>%
  mutate(date = as.Date(observation_date)) %>%
  filter(date >= as.Date("1985-01-01") & date <= as.Date("2023-03-01")) %>%
  arrange(date) %>%
  mutate(indpro_g = 100 * (INDPRO / lag(INDPRO) - 1)) %>%
  select(date, indpro_g)

pcepi <- pcepi_raw %>%
  mutate(date = as.Date(observation_date)) %>%
  filter(date >= as.Date("1985-01-01") & date <= as.Date("2023-03-01")) %>%
  arrange(date) %>%
  mutate(pcepi_g = 100 * (PCEPI / lag(PCEPI) - 1)) %>%
  select(date, pcepi_g)

# ── Merge all into one dataset ─────────────────────────────

my_data <- epu %>%
  inner_join(payems, by = "date") %>%
  inner_join(indpro, by = "date") %>%
  inner_join(pcepi,  by = "date") %>%
  filter(!is.na(payems_g) & !is.na(indpro_g) & !is.na(pcepi_g))

print(head(my_data))
print(tail(my_data))
cat("Rows:", nrow(my_data), "\n")

# ── Clean EPU for plotting (year/month columns retained) ──

epu_raw_clean <- epu_raw %>%
  rename(year = Year, month = Month, epu = News_Based_Policy_Uncert_Index) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year))

# ── Dataset verification plot (replicates DataFigure.png) ─

dir.create("output", showWarnings = FALSE)

png("output/data_figure.png", width = 1200, height = 800)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

pcepi_plot <- pcepi_raw[
  as.Date(pcepi_raw$observation_date) >= as.Date("1985-01-01") &
    as.Date(pcepi_raw$observation_date) <= as.Date("2023-03-01"),
]
plot(as.Date(pcepi_plot$observation_date), pcepi_plot$PCEPI,
     type = "l", col = "steelblue", main = "PCEPI (levels)",
     xlab = "Date", ylab = "Index")

payems_plot <- payems_raw[
  as.Date(payems_raw$observation_date) >= as.Date("1985-01-01") &
    as.Date(payems_raw$observation_date) <= as.Date("2023-03-01"),
]
plot(as.Date(payems_plot$observation_date), payems_plot$PAYEMS,
     type = "l", col = "steelblue", main = "PAYEMS (levels, thousands)",
     xlab = "Date", ylab = "Thousands of persons")

indpro_plot <- indpro_raw[
  as.Date(indpro_raw$observation_date) >= as.Date("1985-01-01") &
    as.Date(indpro_raw$observation_date) <= as.Date("2023-03-01"),
]
plot(as.Date(indpro_plot$observation_date), indpro_plot$INDPRO,
     type = "l", col = "steelblue", main = "INDPRO (levels)",
     xlab = "Date", ylab = "Index (2017=100)")

epu_plot <- epu_raw_clean[
  epu_raw_clean$year > 1985 |
    (epu_raw_clean$year == 1985 & epu_raw_clean$month >= 1),
]
epu_plot <- epu_plot[
  epu_plot$year < 2023 |
    (epu_plot$year == 2023 & epu_plot$month <= 3),
]
plot(as.Date(paste(epu_plot$year, epu_plot$month, "01", sep = "-")), epu_plot$epu,
     type = "l", col = "steelblue", main = "News-Based Policy Uncertainty Index (levels)",
     xlab = "Date", ylab = "Index")

dev.off()
cat("Data verification plot saved to output/data_figure.png\n")

# ── Estimate VAR(1) on 1985M2–2019M12 ─────────────────────

sample <- my_data[my_data$date <= as.Date("2019-12-01"), ]

myvar <- subset(sample, select = c(epu, payems_g, indpro_g, pcepi_g))

var.est1 <- VAR(myvar, p = 1, type = "const", season = NULL)

summary(var.est1)

# ── Create output folder ───────────────────────────────────

dir.create("output", showWarnings = FALSE)

# ── Structural VAR (Cholesky / recursive identification) ───

a.mat <- diag(4)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[4, 1] <- NA
a.mat[3, 2] <- NA
a.mat[4, 2] <- NA
a.mat[4, 3] <- NA

b.mat <- diag(4)
diag(b.mat) <- NA

svar.one <- SVAR(var.est1, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)

# ── IRFs for PAYEMS and INDPRO ─────────────────────────────

irf_payems <- irf(svar.one, impulse = "epu", response = "payems_g",
                  n.ahead = 24, ortho = TRUE, boot = TRUE, ci = 0.95,
                  runs = 1000, seed = 123)

irf_indpro <- irf(svar.one, impulse = "epu", response = "indpro_g",
                  n.ahead = 24, ortho = TRUE, boot = TRUE, ci = 0.95,
                  runs = 1000, seed = 123)

plot_irf_response <- function(irf_result, response, main) {
  horizon <- 0:24
  point <- irf_result$irf$epu[, response]
  lower <- irf_result$Lower$epu[, response]
  upper <- irf_result$Upper$epu[, response]

  plot(horizon, point, type = "l", col = "black", lwd = 1.5,
       ylim = range(c(lower, upper, 0)), main = main,
       xlab = "Months after shock", ylab = "Response (percentage points)")
  lines(horizon, lower, col = "firebrick", lty = 2)
  lines(horizon, upper, col = "firebrick", lty = 2)
  abline(h = 0, col = "grey50", lty = 3)
  legend("bottomright", legend = c("Point estimate", "95% bootstrap confidence band"),
         col = c("black", "firebrick"), lty = c(1, 2), lwd = c(1.5, 1),
         bty = "n")
}

png("output/irf_payems.png", width = 800, height = 600)
plot_irf_response(irf_payems, "payems_g",
                  "IRF: EPU shock → Employment growth (PAYEMS)")
dev.off()

png("output/irf_indpro.png", width = 800, height = 600)
plot_irf_response(irf_indpro, "indpro_g",
                  "IRF: EPU shock → Industrial production growth (INDPRO)")
dev.off()

# ── Save all numerical output to file ─────────────────────

P_est <- solve(svar.one$A) %*% svar.one$B
rownames(P_est) <- c("epu", "payems_g", "indpro_g", "pcepi_g")
colnames(P_est) <- c("shock_epu", "shock_payems", "shock_indpro", "shock_pcepi")

sink("output/var_results.txt")
cat("============================================================\n")
cat("REDUCED-FORM VAR(1) RESULTS\n")
cat("Estimation sample: 1985M2 - 2019M12\n")
cat("============================================================\n\n")
print(summary(var.est1))
cat("\n============================================================\n")
cat("COVARIANCE MATRIX OF RESIDUALS\n")
cat("============================================================\n\n")
print(round(summary(var.est1)$covres, 6))
cat("\n============================================================\n")
cat("STRUCTURAL IMPACT MATRIX P = A0^{-1} B\n")
cat("============================================================\n\n")
print(round(P_est, 6))
cat("\n============================================================\n")
cat("IRF VALUES: EPU shock → PAYEMS\n")
cat("============================================================\n\n")
print(irf_payems)
cat("\n============================================================\n")
cat("IRF VALUES: EPU shock → INDPRO\n")
cat("============================================================\n\n")
print(irf_indpro)
sink()

cat("All output saved to output/var_results.txt\n")

############################################################
# PART II — Forecasting
############################################################

# ── Extract VAR coefficients ───────────────────────────────

phi1 <- var.est1$varresult$epu$coefficients
phi2 <- var.est1$varresult$payems_g$coefficients
phi3 <- var.est1$varresult$indpro_g$coefficients
phi4 <- var.est1$varresult$pcepi_g$coefficients

# ── Set up forecast data frame ─────────────────────────────

# create index relative to March 2023 (forecast origin = 0)
my_data$index    <- 1:nrow(my_data)
origin_idx       <- which(my_data$date == as.Date("2023-03-01"))
my_data$time_fcast <- my_data$index - origin_idx

f_unco <- data.frame(
  date       = my_data$date,
  time_fcast = my_data$time_fcast,
  epu        = my_data$epu,
  payems_g   = my_data$payems_g,
  indpro_g   = my_data$indpro_g,
  pcepi_g    = my_data$pcepi_g
)

# add 12 empty rows for forecast horizon
future_dates <- seq(as.Date("2023-04-01"), by = "month", length.out = 12)
future_rows  <- data.frame(
  date       = future_dates,
  time_fcast = 1:12,
  epu        = NA,
  payems_g   = NA,
  indpro_g   = NA,
  pcepi_g    = NA
)
f_unco <- rbind(f_unco, future_rows)

# ── Unconditional forecast (12 horizons) ───────────────────

varnames <- c("epu", "payems_g", "indpro_g", "pcepi_g")

for (i in 1:12) {
  prev <- as.numeric(f_unco[f_unco$time_fcast == i - 1, varnames])
  f_unco$epu     [f_unco$time_fcast == i] <- prev %*% phi1[1:4] + phi1[5]
  f_unco$payems_g[f_unco$time_fcast == i] <- prev %*% phi2[1:4] + phi2[5]
  f_unco$indpro_g[f_unco$time_fcast == i] <- prev %*% phi3[1:4] + phi3[5]
  f_unco$pcepi_g [f_unco$time_fcast == i] <- prev %*% phi4[1:4] + phi4[5]
}

# print forecast values
cat("Unconditional forecasts — PAYEMS growth:\n")
print(f_unco[f_unco$time_fcast >= 1, c("date", "payems_g")])
cat("\nUnconditional forecasts — INDPRO growth:\n")
print(f_unco[f_unco$time_fcast >= 1, c("date", "indpro_g")])

# ── Plot and save unconditional forecasts ──────────────────

fcast_window <- f_unco[f_unco$time_fcast >= -12 & !is.na(f_unco$time_fcast), ]

png("output/unco_payems.png", width = 900, height = 500)
plot(f_unco$date[f_unco$time_fcast <= 0],
     f_unco$payems_g[f_unco$time_fcast <= 0],
     type = "l", col = "black", lwd = 1.5,
     xlim = range(fcast_window$date),
     ylim = range(fcast_window$payems_g, na.rm = TRUE),
     main = "Unconditional Forecast: Employment Growth (PAYEMS)",
     xlab = "Date", ylab = "Month-on-month growth rate (%)")
lines(f_unco$date[f_unco$time_fcast >= 0],
      f_unco$payems_g[f_unco$time_fcast >= 0],
      col = "dodgerblue", lwd = 2)
abline(v = as.Date("2023-03-01"), col = "red", lty = 2)
abline(h = 0, col = "grey70", lty = 3)
legend("topleft", legend = c("Observed", "Forecast", "Forecast origin (Mar 2023)"),
       col = c("black", "dodgerblue", "red"), lty = c(1, 1, 2), lwd = c(1.5, 2, 1), bty = "n")
dev.off()

png("output/unco_indpro.png", width = 900, height = 500)
plot(f_unco$date[f_unco$time_fcast <= 0],
     f_unco$indpro_g[f_unco$time_fcast <= 0],
     type = "l", col = "black", lwd = 1.5,
     xlim = range(fcast_window$date),
     ylim = range(fcast_window$indpro_g, na.rm = TRUE),
     main = "Unconditional Forecast: Industrial Production Growth (INDPRO)",
     xlab = "Date", ylab = "Month-on-month growth rate (%)")
lines(f_unco$date[f_unco$time_fcast >= 0],
      f_unco$indpro_g[f_unco$time_fcast >= 0],
      col = "dodgerblue", lwd = 2)
abline(v = as.Date("2023-03-01"), col = "red", lty = 2)
abline(h = 0, col = "grey70", lty = 3)
legend("topleft", legend = c("Observed", "Forecast", "Forecast origin (Mar 2023)"),
       col = c("black", "dodgerblue", "red"), lty = c(1, 1, 2), lwd = c(1.5, 2, 1), bty = "n")
dev.off()

cat("Forecast plots saved.\n")

############################################################
# PART II.2 — Structural forecast (election EPU scenario)
############################################################

# ── Normalize the Cholesky impact matrix ──────────────────

chol_est <- t(chol(summary(var.est1)$covres))
chol_normalized <- chol_est / chol_est[1, 1]

chol_11 <- chol_normalized[1, 1]  # EPU
chol_21 <- chol_normalized[2, 1]  # PAYEMS
chol_31 <- chol_normalized[3, 1]  # INDPRO
chol_41 <- chol_normalized[4, 1]  # PCEPI

cat("Normalized first column of P:\n")
print(c(chol_11, chol_21, chol_31, chol_41))

# ── EPU scenario path (Apr–Aug 2023 = Oct 2020–Feb 2021) ──

get_epu <- function(y, m) {
  epu_raw_clean$epu[epu_raw_clean$year == y & epu_raw_clean$month == m]
}

epu_scenario <- c(
  get_epu(2020, 10),  # April 2023
  get_epu(2020, 11),  # May 2023
  get_epu(2020, 12),  # June 2023
  get_epu(2021,  1),  # July 2023
  get_epu(2021,  2)   # August 2023
)

cat("EPU scenario path (Apr–Aug 2023):\n")
print(epu_scenario)

# ── Set up structural forecast data frame ─────────────────

f_struct <- data.frame(
  date       = f_unco$date,
  time_fcast = f_unco$time_fcast,
  epu        = f_unco$epu,
  payems_g   = f_unco$payems_g,
  indpro_g   = f_unco$indpro_g,
  pcepi_g    = f_unco$pcepi_g
)

# ── Structural forecast: horizons 1–5 (EPU path imposed) ──

for (i in 1:5) {
  idx   <- which(f_struct$time_fcast == i)
  idx_1 <- which(f_struct$time_fcast == i - 1)

  prev <- as.numeric(f_struct[idx_1, varnames])

  # step 1: reduced-form benchmark
  epu_rf     <- prev %*% phi1[1:4] + phi1[5]
  payems_rf  <- prev %*% phi2[1:4] + phi2[5]
  indpro_rf  <- prev %*% phi3[1:4] + phi3[5]
  pcepi_rf   <- prev %*% phi4[1:4] + phi4[5]

  # step 2: EPU gap
  delta_epu <- epu_scenario[i] - epu_rf

  # step 3: structural adjustment
  f_struct$epu     [idx] <- epu_rf    + chol_11 * delta_epu
  f_struct$payems_g[idx] <- payems_rf + chol_21 * delta_epu
  f_struct$indpro_g[idx] <- indpro_rf + chol_31 * delta_epu
  f_struct$pcepi_g [idx] <- pcepi_rf  + chol_41 * delta_epu
}

# ── Horizons 6–12: EPU evolves endogenously ───────────────

for (i in 6:12) {
  idx   <- which(f_struct$time_fcast == i)
  idx_1 <- which(f_struct$time_fcast == i - 1)

  prev <- as.numeric(f_struct[idx_1, varnames])

  f_struct$epu     [idx] <- prev %*% phi1[1:4] + phi1[5]
  f_struct$payems_g[idx] <- prev %*% phi2[1:4] + phi2[5]
  f_struct$indpro_g[idx] <- prev %*% phi3[1:4] + phi3[5]
  f_struct$pcepi_g [idx] <- prev %*% phi4[1:4] + phi4[5]
}

cat("Structural forecasts — PAYEMS:\n")
print(f_struct[f_struct$time_fcast >= 1, c("date", "epu", "payems_g")])
cat("\nStructural forecasts — INDPRO:\n")
print(f_struct[f_struct$time_fcast >= 1, c("date", "epu", "indpro_g")])


# ── Plot and save structural forecast vs unconditional ─────

struct_window <- f_struct[f_struct$time_fcast >= -12 & !is.na(f_struct$time_fcast), ]

png("output/struct_payems.png", width = 900, height = 500)
plot(f_unco$date[f_unco$time_fcast <= 0],
     f_unco$payems_g[f_unco$time_fcast <= 0],
     type = "l", col = "black", lwd = 1.5,
     xlim = range(struct_window$date),
     ylim = range(c(f_unco$payems_g[f_unco$time_fcast %in% -12:12],
                    f_struct$payems_g[f_struct$time_fcast %in% 1:12]), na.rm = TRUE),
     main = "Employment Growth: Unconditional vs Structural Forecast",
     xlab = "Date", ylab = "Month-on-month growth rate (%)")
lines(f_unco$date[f_unco$time_fcast >= 0],
      f_unco$payems_g[f_unco$time_fcast >= 0],
      col = "dodgerblue", lwd = 2)
lines(f_struct$date[f_struct$time_fcast >= 1],
      f_struct$payems_g[f_struct$time_fcast >= 1],
      col = "firebrick", lwd = 2, lty = 2)
abline(v = as.Date("2023-03-01"), col = "grey40", lty = 3)
abline(h = 0, col = "grey70", lty = 3)
legend("topleft", legend = c("Observed", "Unconditional forecast", "Structural forecast (election EPU)"),
       col = c("black", "dodgerblue", "firebrick"), lty = c(1, 1, 2), lwd = 2, bty = "n")
dev.off()

png("output/struct_indpro.png", width = 900, height = 500)
plot(f_unco$date[f_unco$time_fcast <= 0],
     f_unco$indpro_g[f_unco$time_fcast <= 0],
     type = "l", col = "black", lwd = 1.5,
     xlim = range(struct_window$date),
     ylim = range(c(f_unco$indpro_g[f_unco$time_fcast %in% -12:12],
                    f_struct$indpro_g[f_struct$time_fcast %in% 1:12]), na.rm = TRUE),
     main = "Industrial Production: Unconditional vs Structural Forecast",
     xlab = "Date", ylab = "Month-on-month growth rate (%)")
lines(f_unco$date[f_unco$time_fcast >= 0],
      f_unco$indpro_g[f_unco$time_fcast >= 0],
      col = "dodgerblue", lwd = 2)
lines(f_struct$date[f_struct$time_fcast >= 1],
      f_struct$indpro_g[f_struct$time_fcast >= 1],
      col = "firebrick", lwd = 2, lty = 2)
abline(v = as.Date("2023-03-01"), col = "grey40", lty = 3)
abline(h = 0, col = "grey70", lty = 3)
legend("topleft", legend = c("Observed", "Unconditional forecast", "Structural forecast (election EPU)"),
       col = c("black", "dodgerblue", "firebrick"), lty = c(1, 1, 2), lwd = 2, bty = "n")
dev.off()

cat("Structural forecast plots saved.\n")
