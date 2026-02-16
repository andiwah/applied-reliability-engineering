# =========================================
# NHPP Forecasting using Crow–AMSAA
# Fleet + PMP population
# =========================================

library(readr)
library(dplyr)
library(ggplot2)

DATA_FILE <- "KM_23_1_inter_failure_dataset_by_population.csv"

# ---- Settings: choose ONE horizon mode ----
HORIZON_MODE <- "PCT"   # "PCT" or "HOURS"
HORIZON_PCT  <- 0.25    # used if HORIZON_MODE == "PCT"
HORIZON_HRS  <- 2000    # used if HORIZON_MODE == "HOURS"

# ---- Load dataset ----
df <- read_csv(DATA_FILE)

# Helper: build cum_time + N from inter-failure intervals (failures only)
build_process <- function(d) {
  d %>%
    filter(status == 1) %>%
    arrange(time) %>%
    mutate(
      cum_time = cumsum(time),
      N = row_number()
    )
}

# Helper: fit Crow–AMSAA via log-log regression, return beta + eta
fit_crow_amsaa <- function(proc_df) {
  fit <- lm(log(N) ~ log(cum_time), data = proc_df)
  beta <- coef(fit)[2]
  eta  <- exp(-coef(fit)[1] / beta)
  list(fit = fit, beta = beta, eta = eta)
}

# Helper: forecast failures between t1 and t2
expected_failures_between <- function(t1, t2, beta, eta) {
  # E[N(t)] = (t/eta)^beta
  (t2 / eta)^beta - (t1 / eta)^beta
}

# Helper: pick forecast horizon
future_time <- function(t_current) {
  if (HORIZON_MODE == "PCT") return(t_current * (1 + HORIZON_PCT))
  if (HORIZON_MODE == "HOURS") return(t_current + HORIZON_HRS)
  stop("Invalid HORIZON_MODE. Use 'PCT' or 'HOURS'.")
}

# ---- Fleet process ----
fleet <- build_process(df)
fleet_fit <- fit_crow_amsaa(fleet)

t1_fleet <- max(fleet$cum_time)
t2_fleet <- future_time(t1_fleet)

fleet_forecast <- expected_failures_between(t1_fleet, t2_fleet, fleet_fit$beta, fleet_fit$eta)

cat(sprintf("Fleet beta=%.4f, eta=%.2f\n", fleet_fit$beta, fleet_fit$eta))
cat(sprintf("Fleet forecast E[failures] from %.1f to %.1f hours: %.3f\n",
            t1_fleet, t2_fleet, fleet_forecast))

# ---- PMP process ----
pmp <- build_process(df %>% filter(group == "PMP"))
pmp_fit <- fit_crow_amsaa(pmp)

t1_pmp <- max(pmp$cum_time)
t2_pmp <- future_time(t1_pmp)

pmp_forecast <- expected_failures_between(t1_pmp, t2_pmp, pmp_fit$beta, pmp_fit$eta)

cat(sprintf("PMP beta=%.4f, eta=%.2f\n", pmp_fit$beta, pmp_fit$eta))
cat(sprintf("PMP forecast E[failures] from %.1f to %.1f hours: %.3f\n",
            t1_pmp, t2_pmp, pmp_forecast))

# ---- Forecast curves (cumulative expected failures) ----
make_forecast_curve <- function(beta, eta, t_max, n = 250) {
  t_seq <- seq(1e-6, t_max, length.out = n)  # avoid 0^beta issues
  N_pred <- (t_seq / eta)^beta
  data.frame(time = t_seq, N = N_pred)
}

fleet_curve <- make_forecast_curve(fleet_fit$beta, fleet_fit$eta, t2_fleet)
pmp_curve   <- make_forecast_curve(pmp_fit$beta,   pmp_fit$eta,   t2_pmp)

# ---- Plot: Fleet ----
p_fleet <- ggplot(fleet_curve, aes(time, N)) +
  geom_line() +
  geom_vline(xintercept = t1_fleet, linetype = "dashed") +
  geom_vline(xintercept = t2_fleet, linetype = "dotted") +
  labs(
    title = "NHPP Forecast — Fleet (Crow–AMSAA)",
    x = "Cumulative operating time (hours)",
    y = "Expected cumulative failures  E[N(t)]"
  )

ggsave("NHPP_Forecast_Fleet.png", p_fleet, dpi = 300, width = 7, height = 4.2)

# ---- Plot: PMP ----
p_pmp <- ggplot(pmp_curve, aes(time, N)) +
  geom_line() +
  geom_vline(xintercept = t1_pmp, linetype = "dashed") +
  geom_vline(xintercept = t2_pmp, linetype = "dotted") +
  labs(
    title = "NHPP Forecast — Pumps (PMP) (Crow–AMSAA)",
    x = "Cumulative operating time (hours)",
    y = "Expected cumulative failures  E[N(t)]"
  )

ggsave("NHPP_Forecast_PMP.png", p_pmp, dpi = 300, width = 7, height = 4.2)

# ---- Save CSV outputs ----
fleet_out <- data.frame(
  scope = "Fleet",
  beta = fleet_fit$beta,
  eta = fleet_fit$eta,
  t_current = t1_fleet,
  t_future  = t2_fleet,
  expected_failures = fleet_forecast,
  horizon_mode = HORIZON_MODE,
  horizon_pct = ifelse(HORIZON_MODE == "PCT", HORIZON_PCT, NA),
  horizon_hrs = ifelse(HORIZON_MODE == "HOURS", HORIZON_HRS, NA)
)

pmp_out <- data.frame(
  scope = "PMP",
  beta = pmp_fit$beta,
  eta = pmp_fit$eta,
  t_current = t1_pmp,
  t_future  = t2_pmp,
  expected_failures = pmp_forecast,
  horizon_mode = HORIZON_MODE,
  horizon_pct = ifelse(HORIZON_MODE == "PCT", HORIZON_PCT, NA),
  horizon_hrs = ifelse(HORIZON_MODE == "HOURS", HORIZON_HRS, NA)
)

write.csv(fleet_out, "NHPP_forecast_fleet.csv", row.names = FALSE)
write.csv(pmp_out,   "NHPP_forecast_pmp.csv",   row.names = FALSE)

summary_out <- bind_rows(fleet_out, pmp_out)
write.csv(summary_out, "NHPP_forecast_summary.csv", row.names = FALSE)

# Optional: save curves
write.csv(fleet_curve, "NHPP_curve_fleet.csv", row.names = FALSE)
write.csv(pmp_curve,   "NHPP_curve_pmp.csv",   row.names = FALSE)
