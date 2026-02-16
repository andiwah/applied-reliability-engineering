# =========================
# Hazard Function Estimation
# from KM_23_1_inter_failure_dataset_all_assets.csv
# =========================

install.packages(c("survival", "muhaz", "readr"))

library(survival)
library(muhaz)
library(readr)

# ---- Load dataset ----
df <- read_csv("KM_23_1_inter_failure_dataset_all_assets.csv")

# Expect columns:
# time  = interval
# status = 1 failure, 0 censored

# ---- Fit survival object ----
surv_obj <- Surv(df$time, df$status)

# ---- Kernel-smoothed hazard ----
hz <- muhaz(
  times = df$time,
  delta = df$status,
  bw.method = "global"
)

# ---- Plot hazard ----
plot(
  hz,
  xlab = "Inter-failure interval (hours)",
  ylab = "Hazard rate h(t)",
  main = "Smoothed Hazard Function (Inter-failure Intervals)",
  lwd = 2
)
grid()

# ---- Save 300 dpi ----
png("Hazard_23_2_inter_failure.png", width = 7, height = 4.2, units = "in", res = 300)
plot(hz, xlab = "Inter-failure interval (hours)", ylab = "Hazard rate h(t)", lwd = 2)
grid()
dev.off()

# Nelson–Aalen Cumulative Hazard 
library(survival)

fit <- survfit(Surv(time, status) ~ 1, data = df, type = "fh")

plot(
  fit$time,
  -log(fit$surv),
  type = "s",
  xlab = "Inter-failure interval (hours)",
  ylab = "Cumulative hazard H(t)",
  main = "Nelson–Aalen Cumulative Hazard"
)
grid()
