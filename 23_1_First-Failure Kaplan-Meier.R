# =========================
# First-Failure Kaplan–Meier (KM)
# Use Failure_Events_CLEAR_Enhanced.xlsx
# =========================

# Install once if needed:
# sudo apt update
# sudo apt install r-base -y
install.packages(c("readxl", "dplyr", "lubridate", "survival", "survminer", "openxlsx", "ggplot2"))

library(readxl)
library(dplyr)
library(lubridate)
library(survival)
library(survminer)
library(openxlsx)

# Paste URL Raw if you use GitHub Codespaces
DATA_FILE <- "/workspaces/applied-reliability-engineering/Failure_Events_CLEAR_Enhanced.xlsx"

# Load data
# DATA_FILE <- read.xlsx(file_path)

# Define which event types count as failures
FAILURE_EVENTS <- c("FAILURE", "TRIP")

# ---- Load and clean ----
df <- read_excel(DATA_FILE) %>%
  mutate(
    Asset_ID = as.character(Asset_ID),
    Event_Type = toupper(as.character(Event_Type)),
    Failure_Start = ymd_hms(Failure_Start, quiet = TRUE)
  ) %>%
  filter(!is.na(Asset_ID), !is.na(Failure_Start))

# Observation end (study window end)
OBS_END <- max(df$Failure_Start, na.rm = TRUE)

# ---- Compute T0 per asset (start of observation within dataset) ----
t0 <- df %>%
  group_by(Asset_ID) %>%
  summarise(T0 = min(Failure_Start, na.rm = TRUE), .groups = "drop")

# ---- First failure per asset (if any) ----
first_fail <- df %>%
  filter(Event_Type %in% FAILURE_EVENTS) %>%
  group_by(Asset_ID) %>%
  summarise(First_Failure = min(Failure_Start, na.rm = TRUE), .groups = "drop")

# ---- Build First-Failure survival dataset ----
km_ff <- t0 %>%
  left_join(first_fail, by = "Asset_ID") %>%
  mutate(
    # status: 1 = failed, 0 = censored
    status = ifelse(!is.na(First_Failure), 1L, 0L),

    # time: if failed => First_Failure - T0, else => OBS_END - T0
    time = ifelse(
      status == 1L,
      as.numeric(difftime(First_Failure, T0, units = "hours")),
      as.numeric(difftime(OBS_END, T0, units = "hours"))
    )
  ) %>%
  # remove any non-positive times (can happen if failure at same timestamp as T0)
  filter(is.finite(time), time > 0) %>%
  select(Asset_ID, time, status)

cat("First-failure KM rows:", nrow(km_ff), "\n")
cat("Failures:", sum(km_ff$status == 1), " | Right-censored:", sum(km_ff$status == 0), "\n")

# ---- Fit KM ----
km_fit_ff <- survfit(Surv(time, status) ~ 1, data = km_ff)

# ---- Plot ----
p <- ggsurvplot(
  km_fit_ff,
  data = km_ff,
  conf.int = TRUE,
  conf.int.style = "step",
  censor = TRUE,
  risk.table = TRUE,
  xlab = "Time to first failure (hours)",
  ylab = "Reliability  R(t) = S(t)",
  title = "Kaplan–Meier Reliability Curve (Time to First Failure)",
  ggtheme = theme_minimal()
)

print(p)

# Save 300 dpi
ggsave("KM_23_1_first_failure_all_assets.png", p$plot, dpi = 300, width = 7, height = 4.2)

# Save dataset used (good for repo companion)
write.csv(km_ff, "KM_23_1_first_failure_dataset_all_assets.csv", row.names = FALSE)

# Summary (median survival etc.)
summary(km_fit_ff)

# First-Failure KM by population (Pump vs Motor vs Valve)
km_ff_g <- km_ff %>%
  mutate(group = toupper(sub("-.*$", "", Asset_ID))) %>%
  mutate(group = factor(group))

cat("Groups:\n")
print(table(km_ff_g$group))

km_fit_ff_g <- survfit(Surv(time, status) ~ group, data = km_ff_g)

p_g <- ggsurvplot(
  km_fit_ff_g,
  data = km_ff_g,
  conf.int = TRUE,
  censor = TRUE,
  pval = TRUE,          # log-rank p-value
  risk.table = TRUE,
  xlab = "Time to first failure (hours)",
  ylab = "Reliability  R(t)",
  title = "Kaplan–Meier (First Failure) by Asset Population",
  ggtheme = theme_minimal()
)

print(p_g)

ggsave("KM_23_1_first_failure_by_population.png", p_g$plot, dpi = 300, width = 7.5, height = 4.6)

# Log-rank test (same as pval)
survdiff(Surv(time, status) ~ group, data = km_ff_g)
