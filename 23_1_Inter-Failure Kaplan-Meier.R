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

# ---- Load ----
df <- read_excel(DATA_FILE)

# ---- Choose which event types count as failures ----
FAILURE_EVENTS <- c("FAILURE", "TRIP")

df2 <- df %>%
  mutate(
    Event_Type = toupper(as.character(Event_Type)),
    Failure_Start = ymd_hms(Failure_Start, quiet = TRUE)
  ) %>%
  filter(!is.na(Failure_Start)) %>%
  filter(Event_Type %in% FAILURE_EVENTS)

# ---- Sort and compute inter-failure intervals per asset ----
df2 <- df2 %>%
  arrange(Asset_ID, Failure_Start) %>%
  group_by(Asset_ID) %>%
  mutate(
    TTF_hours = as.numeric(difftime(Failure_Start, lag(Failure_Start), units = "hours"))
  ) %>%
  ungroup()

# Keep only valid positive intervals (drops the first event per asset automatically)
intervals <- df2 %>%
  filter(!is.na(TTF_hours), TTF_hours > 0) %>%
  transmute(
    time = TTF_hours,
    status = 1L
  )

# ---- Right-censor: time from last failure to observation end ----
OBS_END <- max(df2$Failure_Start, na.rm = TRUE)

last_fail <- df2 %>%
  group_by(Asset_ID) %>%
  summarise(last_fail_time = max(Failure_Start, na.rm = TRUE), .groups = "drop")

censored <- last_fail %>%
  mutate(
    time = as.numeric(difftime(OBS_END, last_fail_time, units = "hours")),
    status = 0L
  ) %>%
  filter(time > 0) %>%
  select(time, status)

km_data <- bind_rows(intervals, censored)

cat("KM dataset rows:", nrow(km_data), "\n")
cat("Failures:", sum(km_data$status == 1), " | Right-censored:", sum(km_data$status == 0), "\n")

# ---- Fit KM ----
km_fit <- survfit(Surv(time, status) ~ 1, data = km_data)

# ---- Plot (book-ready) ----
p <- ggsurvplot(
  km_fit,
  data = km_data,
  conf.int = TRUE,
  censor = TRUE,
  risk.table = TRUE,
  xlab = "Inter-failure interval (hours)",
  ylab = "Reliability  R(t) = S(t)",
  title = "Kaplan–Meier Reliability Curve (Inter-failure Intervals)",
  ggtheme = theme_minimal()
)

print(p)

# ---- Save 300 dpi figure ----
ggsave("KM_23_1_inter_failure_all_assets.png", p$plot, dpi = 300, width = 7, height = 4.2)

# ---- Optional: save the KM dataset used (good for companion repo) ----
write.csv(km_data, "KM_23_1_inter_failure_dataset_all_assets.csv", row.names = FALSE)

# ---- Summarize values (median survival etc.) ----
summary(km_fit)

# Inter-Failure KM by population (Pump vs Motor vs Valve)
# =========================
# KM by population (group) using Asset_ID prefix
# =========================

km_data_g <- df2 %>%
  arrange(Asset_ID, Failure_Start) %>%
  mutate(
    Asset_Class = toupper(sub("-.*$", "", as.character(Asset_ID)))
  ) %>%
  group_by(Asset_ID) %>%
  mutate(TTF_hours = as.numeric(difftime(Failure_Start, lag(Failure_Start), units = "hours"))) %>%
  ungroup() %>%
  filter(!is.na(TTF_hours), TTF_hours > 0) %>%
  transmute(
    time = TTF_hours,
    status = 1L,
    group = Asset_Class
  )

# Add censoring rows per asset with group
censored_g <- last_fail %>%
  left_join(df2 %>% distinct(Asset_ID, Asset_Class = toupper(sub("-.*$", "", as.character(Asset_ID)))),
            by = "Asset_ID") %>%
  mutate(
    time = as.numeric(difftime(OBS_END, last_fail_time, units = "hours")),
    status = 0L,
    group = Asset_Class
  ) %>%
  filter(time > 0) %>%
  select(time, status, group)

km_data_g <- bind_rows(km_data_g, censored_g) %>%
  filter(!is.na(group)) %>%
  mutate(group = factor(group))

cat("Groups:\n")
print(table(km_data_g$group))

km_fit_g <- survfit(Surv(time, status) ~ group, data = km_data_g)

p_g <- ggsurvplot(
  km_fit_g,
  data = km_data_g,
  conf.int = TRUE,
  censor = TRUE,
  pval = TRUE,          # log-rank p-value
  risk.table = TRUE,
  xlab = "Inter-failure interval (hours)",
  ylab = "Reliability  R(t)",
  title = "Kaplan–Meier Curves by Asset Population (Prefix from Asset_ID)",
  ggtheme = theme_minimal()
)

print(p_g)

ggsave("KM_23_1_inter_failure_by_population.png", p_g$plot, dpi = 300, width = 7.5, height = 4.6)

# Optional: save dataset
write.csv(km_data_g, "KM_23_1_inter_failure_dataset_by_population.csv", row.names = FALSE)

# Optional: log-rank test (same as pval)
survdiff(Surv(time, status) ~ group, data = km_data_g)
