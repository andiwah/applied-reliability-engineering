# =========================================
# Crow–AMSAA Reliability Growth Analysis
# Fleet vs PMP Population
# =========================================

install.packages(c("readr","dplyr","ggplot2"))

library(readr)
library(dplyr)
library(ggplot2)

# ---- Load inter-failure dataset ----
df <- read_csv("KM_23_1_inter_failure_dataset_by_population.csv")

# Expected columns:
# time   = inter-failure interval
# status = 1 (failure)
# group  = population label

# ---- Fleet cumulative time ----
fleet <- df %>%
  filter(status == 1) %>%
  arrange(time) %>%
  mutate(
    cum_time = cumsum(time),
    N = row_number()
  )

# ---- PMP subset ----
pmp <- df %>%
  filter(group == "PMP", status == 1) %>%
  arrange(time) %>%
  mutate(
    cum_time = cumsum(time),
    N = row_number()
  )

# ---- Crow–AMSAA log-log regression ----
fit_fleet <- lm(log(N) ~ log(cum_time), data = fleet)
fit_pmp   <- lm(log(N) ~ log(cum_time), data = pmp)

beta_fleet <- coef(fit_fleet)[2]
beta_pmp   <- coef(fit_pmp)[2]

cat("Fleet beta:", beta_fleet, "\n")
cat("PMP beta:", beta_pmp, "\n")

# ---- Plot fleet ----
p1 <- ggplot(fleet, aes(cum_time, N)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Crow–AMSAA Plot (Fleet)",
    x = "Cumulative time (hours)",
    y = "Cumulative failures"
  )

# ---- Plot PMP ----
p2 <- ggplot(pmp, aes(cum_time, N)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Crow–AMSAA Plot (PMP)",
    x = "Cumulative time (hours)",
    y = "Cumulative failures"
  )

# ---- Save 300 dpi ----
ggsave("CrowAMSAA_Fleet.png", p1, dpi = 300, width = 7, height = 4.2)
ggsave("CrowAMSAA_PMP.png", p2, dpi = 300, width = 7, height = 4.2)
