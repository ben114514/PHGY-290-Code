#paired t test for sys and hr 

# Systolic values
systolic_baseline <- c(113.33, 119.67, 109.67, 112.33, 103,    117, 101.67, 102.67, 113, 110 )   # Baseline averages
systolic_test     <- c(123.67, 124,    114.33, 107.33, 114.33, 121, 103,    100.67, 118, 114.67)   # During test averages

# Heart rate values
hr_baseline <- c(80.67, 77.67, 69.33, 67,    54, 67.67, 59, 56.33, 73.33,69.33)
hr_test     <- c(82,    77.33, 69.67, 73.67, 65, 75.67, 56, 60.33, 79,   74.67)

# ---- Systolic values ----
systolic_baseline <- c(113.33, 119.67, 109.67, 112.33, 103, 
                       117, 101.67, 102.67, 113, 110)

systolic_test <- c(123.67, 124, 114.33, 107.33, 114.33, 
                   121, 103, 100.67, 118, 114.67)

# ---- Paired t-test ----
t_systolic <- t.test(systolic_baseline, systolic_test, paired = TRUE)
print(t_systolic)

# ---- Build dataframe (make Subject a factor) ----
df_systolic <- data.frame(
  Condition = rep(c("Baseline", "Test"), each = length(systolic_baseline)),
  Value     = c(systolic_baseline, systolic_test),
  Subject   = factor(rep(1:length(systolic_baseline), times = 2))  # <-- factor here
)

# ---- Plot ----
library(ggplot2)

theme_set(theme_minimal(base_size = 15, base_family = "Helvetica"))

p_sys <- ggplot(df_systolic, aes(x = Condition, y = Value, group = Subject, color = Subject)) +
  geom_line(alpha = 0.75, linewidth = 1) +
  geom_point(size = 2.8, alpha = 0.95) +
  labs(
    title = "Systolic BP: Baseline vs Test",
    y = "Systolic (mmHg)",
    x = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "none"   # removes participant legend
  )

p_sys

# ---- Paired t-test ----
t_hr <- t.test(hr_baseline, hr_test, paired = TRUE)
print(t_hr)

# ---- Build dataframe (make Subject a factor) ----
df_hr <- data.frame(
  Condition = rep(c("Baseline", "Test"), each = length(hr_baseline)),
  Value     = c(hr_baseline, hr_test),
  Subject   = factor(rep(1:length(hr_baseline), times = 2))
)

# ---- Plot ----
library(ggplot2)

theme_set(theme_minimal(base_size = 15, base_family = "Helvetica"))

p_hr <- ggplot(df_hr, aes(x = Condition, y = Value, group = Subject, color = Subject)) +
  geom_line(alpha = 0.75, linewidth = 1) +
  geom_point(size = 2.8, alpha = 0.95) +
  labs(
    title = "Heart Rate: Baseline vs Test",
    y = "Heart Rate (bpm)",
    x = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "none"   # remove participant legend
  )

p_hr

#STATISTICAL TEST
shapiro.test(diff_systolic)
shapiro.test(diff_hr)

t.test(systolic_baseline, systolic_test, paired=TRUE)
t.test(hr_baseline, hr_test, paired = TRUE)
wilcox.test(hr_baseline, hr_test, paired = TRUE)

#paired t test for percent increase in sys and hr
# ---- Percent change calculations ----
pct_sys <- ((systolic_test - systolic_baseline) / systolic_baseline) * 100
pct_hr  <- ((hr_test - hr_baseline) / hr_baseline) * 100

# ---- Build dataframes ----
df_pct_sys <- data.frame(
  Subject = factor(1:length(pct_sys)),
  PercentChange = pct_sys
)

df_pct_hr <- data.frame(
  Subject = factor(1:length(pct_hr)),
  PercentChange = pct_hr
)

# ---- Plot for systolic percent change ----
library(ggplot2)

p_pct_sys <- ggplot(df_pct_sys, aes(x = Subject, y = PercentChange, color = Subject)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, fill = "white", color = "black") +
  labs(
    title = "Percent Change in Systolic BP",
    y = "Percent Change (%)",
    x = "Participant"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

# ---- Plot for HR percent change ----
p_pct_hr <- ggplot(df_pct_hr, aes(x = Subject, y = PercentChange, color = Subject)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, fill = "white", color = "black") +
  labs(
    title = "Percent Change in Heart Rate",
    y = "Percent Change (%)",
    x = "Participant"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

# ---- Show plots ----
p_pct_sys
p_pct_hr

#paired t test for rate pressure product
# ---- Compute RPP ----
rpp_baseline <- systolic_baseline * hr_baseline
rpp_test     <- systolic_test * hr_test

# ---- Paired t-test ----
t_rpp <- t.test(rpp_baseline, rpp_test, paired = TRUE)
print(t_rpp)

# ---- Optional: Build dataframe for plotting ----
df_rpp <- data.frame(
  Condition = rep(c("Baseline", "Test"), each = length(rpp_baseline)),
  Value     = c(rpp_baseline, rpp_test),
  Subject   = factor(rep(1:length(rpp_baseline), times = 2))
)

library(ggplot2)
theme_set(theme_minimal(base_size = 15))

p_rpp <- ggplot(df_rpp, aes(x = Condition, y = Value, group = Subject, color = Subject)) +
  geom_line(alpha = 0.75, linewidth = 1) +
  geom_point(size = 2.8, alpha = 0.95) +
  labs(
    title = "Rate Pressure Product (RPP): Baseline vs Test",
    y = "RPP (Systolic × HR)",
    x = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "none"
  )

p_rpp

install.packages("patchwork")
library(ggplot2)
library(patchwork)

# ---- 0) Align lengths & drop NA/Inf pairwise across all four vectors ----
n <- min(length(systolic_baseline), length(systolic_test),
         length(hr_baseline),       length(hr_test))

sb <- systolic_baseline[seq_len(n)]
st <- systolic_test[seq_len(n)]
hb <- hr_baseline[seq_len(n)]
ht <- hr_test[seq_len(n)]

keep <- is.finite(sb) & is.finite(st) & is.finite(hb) & is.finite(ht)
sb <- sb[keep]; st <- st[keep]; hb <- hb[keep]; ht <- ht[keep]

# ---- 1) Derived quantities ----
d_sys <- st - sb
d_hr  <- ht - hb

pct_sys <- 100 * d_sys / sb
pct_hr  <- 100 * d_hr  / hb

# ---- 2) Helper to choose a sensible binwidth ----
fd_binwidth <- function(x) {
  bw <- 2 * IQR(x) / (length(x)^(1/3))
  if (!is.finite(bw) || bw <= 0) {
    # fallback: Sturges/FD hybrid
    diff(range(x)) / max(8, ceiling(log2(length(x)) + 1))
  } else bw
}

# ---- 3) Individual plots ----
p_hist_sys <- ggplot(data.frame(diff = d_sys), aes(diff)) +
  geom_histogram(binwidth = fd_binwidth(d_sys), color = "white",
                 fill = "steelblue", alpha = 0.9, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Systolic: Paired Differences",
       subtitle = "Test − Baseline (mmHg)",
       x = "Δ Systolic (mmHg)", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

p_hist_hr <- ggplot(data.frame(diff = d_hr), aes(diff)) +
  geom_histogram(binwidth = fd_binwidth(d_hr), color = "white",
                 fill = "tomato", alpha = 0.9, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Heart Rate: Paired Differences",
       subtitle = "Test − Baseline (bpm)",
       x = "Δ Heart Rate (bpm)", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

p_hist_pct_sys <- ggplot(data.frame(pct = pct_sys), aes(pct)) +
  geom_histogram(binwidth = fd_binwidth(pct_sys), color = "white",
                 fill = "steelblue", alpha = 0.9, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Systolic: Percent Change",
       subtitle = "(Test − Baseline) / Baseline × 100",
       x = "Percent change (%)", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

p_hist_pct_hr <- ggplot(data.frame(pct = pct_hr), aes(pct)) +
  geom_histogram(binwidth = fd_binwidth(pct_hr), color = "white",
                 fill = "tomato", alpha = 0.9, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Heart Rate: Percent Change",
       subtitle = "(Test − Baseline) / Baseline × 100",
       x = "Percent change (%)", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# ---- 4) Compose 2x2 panel ----
final_panel <- (p_hist_sys | p_hist_hr) / (p_hist_pct_sys | p_hist_pct_hr) +
  plot_annotation(
    title = "Distributions of Change: Systolic BP and Heart Rate",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))
  )

final_panel
