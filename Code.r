## --- Data ---------------------------------------------------------------
systolic_baseline <- c(113.33, 119.67, 109.67, 112.33, 103, 117, 101.67, 102.67, 113, 110)
systolic_test     <- c(123.67, 124.00, 114.33, 107.33, 114.33, 121, 103.00, 100.67, 118, 114.67)

hr_baseline <- c(80.67, 77.67, 69.33, 67.00, 54.00, 67.67, 59.00, 56.33, 73.33, 69.33)
hr_test     <- c(82.00, 77.33, 69.67, 73.67, 65.00, 75.67, 56.00, 60.33, 79.00, 74.67)

## --- Paired differences (for assumptions) -------------------------------
d_sys <- systolic_test - systolic_baseline
d_hr  <- hr_test - hr_baseline

## --- Percent changes (if you want to analyze these too) -----------------
pct_sys <- 100 * d_sys / systolic_baseline
pct_hr  <- 100 * d_hr  / hr_baseline

## --- Assumption checks: normality of *paired differences* ---------------
shapiro.test(d_sys)
shapiro.test(d_hr)

## (Optional) If you will test percent change with a t-test, check these too:
shapiro.test(pct_sys)
shapiro.test(pct_hr)

## --- Paired tests -------------------------------------------------------
# Raw values
t_systolic <- t.test(systolic_baseline, systolic_test, paired = TRUE)
t_hr       <- t.test(hr_baseline, hr_test, paired = TRUE)

# Nonparametric fallback if Shapiro is clearly non-normal:
w_sys <- wilcox.test(systolic_baseline, systolic_test, paired = TRUE, exact = FALSE)
w_hr  <- wilcox.test(hr_baseline, hr_test, paired = TRUE, exact = FALSE)

# Percent-change tests (optional; paired on pct vs zero)
t_pct_sys <- t.test(pct_sys, mu = 0)   # one-sample t against 0% mean change
t_pct_hr  <- t.test(pct_hr,  mu = 0)
w_pct_sys <- wilcox.test(pct_sys, mu = 0, exact = FALSE)
w_pct_hr  <- wilcox.test(pct_hr,  mu = 0, exact = FALSE)

print(t_systolic); print(w_sys)
print(t_hr);       print(w_hr)
print(t_pct_sys);  print(w_pct_sys)
print(t_pct_hr);   print(w_pct_hr)

## --- RPP analysis (remember: check normality of *differences*) ----------
rpp_baseline <- systolic_baseline * hr_baseline
rpp_test     <- systolic_test     * hr_test
d_rpp        <- rpp_test - rpp_baseline

shapiro.test(d_rpp)
t_rpp <- t.test(rpp_baseline, rpp_test, paired = TRUE)
w_rpp <- wilcox.test(rpp_baseline, rpp_test, paired = TRUE, exact = FALSE)
print(t_rpp); print(w_rpp)

## --- Percent change in RPP ----------------------------------------------
pct_rpp <- 100 * (rpp_test - rpp_baseline) / rpp_baseline

# Data frame
df_pct_rpp <- data.frame(
  Subject = factor(seq_along(pct_rpp)),
  PercentChange = pct_rpp
)

# Normality check
shapiro.test(pct_rpp)

# One-sample t-test vs 0% change
t_pct_rpp <- t.test(pct_rpp, mu = 0)
w_pct_rpp <- wilcox.test(pct_rpp, mu = 0, exact = FALSE)

print(t_pct_rpp)
print(w_pct_rpp)

# Plot
p_pct_rpp <- ggplot(df_pct_rpp, aes(x = Subject, y = PercentChange, color = Subject)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4,
               fill = "white", color = "black") +
  labs(
    title = "Percent Change in Rate Pressure Product (RPP)",
    y = "Percent Change (%)",
    x = "Participant"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

p_pct_rpp


## --- Long data frames for plots -----------------------------------------
library(ggplot2)
theme_set(theme_minimal(base_size = 15))

df_systolic <- data.frame(
  Condition = factor(rep(c("Baseline","Test"), each = length(systolic_baseline)), 
                     levels = c("Baseline","Test")),
  Value     = c(systolic_baseline, systolic_test),
  Subject   = factor(rep(seq_along(systolic_baseline), times = 2))
)

df_hr <- data.frame(
  Condition = factor(rep(c("Baseline","Test"), each = length(hr_baseline)), 
                     levels = c("Baseline","Test")),
  Value     = c(hr_baseline, hr_test),
  Subject   = factor(rep(seq_along(hr_baseline), times = 2))
)

p_sys <- ggplot(df_systolic, aes(Condition, Value, group = Subject, color = Subject)) +
  geom_line(alpha = 0.75, linewidth = 1) +
  geom_point(size = 2.8, alpha = 0.95) +
  labs(title = "Systolic BP: Baseline vs Test", y = "Systolic (mmHg)", x = NULL) +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position = "none")

p_hr <- ggplot(df_hr, aes(Condition, Value, group = Subject, color = Subject)) +
  geom_line(alpha = 0.75, linewidth = 1) +
  geom_point(size = 2.8, alpha = 0.95) +
  labs(title = "Heart Rate: Baseline vs Test", y = "Heart Rate (bpm)", x = NULL) +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position = "none")

p_sys; p_hr

## --- Percent-change dot plots -------------------------------------------
df_pct_sys <- data.frame(Subject = factor(seq_along(pct_sys)), PercentChange = pct_sys)
df_pct_hr  <- data.frame(Subject = factor(seq_along(pct_hr)),  PercentChange = pct_hr)

p_pct_sys <- ggplot(df_pct_sys, aes(Subject, PercentChange, color = Subject)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, fill = "white", color = "black") +
  labs(title = "Percent Change in Systolic BP", y = "Percent Change (%)", x = "Participant") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title = element_text(face = "bold"))

p_pct_hr <- ggplot(df_pct_hr, aes(Subject, PercentChange, color = Subject)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, fill = "white", color = "black") +
  labs(title = "Percent Change in Heart Rate", y = "Percent Change (%)", x = "Participant") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title = element_text(face = "bold"))

p_pct_sys; p_pct_hr

## --- RPP plot ------------------------------------------------------------
df_rpp <- data.frame(
  Condition = factor(rep(c("Baseline","Test"), each = length(rpp_baseline)), 
                     levels = c("Baseline","Test")),
  Value     = c(rpp_baseline, rpp_test),
  Subject   = factor(rep(seq_along(rpp_baseline), times = 2))
)

p_rpp <- ggplot(df_rpp, aes(Condition, Value, group = Subject, color = Subject)) +
  geom_line(alpha = 0.75, linewidth = 1) +
  geom_point(size = 2.8, alpha = 0.95) +
  labs(title = "Rate Pressure Product (RPP): Baseline vs Test", y = "RPP (Systolic × HR)", x = NULL) +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position = "none")

p_rpp
## --- Percent change in RPP ----------------------------------------------
pct_rpp <- 100 * (rpp_test - rpp_baseline) / rpp_baseline

# Data frame
df_pct_rpp <- data.frame(
  Subject = factor(seq_along(pct_rpp)),
  PercentChange = pct_rpp
)

# Normality check
shapiro.test(pct_rpp)

# One-sample t-test vs 0% change
t_pct_rpp <- t.test(pct_rpp, mu = 0)
w_pct_rpp <- wilcox.test(pct_rpp, mu = 0, exact = FALSE)

print(t_pct_rpp)
print(w_pct_rpp)

# Plot
p_pct_rpp <- ggplot(df_pct_rpp, aes(x = Subject, y = PercentChange, color = Subject)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4,
               fill = "white", color = "black") +
  labs(
    title = "Percent Change in Rate Pressure Product (RPP)",
    y = "Percent Change (%)",
    x = "Participant"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

p_pct_rpp
