# --------------------------------------
# 02_analysis.R
# Event study, visualizations, and selection bias analysis
# --------------------------------------

library(data.table)
library(ggplot2)
library(fixest)

# Load panel data
city_panel <- readRDS("data/processed/city_panel_2017_2021.rds")

# ======================================
# 1. EVENT STUDY
# ======================================

# Create treatment groups based on pre-pandemic internet quality (2017)
internet_2017 <- city_panel[year == 2017, .(city_code, internet_2017 = pct_full_remote)]
city_panel <- merge(city_panel, internet_2017, by = "city_code", all.x = TRUE)

# High-internet indicator (top 25%)
high_threshold <- quantile(city_panel[year == 2017, internet_2017], 0.75, na.rm = TRUE)
city_panel[, high_internet := internet_2017 > high_threshold]

# Event study model
event_model <- feols(math_mean ~ i(year, high_internet, ref = 2019) | 
                       city_code + year, 
                     data = city_panel)

# Save results
saveRDS(event_model, "output/results/event_study_model.rds")

# ======================================
# 2. FIGURES
# ======================================

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Figure 1: Math trend
trend_data <- city_panel[, .(
  mean_math = mean(math_mean, na.rm = TRUE),
  se_math = sd(math_mean, na.rm = TRUE) / sqrt(.N)
), by = year]

trend_data[, lower := mean_math - 1.96 * se_math]
trend_data[, upper := mean_math + 1.96 * se_math]

p1 <- ggplot(trend_data, aes(x = year, y = mean_math)) +
  geom_line(linewidth = 1.2, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Average Math Score by Year (2017-2021)",
       x = "Year", y = "Math Score") +
  theme_minimal()
ggsave("output/figures/math_trend.png", p1, width = 8, height = 5, dpi = 300)

# Figure 2: Internet quality trend
internet_trend <- city_panel[, .(
  no_internet = mean(pct_no_internet, na.rm = TRUE),
  mobile = mean(pct_mobile_internet, na.rm = TRUE),
  full_remote = mean(pct_full_remote, na.rm = TRUE)
), by = year]

p2 <- ggplot(internet_trend, aes(x = year)) +
  geom_line(aes(y = no_internet, color = "No Internet"), linewidth = 1.2) +
  geom_line(aes(y = mobile, color = "Mobile Only"), linewidth = 1.2) +
  geom_line(aes(y = full_remote, color = "Full Remote"), linewidth = 1.2) +
  labs(title = "Internet Quality Among Test-Takers (2017-2021)",
       x = "Year", y = "Proportion of Students") +
  theme_minimal()
ggsave("output/figures/internet_quality_trend.png", p2, width = 8, height = 5, dpi = 300)

# Figure 3: School type gap
school_trend <- city_panel[, .(
  private = weighted.mean(math_mean, school_type_private, na.rm = TRUE),
  public = weighted.mean(math_mean, 1 - school_type_private, na.rm = TRUE)
), by = year]

p3 <- ggplot(school_trend, aes(x = year)) +
  geom_line(aes(y = private, color = "Private Schools"), linewidth = 1.2) +
  geom_line(aes(y = public, color = "Public Schools"), linewidth = 1.2) +
  labs(title = "Math Scores: Private vs Public Schools",
       x = "Year", y = "Average Math Score") +
  theme_minimal()
ggsave("output/figures/school_type_trend.png", p3, width = 8, height = 5, dpi = 300)

# Figure 4: Math vs Internet
p4 <- ggplot(city_panel, aes(x = pct_full_remote, y = math_mean, color = as.factor(year))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(title = "Math Score vs Full Remote Learning Capacity",
       x = "% Students with Computer + Internet", y = "Average Math Score",
       color = "Year") +
  theme_minimal()
ggsave("output/figures/math_vs_internet.png", p4, width = 8, height = 5, dpi = 300)

# Figure 5: Event study
coef_values <- coef(event_model)
coef_se <- se(event_model)
year_terms <- grep("year::.*:high_internet", names(coef_values), value = TRUE)

coef_plot <- data.table(
  year = as.numeric(gsub("year::([0-9]+):.*", "\\1", year_terms)),
  estimate = coef_values[year_terms],
  std_error = coef_se[year_terms]
)
coef_plot[, lower := estimate - 1.96 * std_error]
coef_plot[, upper := estimate + 1.96 * std_error]

p5 <- ggplot(coef_plot, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red") +
  geom_point(size = 4, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Event Study: High vs Low Internet Cities",
       x = "Year", y = "Treatment Effect") +
  theme_minimal()
ggsave("output/figures/event_study.png", p5, width = 8, height = 5, dpi = 300)

# Figure 6: Income trend (selection bias)
income_trend <- city_panel[, .(
  income_mean = mean(income_median, na.rm = TRUE)
), by = year]

p6 <- ggplot(income_trend, aes(x = year, y = income_mean)) +
  geom_line(linewidth = 1.2, color = "darkred") +
  geom_point(size = 3, color = "darkred") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  labs(title = "Mean Income Rank Among Test-Takers (2017-2021)",
       x = "Year", y = "Mean Income Rank (higher = richer)") +
  theme_minimal()
ggsave("output/figures/income_trend.png", p6, width = 8, height = 5, dpi = 300)

# ======================================
# 3. CORRELATION TABLE
# ======================================

cor_by_year <- city_panel[, .(
  correlation = cor(pct_full_remote, math_mean, use = "complete.obs")
), by = year]
fwrite(cor_by_year, "output/tables/correlation_by_year.csv")

cat("\n✅ All figures saved to output/figures/\n")
