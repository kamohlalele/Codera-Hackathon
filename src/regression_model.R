# ============================================================
# USDZAR 1-month-ahead forecasting: regression model vs AR(1) benchmark
# Out-of-sample, expanding window, every Friday 2021-01-01 .. 2025-12-31
# ============================================================
library(tidyverse)
library(lubridate)
library(forecast)

# ---- Portable project paths (works for anyone who clones the repo) ----
# The `here` package finds the project root automatically by locating the .git
# folder, so every path below resolves correctly on any machine, no matter where
# the R session was started. No manual setwd() and no path editing needed.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# ---- 0. Load the merged weekly data if not already in the environment ----
if (!exists("fx_weekly_merged")) {
  fx_weekly_merged <- readr::read_csv(
    here("data", "fx_weekly_merged.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)
}

#### Checking the data follows the correct pattern ####
glimpse(fx_weekly_merged)
range(fx_weekly_merged$date)
colSums(is.na(fx_weekly_merged))
sum(duplicated(fx_weekly_merged$date))
all(diff(fx_weekly_merged$date) > 0)

ggplot(fx_weekly_merged, aes(date, usdzar)) +
  geom_line(color = "steelblue") +
  labs(title = "USDZAR — Weekly Series (Sanity Check)", x = NULL, y = "ZAR per USD") +
  theme_minimal()

# ============================================================
# 1. Configuration
# ============================================================
h <- 4                                # forecast horizon: 4 weeks = 1 month
test_start <- as.Date("2021-01-01")
test_end   <- as.Date("2025-12-31")

# ============================================================
# 2. Build the transformed variables (same rules as assumption_test.R)
#      Rates          -> weekly first difference (percentage-point change)
#      Prices/indices -> 100 x weekly log difference (~ weekly % change)
#      Model target   -> 100 x 4-week-ahead log return of USDZAR
# ============================================================
model_data <- fx_weekly_merged %>%
  arrange(date) %>%
  mutate(
    # Dependent variable: the 1-month-ahead log return of USDZAR.
    # `target` is already USDZAR 4 weeks ahead, so this is the t -> t+4 return.
    y_ret = 100 * (log(target) - log(usdzar)),

    # Prices / indices: 100 x log difference
    dl_usdzar = 100 * (log(usdzar)    - log(lag(usdzar))),
    dl_gold   = 100 * (log(gold)      - log(lag(gold))),
    dl_vix    = 100 * (log(vix)       - log(lag(vix))),
    dl_cpi    = 100 * (log(cpi)       - log(lag(cpi))),
    dl_crude  = 100 * (log(crude_oil) - log(lag(crude_oil))),

    # Rates: first difference
    d_prime = prime        - lag(prime),
    d_repo  = repo         - lag(repo),
    d_ofx   = overnight_fx - lag(overnight_fx),
    d_gov   = gov_bond     - lag(gov_bond),
    d_us10  = us_10y       - lag(us_10y),
    d_usinf = us_inflation - lag(us_inflation)
  )

# Transformed predictors the model uses (edit this vector to add/drop features).
# A parsimonious, economically-motivated set: each has genuine weekly variation
# and a clear story, which forecasts better out-of-sample than throwing in all
# 11 variables (the extra ones — carried-forward CPI/crude/US-inflation and the
# rarely-changing policy rates — mostly add noise and overfit).
predictors <- c(
  "dl_usdzar",  # USDZAR momentum (last week's move)
  "dl_gold",    # gold price — SA is a major gold exporter
  "dl_vix",     # global risk sentiment — ZAR is a risk-on currency
  "d_us10",     # US 10-year yield — dollar / rate-differential driver
  "d_gov"       # SA government bond yield — SA risk premium / rate differential
)

# Full set, kept for reference — swap back in to compare:
# predictors <- c("dl_usdzar", "dl_gold", "dl_vix", "dl_cpi", "dl_crude",
#                 "d_prime", "d_repo", "d_ofx", "d_gov", "d_us10", "d_usinf")

model_formula <- as.formula(
  paste("y_ret ~", paste(predictors, collapse = " + "))
)

# ============================================================
# 3. Forecast origins: every Friday in the test window that has a
#    realised 1-month-ahead outcome (target not missing)
# ============================================================
test_dates <- model_data %>%
  filter(date >= test_start, date <= test_end, !is.na(target)) %>%
  pull(date)

length(test_dates)   # sanity check, ~260 Fridays (52 weeks x 5 years)

# ============================================================
# 4. Expanding-window, out-of-sample loop
#    For each origin Friday t:
#      * AR(1)     : fit AR(1) on the USDZAR level using data up to and
#                    including t, then forecast h = 4 steps to t+4.
#      * Our model : fit OLS on transformed predictors using only rows whose
#                    1-month outcome is already realised (date <= t - 4 weeks),
#                    predict the t -> t+4 return from predictors known at t,
#                    then convert that return back to a USDZAR level.
#    No look-ahead: nothing dated after t enters either fit.
# ============================================================
results <- tibble(
  origin_date  = test_dates,
  actual       = NA_real_,
  ar1_forecast = NA_real_,
  reg_forecast = NA_real_
)

for (i in seq_along(test_dates)) {
  origin <- test_dates[i]

  # Actual realised value = USDZAR 4 weeks after the origin.
  results$actual[i] <- model_data$target[model_data$date == origin]

  # ---- AR(1) benchmark on the USDZAR level ----
  # method = "ML": USDZAR behaves like a random walk, so the AR coefficient sits
  # right at ~1. Arima's default CSS warm-up rejects that ("non-stationary AR
  # part from CSS"); maximum-likelihood estimation is stable and still fits the
  # exact same AR(1) model, x[t] = c + phi * x[t-1].
  train_lvl <- model_data %>% filter(date <= origin) %>% pull(usdzar)
  ar1_fit   <- Arima(ts(train_lvl, frequency = 52), order = c(1, 0, 0), method = "ML")
  results$ar1_forecast[i] <- as.numeric(forecast(ar1_fit, h = h)$mean[h])

  # ---- Our regression model on transformed variables ----
  # Keep only rows whose 4-week outcome is known by the origin (no leakage):
  # a row dated s can be trained on only once s + 4 weeks <= origin.
  train_reg <- model_data %>%
    filter(date <= origin - (h * 7)) %>%
    filter(if_all(all_of(c("y_ret", predictors)), ~ !is.na(.)))

  reg_fit <- lm(model_formula, data = train_reg)

  # Predictors observed at the origin (all known at time t).
  x_now    <- model_data %>% filter(date == origin)
  yhat_ret <- as.numeric(predict(reg_fit, newdata = x_now))   # predicted % return

  # Convert the predicted 1-month return back into a USDZAR level.
  results$reg_forecast[i] <- x_now$usdzar * exp(yhat_ret / 100)
}

# Sanity: there should be no missing forecasts.
sum(is.na(results$ar1_forecast))
sum(is.na(results$reg_forecast))
head(results)
tail(results)

# ============================================================
# 5. Out-of-sample accuracy: RMSE (and R-squared) for both models
# ============================================================
# Combined forecast: simple average of the AR(1) and the regression. Averaging
# two imperfect forecasts diversifies their errors and often beats both.
results <- results %>%
  mutate(
    combo_forecast = (ar1_forecast + reg_forecast) / 2,
    ar1_error   = actual - ar1_forecast,
    reg_error   = actual - reg_forecast,
    combo_error = actual - combo_forecast
  )

rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
r2   <- function(err, actual) {
  1 - sum(err^2, na.rm = TRUE) /
      sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
}

rmse_ar1   <- rmse(results$ar1_error)
rmse_reg   <- rmse(results$reg_error)
rmse_combo <- rmse(results$combo_error)
r2_ar1     <- r2(results$ar1_error, results$actual)
r2_reg     <- r2(results$reg_error, results$actual)
r2_combo   <- r2(results$combo_error, results$actual)

cat("Out-of-sample RMSE  — AR(1):", round(rmse_ar1, 4),
    "| Regression:", round(rmse_reg, 4),
    "| Combined:", round(rmse_combo, 4), "\n")
cat("Out-of-sample R2    — AR(1):", round(r2_ar1, 4),
    "| Regression:", round(r2_reg, 4),
    "| Combined:", round(r2_combo, 4), "\n")
cat("RMSE improvement over AR(1) — Regression:",
    round(100 * (rmse_ar1 - rmse_reg) / rmse_ar1, 2), "% |",
    "Combined:",
    round(100 * (rmse_ar1 - rmse_combo) / rmse_ar1, 2), "%\n")

# ============================================================
# 6. Plots
# ============================================================

# 6a. Forecasts vs actual for both models.
results %>%
  select(origin_date, actual, ar1_forecast, reg_forecast) %>%
  pivot_longer(-origin_date, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         actual       = "Actual",
                         ar1_forecast = "AR(1) forecast",
                         reg_forecast = "Regression forecast")) %>%
  ggplot(aes(origin_date, value, colour = series)) +
  geom_line(linewidth = 0.7) +
  labs(title = "1-Month-Ahead USDZAR: Forecasts vs Actual (2021–2025)",
       x = "Forecast origin (Friday)", y = "USDZAR", colour = NULL) +
  theme_minimal()

# 6b. Rolling (cumulative) out-of-sample RMSE for both models.
results %>%
  mutate(
    `AR(1)`      = sqrt(cummean(ar1_error^2)),
    `Regression` = sqrt(cummean(reg_error^2))
  ) %>%
  select(origin_date, `AR(1)`, `Regression`) %>%
  pivot_longer(-origin_date, names_to = "model", values_to = "rolling_rmse") %>%
  ggplot(aes(origin_date, rolling_rmse, colour = model)) +
  geom_line(linewidth = 1) +
  labs(title = "Rolling Out-of-Sample RMSE (2021–2025)",
       x = "Forecast origin (Friday)", y = "Cumulative RMSE", colour = NULL) +
  theme_minimal()

# 6c. Final RMSE comparison bar chart, with the numbers on the chart
#     (directly satisfies the task requirement).
rmse_table <- tibble(
  model = c("AR(1)", "Regression", "Combined"),
  rmse  = c(rmse_ar1, rmse_reg, rmse_combo)
) %>%
  mutate(model = factor(model, levels = c("AR(1)", "Regression", "Combined")))

ggplot(rmse_table, aes(model, rmse, fill = model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(rmse, 4)), vjust = -0.4, size = 5) +
  labs(title = "Out-of-Sample RMSE: Regression & Combined vs AR(1) (2021–2025)",
       x = NULL, y = "RMSE (ZAR per USD)") +
  theme_minimal()

# ============================================================
# 7. Save the forecast results
# ============================================================
dir.create(here("outputs"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(results, here("outputs", "forecast_results_2021_2025.csv"))

# ============================================================
# 8. Summary statistics (for the write-up / slides)
# ============================================================

# ---- 8a. Sample and observation counts (required by the task) ----
first_origin <- min(test_dates)
last_origin  <- max(test_dates)

# Number of complete-case rows the regression trains on at a given origin.
reg_train_n <- function(origin) {
  model_data %>%
    filter(date <= origin - (h * 7)) %>%
    filter(if_all(all_of(c("y_ret", predictors)), ~ !is.na(.))) %>%
    nrow()
}

sample_counts <- tibble(
  metric = c(
    "Total weekly observations",
    "Full sample start",
    "Full sample end",
    "Forecast period start",
    "Forecast period end",
    "Number of Friday forecasts",
    "AR(1) training obs at first forecast",
    "AR(1) training obs at last forecast",
    "Regression training obs at first forecast",
    "Regression training obs at last forecast"
  ),
  value = c(
    as.character(nrow(model_data)),
    as.character(min(model_data$date)),
    as.character(max(model_data$date)),
    as.character(first_origin),
    as.character(last_origin),
    as.character(length(test_dates)),
    as.character(sum(model_data$date <= first_origin)),
    as.character(sum(model_data$date <= last_origin)),
    as.character(reg_train_n(first_origin)),
    as.character(reg_train_n(last_origin))
  )
)

cat("\n--- Sample & observation counts ---\n")
print(sample_counts, n = Inf)

# ---- 8b. Descriptive statistics over the forecast period (2021-2025) ----
level_vars <- c("usdzar", "gold", "vix", "us_10y", "gov_bond")

describe_vars <- function(df, vars) {
  df %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      n      = sum(!is.na(value)),
      mean   = mean(value, na.rm = TRUE),
      sd     = sd(value, na.rm = TRUE),
      min    = min(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      max    = max(value, na.rm = TRUE),
      .groups = "drop"
    )
}

forecast_window <- model_data %>%
  filter(date >= test_start, date <= test_end)

# Levels of the variables the model uses.
summary_levels <- describe_vars(forecast_window, level_vars)
cat("\n--- Descriptive stats: variable levels, 2021-2025 ---\n")
print(summary_levels)

# Transformed predictors that actually enter the regression.
summary_transformed <- describe_vars(forecast_window, predictors)
cat("\n--- Descriptive stats: transformed predictors, 2021-2025 ---\n")
print(summary_transformed)

# ---- 8c. Missing-value counts over the forecast period ----
na_counts <- forecast_window %>%
  summarise(across(all_of(c("usdzar", "target", level_vars, predictors)),
                   ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")
cat("\n--- Missing values over the forecast period ---\n")
print(na_counts, n = Inf)

# ---- 8d. Model performance summary (RMSE, MAE, R-squared) ----
mae <- function(e) mean(abs(e), na.rm = TRUE)

model_performance <- tibble(
  model       = c("AR(1)", "Regression", "Combined"),
  n_forecasts = length(test_dates),
  rmse        = c(rmse_ar1, rmse_reg, rmse_combo),
  mae         = c(mae(results$ar1_error), mae(results$reg_error), mae(results$combo_error)),
  r2          = c(r2_ar1, r2_reg, r2_combo)
)
cat("\n--- Out-of-sample model performance ---\n")
print(model_performance)

# ---- 8e. Save the summary tables for the slides ----
write_csv(sample_counts,       here("outputs", "summary_sample_counts.csv"))
write_csv(summary_levels,      here("outputs", "summary_stats_levels_2021_2025.csv"))
write_csv(summary_transformed, here("outputs", "summary_stats_transformed_2021_2025.csv"))
write_csv(model_performance,   here("outputs", "model_performance.csv"))

# ---- 8f. Full-sample regression coefficients (does the data match the theory?) ----
# Fit the regression once on all complete-case data. These coefficients are
# indicative only — the forecasting loop re-estimates the model every Friday —
# but they show the AVERAGE direction of each driver. The target is the 4-week
# USDZAR return, so a positive coefficient means "predictor up -> USDZAR up
# (rand weakens)"; negative means "predictor up -> USDZAR down (rand strengthens)".
full_fit <- lm(
  model_formula,
  data = model_data %>%
    filter(if_all(all_of(c("y_ret", predictors)), ~ !is.na(.)))
)

coef_table <- summary(full_fit)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  as_tibble() %>%
  rename(estimate = Estimate, std_error = `Std. Error`,
         t_value = `t value`, p_value = `Pr(>|t|)`) %>%
  mutate(
    direction = case_when(
      term == "(Intercept)" ~ "baseline",
      estimate >= 0         ~ "USDZAR up (rand weakens)",
      TRUE                  ~ "USDZAR down (rand strengthens)"
    ),
    significant_5pct = p_value < 0.05
  )

cat("\n--- Full-sample regression coefficients (indicative) ---\n")
print(coef_table)

write_csv(coef_table, here("outputs", "regression_coefficients_full_sample.csv"))

# ============================================================
# 9. Alternative regression: swap GOLD out, TRADE BALANCE in
#    Builds a second weekly dataset that adds the quarterly SA trade balance
#    (carried forward to weekly) and drops gold, then re-runs the exact same
#    out-of-sample loop (1990-start expanding window, 2021-2025 origins) so the
#    RMSE is directly comparable to the original regression.
# ============================================================

# ---- 9a. Read the trade balance file (already date + value columns) ----
# Quarterly series KBP5010L, so it is carried forward to every Friday (LOCF),
# the same upsampling rule used for the other lower-frequency series.
trade_raw <- read_csv(here("data", "BER_Trade_Balance_2026-07-27.csv"),
                      show_col_types = FALSE) %>%
  transmute(date = as.Date(date_col), trade_balance = as.numeric(KBP5010L)) %>%
  filter(!is.na(date), !is.na(trade_balance)) %>%
  arrange(date)

trade_weekly <- tibble(date = fx_weekly_merged$date) %>%
  full_join(trade_raw, by = "date") %>%
  arrange(date) %>%
  tidyr::fill(trade_balance, .direction = "down") %>%   # carry last value forward
  filter(date %in% fx_weekly_merged$date) %>%
  arrange(date)

# ---- 9b. Second merged dataset: add trade balance, remove gold ----
fx_weekly_merged_tb <- fx_weekly_merged %>%
  select(-gold) %>%
  left_join(trade_weekly, by = "date") %>%
  arrange(date)

# ---- 9c. Transformations for this variant (gold replaced by trade balance) ----
model_data_tb <- fx_weekly_merged_tb %>%
  arrange(date) %>%
  mutate(
    y_ret     = 100 * (log(target)  - log(usdzar)),
    dl_usdzar = 100 * (log(usdzar)  - log(lag(usdzar))),
    dl_vix    = 100 * (log(vix)     - log(lag(vix))),
    d_us10    = us_10y   - lag(us_10y),
    d_gov     = gov_bond - lag(gov_bond),
    d_trade   = trade_balance - lag(trade_balance)  # flow can be negative -> first difference
  )

predictors_tb <- c("dl_usdzar", "dl_vix", "d_us10", "d_gov", "d_trade")
formula_tb    <- as.formula(paste("y_ret ~", paste(predictors_tb, collapse = " + ")))

# ---- 9d. Same out-of-sample expanding-window loop, same 2021-2025 origins ----
results_tb <- tibble(
  origin_date     = test_dates,
  actual          = NA_real_,
  reg_tb_forecast = NA_real_
)

for (i in seq_along(test_dates)) {
  origin <- test_dates[i]
  results_tb$actual[i] <- model_data_tb$target[model_data_tb$date == origin]

  train_tb <- model_data_tb %>%
    filter(date <= origin - (h * 7)) %>%
    filter(if_all(all_of(c("y_ret", predictors_tb)), ~ !is.na(.)))

  fit_tb   <- lm(formula_tb, data = train_tb)
  x_now    <- model_data_tb %>% filter(date == origin)
  yhat_ret <- as.numeric(predict(fit_tb, newdata = x_now))
  results_tb$reg_tb_forecast[i] <- x_now$usdzar * exp(yhat_ret / 100)
}

results_tb <- results_tb %>% mutate(reg_tb_error = actual - reg_tb_forecast)

# ---- 9e. Compare RMSE: trade-balance model vs original (gold) regression ----
rmse_reg_tb <- rmse(results_tb$reg_tb_error)
r2_reg_tb   <- r2(results_tb$reg_tb_error, results_tb$actual)

cat("\n--- Alternative regression: trade balance (no gold) vs original ---\n")
cat("RMSE — AR(1) benchmark:            ", round(rmse_ar1, 4), "\n")
cat("RMSE — original regression (gold): ", round(rmse_reg, 4), "\n")
cat("RMSE — trade-balance regression:   ", round(rmse_reg_tb, 4), "\n")
cat("Trade-balance vs gold regression:  ",
    round(100 * (rmse_reg - rmse_reg_tb) / rmse_reg, 2),
    "% (positive = trade balance is better)\n")

# Comparison bar chart with the numbers on it.
rmse_compare <- tibble(
  model = c("AR(1)", "Regression (gold)", "Regression (trade balance)"),
  rmse  = c(rmse_ar1, rmse_reg, rmse_reg_tb)
) %>%
  mutate(model = factor(model, levels = model))

ggplot(rmse_compare, aes(model, rmse, fill = model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(rmse, 4)), vjust = -0.4, size = 5) +
  labs(title = "Out-of-Sample RMSE: Trade-Balance vs Gold Regression vs AR(1) (2021-2025)",
       x = NULL, y = "RMSE (ZAR per USD)") +
  theme_minimal()

write_csv(results_tb, here("outputs", "forecast_results_trade_balance_2021_2025.csv"))

# ============================================================
# 10. Attribution: is the gain from ADDING trade balance, or from
#     REMOVING gold? Fit the 4 core predictors alone (no gold, no trade
#     balance) with the same out-of-sample loop, then decompose the effect.
# ============================================================
predictors_core <- c("dl_usdzar", "dl_vix", "d_us10", "d_gov")
formula_core    <- as.formula(paste("y_ret ~", paste(predictors_core, collapse = " + ")))

results_core <- tibble(
  origin_date       = test_dates,
  actual            = NA_real_,
  reg_core_forecast = NA_real_
)

for (i in seq_along(test_dates)) {
  origin <- test_dates[i]
  results_core$actual[i] <- model_data$target[model_data$date == origin]

  train_core <- model_data %>%
    filter(date <= origin - (h * 7)) %>%
    filter(if_all(all_of(c("y_ret", predictors_core)), ~ !is.na(.)))

  fit_core <- lm(formula_core, data = train_core)
  x_now    <- model_data %>% filter(date == origin)
  yhat_ret <- as.numeric(predict(fit_core, newdata = x_now))
  results_core$reg_core_forecast[i] <- x_now$usdzar * exp(yhat_ret / 100)
}

results_core  <- results_core %>% mutate(reg_core_error = actual - reg_core_forecast)
rmse_reg_core <- rmse(results_core$reg_core_error)
r2_reg_core   <- r2(results_core$reg_core_error, results_core$actual)

cat("\n--- Attribution: what drives the improvement? ---\n")
cat("RMSE — AR(1) benchmark:                 ", round(rmse_ar1, 4), "\n")
cat("RMSE — 5 vars WITH gold:                ", round(rmse_reg, 4), "\n")
cat("RMSE — 4 core vars (no gold, no trade): ", round(rmse_reg_core, 4), "\n")
cat("RMSE — 4 core + trade balance:          ", round(rmse_reg_tb, 4), "\n\n")
cat("Effect of REMOVING gold (5-with-gold -> 4-core):  ",
    round(100 * (rmse_reg - rmse_reg_core) / rmse_reg, 2),
    "% (positive = better)\n")
cat("Effect of ADDING trade balance (4-core -> +trade):",
    round(100 * (rmse_reg_core - rmse_reg_tb) / rmse_reg_core, 2),
    "% (positive = better)\n")

# R-squared on the 1-month MOVEMENT (the honest view): how much of the actual
# 4-week *change* in USDZAR does each model explain? The denominator uses the
# variance of the actual change (not the level), so it is a far more demanding
# test — and can go negative if a model beats the mean-change less than expected.
# All results tables share the same origins, so the actual change is common.
usdzar_at_origin <- model_data$usdzar[match(test_dates, model_data$date)]
actual_change    <- results$actual - usdzar_at_origin
sst_change       <- sum((actual_change - mean(actual_change))^2)
r2_move <- function(err) 1 - sum(err^2) / sst_change

rmse_all <- tibble(
  model = c("AR(1)", "5 vars (with gold)", "4 core (no gold/trade)", "4 core + trade balance"),
  rmse  = c(rmse_ar1, rmse_reg, rmse_reg_core, rmse_reg_tb),
  r2_level = c(r2_ar1, r2_reg, r2_reg_core, r2_reg_tb),
  r2_movement = c(
    r2_move(results$ar1_error),
    r2_move(results$reg_error),
    r2_move(results_core$reg_core_error),
    r2_move(results_tb$reg_tb_error)
  )
)
cat("\n--- RMSE, level R-squared, and movement R-squared, all variants ---\n")
print(rmse_all)
write_csv(rmse_all, here("outputs", "rmse_attribution.csv"))
