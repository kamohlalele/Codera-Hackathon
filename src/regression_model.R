library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
####Checkinbg the data follows correct pattern###


glimpse(fx_weekly_merged)
range(fx_weekly_merged$date)
colSums(is.na(fx_weekly_merged))
sum(duplicated(fx_weekly_merged$date))
all(diff(fx_weekly_merged$date) > 0)

ggplot(fx_weekly_merged, aes(date, usdzar)) +
  geom_line(color = "steelblue") +
  labs(title = "USDZAR — Weekly Series (Sanity Check)", x = NULL, y = "ZAR per USD") +
  theme_minimal()

###Defing the test period for the regression model####
  test_start <- as.Date("2021-01-01")
test_end   <- as.Date("2025-12-31")

test_dates <- fx_weekly_merged %>%
  filter(date >= test_start, date <= test_end, !is.na(target)) %>%
  pull(date)

length(test_dates)  # sanity check, roughly 260 (52 weeks x 5 years)

##Creating a results table to store the actual and forecasted values for the AR(1) model###
ar1_results <- tibble(
  origin_date = test_dates,for (i in seq_along(test_dates)) {
  origin <- test_dates[i]
  
####Expanding Window Loop###
  # Training data = everything strictly BEFORE this Friday (no lookahead / no cheating)
  train <- fx_weekly_merged %>% filter(date < origin) %>% pull(usdzar)
  ts_train <- ts(train, frequency = 52)
  
  fit <- Arima(ts_train, order = c(1, 0, 0))
  fc  <- forecast(fit, h = h)$mean[h]   # the h-th step = 1 month ahead
  
  ar1_results$actual[i]       <- fx_weekly_merged$target[fx_weekly_merged$date == origin]
  ar1_results$ar1_forecast[i] <- fc
}
  actual = NA_real_,
  ar1_forecast = NA_real_
)

###Checking the Loop###
sum(is.na(ar1_results$ar1_forecast))  # should be 0
head(ar1_results)
tail(ar1_results)

###Compute out of sample RMSE and R-squared for the AR(1) model###
ar1_results <- ar1_results %>%
  mutate(ar1_error = actual - ar1_forecast)

rmse_ar1_oos <- sqrt(mean(ar1_results$ar1_error^2, na.rm = TRUE))

ss_res <- sum(ar1_results$ar1_error^2, na.rm = TRUE)
ss_tot <- sum((ar1_results$actual - mean(ar1_results$actual, na.rm = TRUE))^2, na.rm = TRUE)
r2_ar1_oos <- 1 - ss_res / ss_tot

cat("Out-of-sample AR(1) RMSE:", round(rmse_ar1_oos, 4), "\n")
cat("Out-of-sample AR(1) R²:  ", round(r2_ar1_oos, 4), "\n")



Plot RMSE and R-squared for the AR(1) model###
ar1_results %>%
  pivot_longer(c(actual, ar1_forecast), names_to = "series", values_to = "value") %>%
  ggplot(aes(origin_date, value, color = series)) +
  geom_line(size = 0.8) +
  labs(title = "AR(1) Out-of-Sample Forecasts vs Actual USDZAR (2021–2025)",
       x = "Forecast Origin (Friday)", y = "USDZAR", color = NULL) +
  theme_minimal()


  ###RMSE Chart##
  ar1_results <- ar1_results %>%
  mutate(rolling_rmse = sqrt(cummean(ar1_error^2)))

ggplot(ar1_results, aes(origin_date, rolling_rmse)) +
  geom_line(color = "firebrick", size = 1) +
  annotate("label", x = max(ar1_results$origin_date), y = rmse_ar1_oos,
           label = paste0("Final RMSE: ", round(rmse_ar1_oos, 3)), hjust = 1) +
  labs(title = "AR(1) Rolling Out-of-Sample RMSE (2021–2025)",
       x = "Forecast Origin (Friday)", y = "Cumulative RMSE") +
  theme_minimal()