library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(zoo)
library(readxl)

fx_raw <- read_excel(
  "data/ECONDATA_MARKET_RATES(1.0.0).xlsx",
  sheet = "MARKET_RATES(1.0.0)",
  skip = 12,
  col_names = c("date", "usdzar")
)

fx <- fx_raw %>%
  mutate(
    date = as.Date(date),
    usdzar = as.numeric(usdzar)
  ) %>%
  filter(!is.na(usdzar)) %>%
  arrange(date)

range(fx$date)


# ============================================================
# 2. Reduce to weekly (Friday) series, filling holiday gaps
# ============================================================
all_fridays <- tibble(
  date = seq(floor_date(min(fx$date), "week", week_start = 1) + 4,
             max(fx$date), by = "week")
)

fx_friday <- all_fridays %>%
  left_join(fx, by = "date") %>%
  arrange(date)

# Fill any missing Fridays (public holidays) with the last available daily value
missing_fridays <- fx_friday %>% filter(is.na(usdzar)) %>% pull(date)

for (d in missing_fridays) {
  prior_val <- fx %>% filter(date < d) %>% slice_tail(n = 1) %>% pull(usdzar)
  fx_friday$usdzar[fx_friday$date == d] <- prior_val
}

sum(is.na(fx_friday$usdzar))  # should be 0 now

# ============================================================
# 3. Set 1-month-ahead target (h = 4 weeks)
# ============================================================
h <- 4

fx_friday <- fx_friday %>%
  mutate(target = lead(usdzar, h))

# ============================================================
# 4. Adding the explanatory variables
# ============================================================

##US DATA####
