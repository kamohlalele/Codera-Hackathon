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
  mutate(target = lead(usdzar, h)) %>%
  filter(date >= as.Date("1990-01-01"))   # keep 1990 onwards only

# ============================================================
# 4. Adding the explanatory variables
#    Every series is aligned to the Friday grid (fx_friday$date) with
#    last-observation-carried-forward (na.locf): each Friday takes the
#    most recent value available on or before it. For daily series this
#    grabs the Friday value (or the last business day before a holiday);
#    for monthly / annual series it carries the latest release forward,
#    upsampling them to weekly automatically.
# ============================================================

fridays <- fx_friday$date

# Helper: given a data frame with columns `date` and `value`, return the
# LOCF-aligned value vector matching fx_friday$date (one value per Friday).
align_to_fridays <- function(df) {
  df <- df %>%
    transmute(date = as.Date(date), value = as.numeric(value)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)

  tibble(date = fridays) %>%
    full_join(df, by = "date") %>%
    arrange(date) %>%
    mutate(value = zoo::na.locf(value, na.rm = FALSE)) %>%
    filter(date %in% fridays) %>%
    arrange(date) %>%
    pull(value)
}

# ---- 4a. Gold / Prime / Repo / Overnight FX (daily; one file, 4 blocks) ----
gpro_raw <- read_excel(
  "data/ECONDATA_GOLD_PRIME_REPO_OVERNIGHTFX(1.0.0).xlsx",
  sheet = "MARKET_RATES(1.0.0)",
  skip = 12,
  col_names = c("gold_date", "gold", "prime_date", "prime",
                "repo_date", "repo", "ofx_date", "ofx")
)

gold  <- align_to_fridays(gpro_raw %>% select(date = gold_date,  value = gold))
prime <- align_to_fridays(gpro_raw %>% select(date = prime_date, value = prime))
repo  <- align_to_fridays(gpro_raw %>% select(date = repo_date,  value = repo))
ofx   <- align_to_fridays(gpro_raw %>% select(date = ofx_date,   value = ofx))

# ---- 4b. SA CPI (monthly; series starts 2002, earlier Fridays stay NA) ----
cpi_raw <- read_excel("data/CPI (2002).xlsx", sheet = "Monthly", skip = 9,
                      col_names = c("date", "cpi"))
cpi <- align_to_fridays(cpi_raw %>% select(date = 1, value = 2))

# ---- 4c. Crude oil (annual) ----
oil_raw <- read_csv("data/BER_Crude_Oil_2026-07-25.csv", show_col_types = FALSE)
crude_oil <- align_to_fridays(oil_raw %>% select(date = 1, value = 2))

# ---- 4d. SA government bond yield (daily) ----
bond_raw <- read_csv("data/BER_GOV_BONDS_2026-07-25.csv", show_col_types = FALSE)
gov_bond <- align_to_fridays(bond_raw %>% select(date = 1, value = 2))

# ---- 4e. VIX (daily; date is d/m/Y) ----
vix_raw <- read_csv("data/VIX.csv", show_col_types = FALSE,
                    col_types = cols(observation_date = col_character(),
                                     VIXCLS = col_character()))
vix <- align_to_fridays(
  vix_raw %>% transmute(date = dmy(observation_date), value = as.numeric(VIXCLS))
)

# ---- 4f. US inflation (annual) ----
usinf_raw <- read_excel("data/FPCPITOTLZGUSA.xlsx", sheet = "Annual")
us_inflation <- align_to_fridays(usinf_raw %>% select(date = 1, value = 2))

# ---- 4g. US 10-year Treasury yield (daily) ----
ust_raw <- read_excel("data/US treasury 10 year.xlsx", sheet = "Daily")
us_10y <- align_to_fridays(ust_raw %>% select(date = 1, value = 2))

# ---- 4h. Trade balance (monthly) ----
# NOTE: data/Trade Balance.xlsx currently exports metadata only (no
# observations, "Selection: None"), so it is skipped. Re-export the file
# with data and uncomment the block below to include it.
# tb_raw <- read_excel("data/Trade Balance.xlsx", sheet = "Monthly", skip = 9,
#                      col_names = c("date", "trade_balance"))
# trade_balance <- align_to_fridays(tb_raw %>% select(date = 1, value = 2))

# ============================================================
# 5. Assemble the final weekly (Friday) dataset
# ============================================================
fx_weekly <- fx_friday %>%
  mutate(
    gold         = gold,
    prime        = prime,
    repo         = repo,
    overnight_fx = ofx,
    gov_bond     = gov_bond,
    vix          = vix,
    us_10y       = us_10y,
    cpi          = cpi,
    crude_oil    = crude_oil,
    us_inflation = us_inflation
    # trade_balance = trade_balance
  ) %>%
  relocate(target, .after = last_col())

glimpse(fx_weekly)

# Write the merged weekly dataset
write_csv(fx_weekly, "data/fx_weekly_merged.csv")

# Also expose it in the global environment under the file's name
fx_weekly_merged <- fx_weekly
