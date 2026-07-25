from pathlib import Path

import pandas as pd
import numpy as np

DATA_FILE = Path(__file__).resolve().parent.parent / "data" / "ECONDATA_MARKET_RATES(1.0.0).xlsx"

fx_raw = pd.read_excel(
    DATA_FILE,
    sheet_name="MARKET_RATES(1.0.0)",
    skiprows=12,
    header=None,
    names=["date", "usdzar"],
)

fx = (
    fx_raw.assign(
        date=pd.to_datetime(fx_raw["date"]),
        usdzar=pd.to_numeric(fx_raw["usdzar"], errors="coerce"),
    )
    .dropna(subset=["usdzar"])
    .sort_values("date")
    .reset_index(drop=True)
)

print(fx["date"].min(), fx["date"].max())


# ============================================================
# 2. Reduce to weekly (Friday) series, filling holiday gaps
# ============================================================
first_friday = fx["date"].min().to_period("W-SUN").start_time + pd.Timedelta(days=4)
all_fridays = pd.DataFrame(
    {"date": pd.date_range(start=first_friday, end=fx["date"].max(), freq="W-FRI")}
)

fx_friday = all_fridays.merge(fx, on="date", how="left").sort_values("date").reset_index(drop=True)

# Fill any missing Fridays (public holidays) with the last available daily value
missing_fridays = fx_friday.loc[fx_friday["usdzar"].isna(), "date"]

for d in missing_fridays:
    prior_val = fx.loc[fx["date"] < d, "usdzar"].iloc[-1]
    fx_friday.loc[fx_friday["date"] == d, "usdzar"] = prior_val

print(fx_friday["usdzar"].isna().sum())  # should be 0 now

# ============================================================
# 3. Set 1-month-ahead target (h = 4 weeks)
# ============================================================
h = 4

fx_friday["target"] = fx_friday["usdzar"].shift(-h)
