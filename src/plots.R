# ============================================================
# Plots of the weekly series used in the USD/ZAR model
# ============================================================

library(tidyverse)
library(scales)

# Portable paths: `here` anchors every path to the repo root.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# Read the merged weekly dataset created by src/r_script_cleaning.R.
data_file <- here("data", "fx_weekly_merged.csv")
plot_dir <- here("outputs", "series_plots")

if (!file.exists(data_file)) {
  stop(
    "Missing ", data_file,
    ". Run src/r_script_cleaning.R from the project root first."
  )
}

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

fx_weekly_merged <- read_csv(data_file, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# These are the series prepared in the model dataset. `target` is included as
# the prediction target, while the remaining columns are model inputs.
series_labels <- c(
  usdzar = "USD/ZAR exchange rate",
  target = "USD/ZAR target, 4 weeks ahead",
  gold = "Gold price",
  prime = "Prime rate",
  repo = "Repo rate",
  overnight_fx = "Overnight FX rate",
  gov_bond = "SA government bond yield",
  vix = "VIX volatility index",
  us_10y = "US 10-year Treasury yield",
  cpi = "SA CPI",
  crude_oil = "Crude oil price",
  us_inflation = "US inflation"
)

plot_data <- fx_weekly_merged %>%
  select(date, all_of(names(series_labels))) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(
    series = factor(
      series,
      levels = names(series_labels),
      labels = series_labels
    )
  )

combined_plot <- ggplot(plot_data, aes(date, value)) +
  geom_line(linewidth = 0.35, colour = "#2f6f73", na.rm = TRUE) +
  facet_wrap(~ series, scales = "free_y", ncol = 2) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Weekly model series",
    subtitle = "Friday-aligned series used in the USD/ZAR forecasting dataset",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(plot_dir, "all_model_series.png"),
  combined_plot,
  width = 12,
  height = 14,
  dpi = 220
)

ggsave(
  file.path(plot_dir, "all_model_series.pdf"),
  combined_plot,
  width = 12,
  height = 14
)

for (series_name in names(series_labels)) {
  individual_plot <- fx_weekly_merged %>%
    ggplot(aes(date, .data[[series_name]])) +
    geom_line(linewidth = 0.5, colour = "#2f6f73", na.rm = TRUE) +
    scale_x_date(
      date_breaks = "5 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(title = series_labels[[series_name]], x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    file.path(plot_dir, paste0(series_name, ".png")),
    individual_plot,
    width = 9,
    height = 4.8,
    dpi = 220
  )
}

message("Series plots saved to: ", plot_dir)
