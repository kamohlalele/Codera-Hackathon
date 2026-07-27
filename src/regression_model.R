# ============================================================
# 1. Packages and project paths
# ============================================================

# tidyverse supplies data manipulation, CSV import/export, and map functions.
library(tidyverse)

# tseries supplies the Augmented Dickey-Fuller and KPSS stationarity tests.
library(tseries)

# Read the weekly Friday dataset created by r_script_cleaning.R.
data_file <- "data/fx_weekly_merged.csv"

# Save all diagnostic results in a dedicated output folder.
output_dir <- "outputs/assumption_tests"

# Save transformed-variable charts in their own subfolder.
transformed_plot_dir <- file.path(output_dir, "transformed_plots")

# Create the output folder if it does not already exist.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Create the transformed-plot folder if it does not already exist.
dir.create(transformed_plot_dir, recursive = TRUE, showWarnings = FALSE)

# Stop with an informative error if the merged dataset is unavailable.
if (!file.exists(data_file)) {
  stop(
    "Missing ", data_file,
    ". Run src/r_script_cleaning.R from the project root first."
  )
}

# ============================================================
# 2. Read and select the analysis sample
# ============================================================

# Import the merged data and recognise common missing-value markers.
weekly_data <- read_csv(
  data_file,
  na = c("", "NA", "."),
  show_col_types = FALSE
) %>%
  # Convert date from text to an R Date and put observations in time order.
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  # Use the requested modelling period.
  filter(
    date >= as.Date("2021-01-01"),
    date <= as.Date("2025-12-31")
  )

# Stop if filtering left no observations.
if (nrow(weekly_data) == 0) {
  stop("No observations were found between 2021-01-01 and 2025-12-31.")
}

# Find trade-balance columns even if their exact spelling changes.
trade_balance_columns <- names(weekly_data)[
  str_detect(
    str_to_lower(names(weekly_data)),
    "trade.*balance|balance.*trade"
  )
]

# Exclude date because it is not numeric data.
# Exclude target because it is a four-week lead of USD/ZAR, not a predictor.
# Exclude trade balances in line with the current project scope.
excluded_columns <- c("date", "target", trade_balance_columns)

# Keep every remaining numeric variable for the diagnostic loop.
variables_to_test <- weekly_data %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(excluded_columns)

# Stop if no usable variables were found.
if (length(variables_to_test) == 0) {
  stop("The merged dataset contains no numeric variables to test.")
}

# ============================================================
# 3. Safe helper functions for statistical tests
# ============================================================

# Extract a p-value without stopping the full loop when one test cannot run.
safe_p_value <- function(test_expression) {
  tryCatch(
    suppressWarnings(test_expression$p.value),
    error = function(error) NA_real_
  )
}

# Run an ADF test.
# H0: the series has a unit root and is non-stationary.
# A p-value below 0.05 supports stationarity.
adf_p_value <- function(values) {
  lag_order <- trunc((length(values) - 1)^(1 / 3))

  safe_p_value(
    tseries::adf.test(
      values,
      alternative = "stationary",
      k = lag_order
    )
  )
}

# Run a KPSS level-stationarity test.
# H0: the series is stationary.
# A p-value above 0.05 means stationarity is not rejected.
kpss_p_value <- function(values) {
  safe_p_value(
    tseries::kpss.test(
      values,
      null = "Level",
      lshort = TRUE
    )
  )
}

# Implement a Ramsey RESET test using only base R.
# H0: the linear AR(1) functional form is adequate.
# A p-value above 0.05 means there is no evidence against linearity.
reset_p_value <- function(model) {
  tryCatch(
    {
      augmented_data <- model$model %>%
        mutate(
          fitted_squared = fitted(model)^2,
          fitted_cubed = fitted(model)^3
        )

      augmented_model <- lm(
        current_value ~ lag_1 + fitted_squared + fitted_cubed,
        data = augmented_data
      )

      comparison <- anova(model, augmented_model)
      as.numeric(comparison$`Pr(>F)`[2])
    },
    error = function(error) NA_real_
  )
}

# Implement a Breusch-Pagan test using only base R.
# H0: the residual variance is constant (homoskedastic).
# A p-value above 0.05 means heteroskedasticity is not detected.
breusch_pagan_p_value <- function(model) {
  tryCatch(
    {
      squared_residuals <- residuals(model)^2
      lagged_value <- model$model$lag_1
      auxiliary_model <- lm(squared_residuals ~ lagged_value)
      test_statistic <- length(squared_residuals) *
        summary(auxiliary_model)$r.squared

      pchisq(test_statistic, df = 1, lower.tail = FALSE)
    },
    error = function(error) NA_real_
  )
}

# Test whether residual autocorrelation remains after fitting the AR(1).
# H0: residual autocorrelations through the selected lag are jointly zero.
# A p-value above 0.05 supports uncorrelated errors.
ljung_box_p_value <- function(model) {
  residual_values <- residuals(model)
  test_lag <- min(10, max(2, floor(length(residual_values) / 5)))

  safe_p_value(
    Box.test(
      residual_values,
      lag = test_lag,
      type = "Ljung-Box",
      fitdf = 1
    )
  )
}

# Test residual normality for conventional small-sample inference.
# Normality is not a Gauss-Markov assumption, but it is commonly checked.
shapiro_p_value <- function(model) {
  residual_values <- residuals(model)

  # shapiro.test requires 3 to 5,000 observations with some variation.
  if (
    length(residual_values) < 3 ||
    length(residual_values) > 5000 ||
    n_distinct(residual_values) < 3
  ) {
    return(NA_real_)
  }

  safe_p_value(shapiro.test(residual_values))
}

# ============================================================
# 4. Transformation rules
# ============================================================

# Rates are transformed using ordinary first differences.
# This expresses changes in percentage points and keeps zero values valid.
rate_variables <- c(
  "prime",
  "repo",
  "overnight_fx",
  "gov_bond",
  "us_10y",
  "us_inflation"
)

# Prices and indices are transformed using 100 times the log difference.
# For positive values, this approximates the weekly percentage change.
log_difference_variables <- c(
  "usdzar",
  "gold",
  "vix",
  "cpi",
  "crude_oil"
)

# Transform one variable and return both its dates and transformation label.
transform_variable <- function(date, values, variable_name) {
  # Remove missing observations while preserving chronological order.
  valid_data <- tibble(date = date, value = as.numeric(values)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date)

  # Rates use ordinary differences; positive prices and indices use log changes.
  use_first_difference <- variable_name %in% rate_variables
  use_log_difference <- !use_first_difference &&
    variable_name %in% log_difference_variables &&
    all(valid_data$value > 0)

  if (use_log_difference) {
    return(
      tibble(
        date = valid_data$date[-1],
        value = 100 * diff(log(valid_data$value)),
        transformation = "100 x log difference"
      )
    )
  }

  # All rates and any non-positive or unclassified series use first differences.
  tibble(
    date = valid_data$date[-1],
    value = diff(valid_data$value),
    transformation = "first difference"
  )
}

# ============================================================
# 5. Fit AR(1) and test one series
# ============================================================

# Fit an AR(1) to one series and return all diagnostics in one result row.
test_ar1_assumptions <- function(
  values,
  variable_name,
  data_scale,
  transformation_name
) {
  # Convert to numeric and remove non-finite values created by invalid inputs.
  clean_values <- as.numeric(values)
  clean_values <- clean_values[is.finite(clean_values)]

  # An AR(1) requires enough observations and variation to estimate reliably.
  if (length(clean_values) < 20 || n_distinct(clean_values) < 3) {
    return(
      tibble(
        variable = variable_name,
        data_scale = data_scale,
        transformation = transformation_name,
        observations = length(clean_values),
        distinct_values = n_distinct(clean_values),
        ar1_coefficient = NA_real_,
        r_squared = NA_real_,
        adf_p_value = NA_real_,
        kpss_p_value = NA_real_,
        reset_p_value = NA_real_,
        breusch_pagan_p_value = NA_real_,
        ljung_box_p_value = NA_real_,
        shapiro_p_value = NA_real_,
        stationarity_pass = NA,
        linearity_pass = NA,
        homoskedasticity_pass = NA,
        no_residual_autocorrelation_pass = NA,
        normality_for_inference_pass = NA,
        overall_diagnostic_pass = NA,
        note = "Insufficient observations or variation for reliable AR(1) tests"
      )
    )
  }

  # Pair each observation with its value from the previous Friday.
  ar_data <- tibble(
    current_value = clean_values[-1],
    lag_1 = clean_values[-length(clean_values)]
  )

  # Estimate x[t] = intercept + phi*x[t-1] + error[t].
  ar1_model <- lm(current_value ~ lag_1, data = ar_data)

  # Run stationarity tests on the series itself.
  adf_result <- adf_p_value(clean_values)
  kpss_result <- kpss_p_value(clean_values)

  # Run specification and residual tests on the fitted AR(1).
  reset_result <- reset_p_value(ar1_model)
  bp_result <- breusch_pagan_p_value(ar1_model)
  ljung_box_result <- ljung_box_p_value(ar1_model)
  shapiro_result <- shapiro_p_value(ar1_model)

  # Treat stationarity as supported only when ADF and KPSS agree at 5%.
  stationarity_pass <- !is.na(adf_result) &&
    !is.na(kpss_result) &&
    adf_result < 0.05 &&
    kpss_result > 0.05

  # A RESET p-value above 5% does not reject the linear form.
  linearity_pass <- !is.na(reset_result) && reset_result > 0.05

  # A Breusch-Pagan p-value above 5% does not reject constant variance.
  homoskedasticity_pass <- !is.na(bp_result) && bp_result > 0.05

  # A Ljung-Box p-value above 5% does not detect remaining serial correlation.
  no_residual_autocorrelation_pass <- !is.na(ljung_box_result) &&
    ljung_box_result > 0.05

  # A Shapiro p-value above 5% does not reject residual normality.
  normality_for_inference_pass <- !is.na(shapiro_result) &&
    shapiro_result > 0.05

  # Combine the time-series and testable Gauss-Markov diagnostics.
  # Exogeneity is deliberately not included because it cannot be proven by
  # these tests and must be defended from model design and economic reasoning.
  overall_diagnostic_pass <- stationarity_pass &&
    linearity_pass &&
    homoskedasticity_pass &&
    no_residual_autocorrelation_pass

  # Flag stepwise data because annual values carried to Fridays can make tests
  # look more precise than the underlying source frequency really permits.
  diagnostic_note <- if (n_distinct(clean_values) <= 10) {
    paste(
      "Very low variation: interpret tests cautiously;",
      "exogeneity requires economic justification;",
      "normality is not required by Gauss-Markov"
    )
  } else {
    paste(
      "Exogeneity requires economic justification;",
      "normality is not required by Gauss-Markov"
    )
  }

  # Return one tidy row so results from all variables can be stacked.
  tibble(
    variable = variable_name,
    data_scale = data_scale,
    transformation = transformation_name,
    observations = length(clean_values),
    distinct_values = n_distinct(clean_values),
    ar1_coefficient = unname(coef(ar1_model)[["lag_1"]]),
    r_squared = summary(ar1_model)$r.squared,
    adf_p_value = adf_result,
    kpss_p_value = kpss_result,
    reset_p_value = reset_result,
    breusch_pagan_p_value = bp_result,
    ljung_box_p_value = ljung_box_result,
    shapiro_p_value = shapiro_result,
    stationarity_pass = stationarity_pass,
    linearity_pass = linearity_pass,
    homoskedasticity_pass = homoskedasticity_pass,
    no_residual_autocorrelation_pass =
      no_residual_autocorrelation_pass,
    normality_for_inference_pass = normality_for_inference_pass,
    overall_diagnostic_pass = overall_diagnostic_pass,
    note = diagnostic_note
  )
}

# ============================================================
# 6. Loop over every variable: untransformed levels
# ============================================================

# Run the AR(1) and diagnostics on each original series.
level_results <- map_dfr(
  variables_to_test,
  function(variable_name) {
    test_ar1_assumptions(
      values = weekly_data[[variable_name]],
      variable_name = variable_name,
      data_scale = "untransformed level",
      transformation_name = "none"
    )
  }
)

# ============================================================
# 7. Transform every variable and repeat the tests
# ============================================================

# Build one long dataset containing every transformed series.
transformed_long <- map_dfr(
  variables_to_test,
  function(variable_name) {
    transform_variable(
      date = weekly_data$date,
      values = weekly_data[[variable_name]],
      variable_name = variable_name
    ) %>%
      mutate(variable = variable_name, .before = 1)
  }
)

# Repeat the AR(1) diagnostics using the transformed series.
transformed_results <- map_dfr(
  variables_to_test,
  function(variable_name) {
    variable_data <- transformed_long %>%
      filter(variable == variable_name)

    test_ar1_assumptions(
      values = variable_data$value,
      variable_name = variable_name,
      data_scale = "transformed",
      transformation_name = first(variable_data$transformation)
    )
  }
)

# ============================================================
# 8. Save auditable output files
# ============================================================

# Stack level and transformed results for direct comparison.
all_results <- bind_rows(level_results, transformed_results) %>%
  arrange(variable, data_scale)

# Save the complete diagnostic table.
write_csv(
  all_results,
  file.path(output_dir, "ar1_assumption_tests_levels_vs_transformed.csv")
)

# Save the transformed observations in a modelling-friendly wide format.
transformed_weekly_data <- transformed_long %>%
  select(date, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  # Add a suffix so transformed columns cannot be confused with raw columns.
  rename_with(
    ~ paste0(.x, "_transformed"),
    -date
  ) %>%
  arrange(date)

# Use a new output filename. The raw merged CSV is read but never written to.
transformed_data_file <- file.path(
  output_dir,
  "weekly_transformed_copy_2021_2025.csv"
)

# Protect against accidentally pointing the transformed output at the raw file.
if (
  identical(
    tolower(normalizePath(data_file, mustWork = TRUE)),
    tolower(normalizePath(transformed_data_file, mustWork = FALSE))
  )
) {
  stop("The transformed output path must be different from the raw data path.")
}

# Write a separate transformed copy without modifying fx_weekly_merged.csv.
write_csv(
  transformed_weekly_data,
  transformed_data_file
)

# Save a smaller decision table containing the pass/fail columns.
decision_summary <- all_results %>%
  select(
    variable,
    data_scale,
    transformation,
    observations,
    distinct_values,
    stationarity_pass,
    linearity_pass,
    homoskedasticity_pass,
    no_residual_autocorrelation_pass,
    normality_for_inference_pass,
    overall_diagnostic_pass
  )

write_csv(
  decision_summary,
  file.path(output_dir, "ar1_assumption_decision_summary.csv")
)

# ============================================================
# 9. Plot every transformed variable
# ============================================================

# Give the transformed charts readable titles.
variable_labels <- c(
  usdzar = "USD/ZAR exchange rate",
  gold = "Gold price",
  prime = "South African prime lending rate",
  repo = "South African repo rate",
  overnight_fx = "South African overnight FX rate",
  gov_bond = "South African government bond yield",
  vix = "VIX index",
  us_10y = "US 10-year Treasury yield",
  cpi = "South African CPI",
  crude_oil = "Crude oil price",
  us_inflation = "US inflation"
)

# Return a readable label, falling back to a cleaned version of the column name.
label_for <- function(variable_name) {
  if (variable_name %in% names(variable_labels)) {
    return(unname(variable_labels[[variable_name]]))
  }

  str_to_title(str_replace_all(variable_name, "_", " "))
}

# Plot and save one transformed variable.
save_transformed_plot <- function(variable_name) {
  # Select the transformed observations and method for this variable.
  variable_data <- transformed_long %>%
    filter(variable == variable_name)

  transformation_name <- first(variable_data$transformation)

  # Draw the transformed series around a zero reference line.
  transformed_chart <- ggplot(
    variable_data,
    aes(x = date, y = value)
  ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.35,
      colour = "#777777"
    ) +
    geom_line(
      linewidth = 0.65,
      colour = "#B23A48",
      na.rm = TRUE
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(as.Date("2021-01-01"), as.Date("2025-12-31")),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = paste(label_for(variable_name), "- transformed"),
      subtitle = paste(
        transformation_name,
        "| Weekly Friday series, 2021-2025"
      ),
      x = NULL,
      y = transformation_name,
      caption = "Input source: data/fx_weekly_merged.csv"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  # Save the current chart as a high-resolution PNG.
  ggsave(
    filename = file.path(
      transformed_plot_dir,
      paste0("transformed_", variable_name, "_2021_2025.png")
    ),
    plot = transformed_chart,
    width = 10,
    height = 5.5,
    dpi = 180,
    bg = "white"
  )
}

# Run the plotting function once for every transformed variable.
walk(variables_to_test, save_transformed_plot)

# Build one faceted overview using separate y-axis scales for different units.
transformed_overview <- ggplot(
  transformed_long,
  aes(x = date, y = value)
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.25,
    colour = "#777777"
  ) +
  geom_line(
    linewidth = 0.4,
    colour = "#B23A48",
    na.rm = TRUE
  ) +
  facet_wrap(
    vars(variable),
    scales = "free_y",
    ncol = 2,
    labeller = as_labeller(variable_labels, default = label_value)
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits = c(as.Date("2021-01-01"), as.Date("2025-12-31")),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Transformed forecasting variables",
    subtitle = paste(
      "Log differences for positive prices and indices;",
      "first differences for rates"
    ),
    x = NULL,
    y = NULL,
    caption = "Input source: data/fx_weekly_merged.csv"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save the combined transformed-series overview.
ggsave(
  filename = file.path(
    transformed_plot_dir,
    "transformed_variables_overview_2021_2025.png"
  ),
  plot = transformed_overview,
  width = 13,
  height = 18,
  dpi = 180,
  bg = "white"
)

# Print all results in the VS Code R terminal.
print(all_results, n = Inf, width = Inf)

# Confirm where the files were written.
message(
  "Tested ",
  length(variables_to_test),
  " variables in levels and transformed form. The raw merged file was ",
  "not modified. Results and transformed plots were saved to ",
  output_dir,
  "."
)
