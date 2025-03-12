# functions.R
# This file contains functions for analysis annd modeling of Montana FEM/Modulair PM2.5 collocation data.
#
# Libraries required:

library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

#------------------------------------------------------------------------------
# Function: descriptive_stats
# Description: Generate a summary table of descriptive statistics (mean, SD, min, max)
#              for all numeric columns in a dataset.
#------------------------------------------------------------------------------
descriptive_stats <- function(data) {
  data %>%
    summarise(across(where(is.numeric),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd   = ~sd(.x, na.rm = TRUE),
                          min  = ~min(.x, na.rm = TRUE),
                          max  = ~max(.x, na.rm = TRUE)))) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("parameter", ".value"),
      names_pattern = "(.+)_(mean|sd|min|max)$"
    )
}

#------------------------------------------------------------------------------
# Function: calculate_metrics
# Description: Calculate overall accuracy metrics using a linear regression
#              where the FEM monitor measurement (true value) is the independent variable,
#              and the Modulair monitor measurement (predicted value) is the dependent variable.
#
# EPA recommendations treat the FEM measurement as the gold standard.
#
# Returns a tibble with R-squared, MAE, RMSE, NRMSE (as a percentage), slope, and intercept.
#------------------------------------------------------------------------------
calculate_metrics <- function(true_values, predicted_values) {
  # Remove missing values
  valid_idx <- !is.na(true_values) & !is.na(predicted_values)
  true_vals <- true_values[valid_idx]
  pred_vals <- predicted_values[valid_idx]
  
  # Fit linear regression: predicted ~ true
  # FEM measurement (true_vals) is the independent variable,
  # and the Modulair measurement (pred_vals) is the dependent variable.
  fit <- lm(pred_vals ~ true_vals)
  slope <- coef(fit)[2]
  intercept <- coef(fit)[1]
  r_squared <- summary(fit)$r.squared
  
  # Calculate residuals and error metrics
  residuals <- true_vals - pred_vals
  mae <- mean(abs(residuals))
  rmse <- sqrt(mean(residuals^2))
  nrmse <- rmse / mean(true_vals) * 100  # Expressed as percentage
  
  tibble(
    R_squared = r_squared,
    MAE = mae,
    RMSE = rmse,
    NRMSE = nrmse,
    Slope = slope,
    Intercept = intercept
  )
}

#------------------------------------------------------------------------------
# Function: calculate_all_metrics
# Description: Calculates overall accuracy metrics, stratified accuracy metrics by FEM AQI category,
#              and includes the frequency of matching AQI categories between the Modulair monitor and the FEM monitor.
#
# Arguments:
# - data: Data frame containing the collocation data.
# - fem_col: Column name (string) for the FEM monitor measurement (independent variable).
# - mod_col: Column name (string) for the Modulair monitor measurement (dependent variable).
# - fem_aqi_col: Column name for the FEM AQI category.
# - mod_aqi_col: Column name for the Modulair AQI category.
#
# The function returns a list with two components:
#   - overall: A tibble with overall accuracy metrics plus the AQI match frequency.
#   - stratified: A tibble with accuracy metrics stratified by FEM AQI category.
#------------------------------------------------------------------------------
calculate_all_metrics <- function(data, fem_col, mod_col, fem_aqi_col, mod_aqi_col) {
  # Overall accuracy metrics based on EPA recommendations:
  # FEM measurement is the gold standard (true value) and the Modulair measurement is compared against it.
  overall <- calculate_metrics(data[[fem_col]], data[[mod_col]])
  
  # Calculate frequency of matching AQI categories between Modulair and FEM monitors.
  # This metric shows the proportion of time the Modulair's AQI category matches the FEM's AQI category.
  aqi_match <- data %>%
    mutate(match = (.data[[mod_aqi_col]] == .data[[fem_aqi_col]])) %>%
    summarise(match_frequency = mean(match, na.rm = TRUE))
  
  # Add the AQI match frequency as a new column to the overall metrics.
  overall <- overall %>% mutate(aqi_match_frequency = aqi_match$match_frequency)
  
  # Stratified accuracy metrics by FEM AQI category:
  # Filter out rows with NA in the FEM AQI column and then group by this category.
  stratified <- data %>%
    filter(!is.na(.data[[fem_aqi_col]])) %>%
    group_by(category = .data[[fem_aqi_col]]) %>%
    summarise(
      R_squared = if(n() > 1) cor(.data[[fem_col]], .data[[mod_col]], use = "complete.obs")^2 else NA_real_,
      MAE = mean(abs(.data[[fem_col]] - .data[[mod_col]]), na.rm = TRUE),
      RMSE = sqrt(mean((.data[[fem_col]] - .data[[mod_col]])^2, na.rm = TRUE)),
      NRMSE = (sqrt(mean((.data[[fem_col]] - .data[[mod_col]])^2, na.rm = TRUE)) / mean(.data[[fem_col]], na.rm = TRUE)) * 100,
      .groups = "drop"
    )
  
  return(list(overall = overall, stratified = stratified))
}
