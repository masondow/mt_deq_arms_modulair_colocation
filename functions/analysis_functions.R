# R_analysis_functions_functions.R
# This file contains functions for the exploratory analysis of Montana FEM/Modulair PM2.5 collocation data.
# It includes functions to:
#   - Generate descriptive statistics for numeric columns.
#   - Stack monitor data into long format and recode monitor names.
#   - Create scatter plots with best-fit and 1:1 lines (with correlation annotation).
#   - Calculate overall accuracy metrics and stratified metrics by AQI category (including bias direction and count).
#
# Required libraries:
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

#------------------------------------------------------------------------------
# Function: descriptive_stats
# Description: Generate a summary table of descriptive statistics (mean, SD, min, max)
#              for every numeric column in a dataset.
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
# Function: stack_monitor_data
# Description: Stack monitor data into long format and recode monitor names.
# Arguments:
# - data: input data frame.
# - datetime_col: column name for the datetime.
# - monitor_cols: vector of column names for monitors.
# - recode_map: a named vector mapping raw monitor column names to desired labels.
#------------------------------------------------------------------------------
stack_monitor_data <- function(data, datetime_col, monitor_cols, recode_map) {
  data %>%
    select(all_of(c(datetime_col, monitor_cols))) %>%
    pivot_longer(-all_of(datetime_col), names_to = "monitor", values_to = "pm25") %>%
    mutate(monitor = recode(monitor, !!!recode_map))
}

#------------------------------------------------------------------------------
# Function: plot_fit
# Description: Plot the relationship between the Modulair measurement and the FEM measurement,
#              including a linear best-fit line (blue) and a 1:1 line (dashed red).
#              The Pearson correlation coefficient is computed and annotated.
#
# Arguments:
# - data: data frame.
# - mod_col: string; name of the Modulair measurement column (dependent variable).
# - fem_col: string; name of the FEM measurement column (independent variable).
# - site_label: string; label for the site (used in the title).
#------------------------------------------------------------------------------
plot_fit <- function(data, mod_col, fem_col, site_label) {
  # Calculate Pearson correlation
  correlation <- cor(data[[mod_col]], data[[fem_col]], use = "complete.obs", method = "pearson")
  
  ggplot(data, aes_string(x = mod_col, y = fem_col)) +
    geom_point(alpha = 0.5) +  
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  
    labs(title = paste("Best Fit for", site_label, "Site: Modulair vs FEM"),
         x = "Modulair PM2.5 (µg/m³)",
         y = "FEM PM2.5 (µg/m³)") +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = paste("r =", round(correlation, 2)),
             hjust = 1.1, vjust = 1.1, size = 5, color = "blue")
}

#------------------------------------------------------------------------------
# Function: calculate_metrics
# Description: Calculate overall accuracy metrics using a linear regression
#              where the FEM monitor measurement (true value) is the independent variable,
#              and the Modulair monitor measurement (predicted value) is the dependent variable.
#
# EPA recommendations treat the FEM measurement as the gold standard.
#
# Returns a tibble with R-squared, MAE, RMSE, NRMSE (as a percentage), slope, intercept,
# and Average_Error (the mean of residuals, indicating directional bias).
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
  avg_error <- mean(residuals)  # Average error (bias); positive if FEM > Modulair.
  rmse <- sqrt(mean(residuals^2))
  nrmse <- rmse / mean(true_vals) * 100  # Expressed as a percentage
  
  tibble(
    R_squared = r_squared,
    Average_Error = avg_error,
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
#              and includes the frequency of matching AQI categories between the Modulair and FEM monitors.
#
# Arguments:
# - data: Data frame containing the collocation data.
# - fem_col: Column name (string) for the FEM monitor measurement (independent variable).
# - mod_col: Column name (string) for the Modulair monitor measurement (dependent variable).
# - fem_aqi_col: Column name for the FEM AQI category.
# - mod_aqi_col: Column name for the Modulair AQI category.
#
# Returns a list with two components:
#   - overall: A tibble with overall accuracy metrics (including AQI match frequency).
#   - stratified: A tibble with accuracy metrics stratified by FEM AQI category,
#                 including a count of observations.
#------------------------------------------------------------------------------
calculate_all_metrics <- function(data, fem_col, mod_col, fem_aqi_col, mod_aqi_col) {
  # Overall accuracy metrics (FEM is the true value; Modulair is compared against it)
  overall <- calculate_metrics(data[[fem_col]], data[[mod_col]])
  
  # Frequency of matching AQI categories (proportion of time Modulair AQI equals FEM AQI)
  aqi_match <- data %>%
    mutate(match = (.data[[mod_aqi_col]] == .data[[fem_aqi_col]])) %>%
    summarise(match_frequency = mean(match, na.rm = TRUE))
  
  overall <- overall %>% mutate(aqi_match_frequency = aqi_match$match_frequency)
  
  # Stratified metrics by FEM AQI category, including a count of observations.
  stratified <- data %>%
    filter(!is.na(.data[[fem_aqi_col]])) %>%
    group_by(category = .data[[fem_aqi_col]]) %>%
    summarise(
      Count = n(),
      R_squared = if(n() > 1) cor(.data[[fem_col]], .data[[mod_col]], use = "complete.obs")^2 else NA_real_,
      MAE = mean(abs(.data[[fem_col]] - .data[[mod_col]]), na.rm = TRUE),
      Average_Error = mean(.data[[fem_col]] - .data[[mod_col]], na.rm = TRUE),
      RMSE = sqrt(mean((.data[[fem_col]] - .data[[mod_col]])^2, na.rm = TRUE)),
      NRMSE = (sqrt(mean((.data[[fem_col]] - .data[[mod_col]])^2, na.rm = TRUE)) / mean(.data[[fem_col]], na.rm = TRUE)) * 100,
      .groups = "drop"
    )
  
  return(list(overall = overall, stratified = stratified))
}
