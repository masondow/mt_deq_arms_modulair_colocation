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
library(tidymodels)
library(readxl)
library(lubridate)
library(AirMonitor)
library(here)
library(purrr)
library(knitr)
library(patchwork)
library(GGally)

#------------------------------------------------------------------------------
# Function: averaging_threshold
# Description: Helper function to calculate the mean with a 75% data completion threshold.
# x: a numeric vector.
# threshold: the fraction of expected observations required (default 0.75).
# expected: the expected number of observations in a full day (default 24).
#------------------------------------------------------------------------------
averaging_threshold <- function(x, threshold = 0.75, expected = 24) {
  if(sum(!is.na(x)) >= threshold * expected) {
    mean(x, na.rm = TRUE)
  } else {
    NA_real_
  }
}
#------------------------------------------------------------------------------
# Function: daily_averages
# Description: Function to calculate daily (24-hour) averages for a dataset,
# applying the 75% data completion threshold to each numeric column.
#------------------------------------------------------------------------------
daily_averages <- function(data, datetime_col = "datetime") {
  data %>%
    mutate(date = as_date(.data[[datetime_col]])) %>%
    group_by(date) %>%
    summarise(across(where(is.numeric), ~ averaging_threshold(.x, threshold = 0.75, expected = 24)),
              siteid = first(siteid),
              .groups = "drop")
}

#------------------------------------------------------------------------------
# Function: summarize_counts
# Description: helper function to summarize each site's data, counts and percentages
# by AQI category 
#------------------------------------------------------------------------------
summarize_counts <- function(data,
                           siteid_col = "siteid",
                           fem_col,
                           mod_col,
                           aqi_col) {
  
  # Number of total rows
  total_obs <- nrow(data)
  
  # Number of rows where both FEM and Modulair PM2.5 measurements are present
  complete_obs <- data %>%
    filter(!is.na(.data[[fem_col]]) & !is.na(.data[[mod_col]])) %>%
    nrow()
  
  # Tally each FEM AQI category
  # (1=Good, 2=Moderate, 3=USG, 4=Unhealthy, 5=Very Unhealthy, 6=Hazardous)
  # If your actual numeric codes differ, adjust the case_when below.
  aqi_summary <- data %>%
    filter(!is.na(.data[[aqi_col]])) %>%
    group_by(.data[[aqi_col]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = 100 * count / sum(count)) %>%
    mutate(category_name = case_when(
      .data[[aqi_col]] == 1 ~ "Good",
      .data[[aqi_col]] == 2 ~ "Moderate",
      .data[[aqi_col]] == 3 ~ "USG",          # Unhealthy for Sensitive Groups
      .data[[aqi_col]] == 4 ~ "Unhealthy",
      .data[[aqi_col]] == 5 ~ "V.Unhealthy",
      .data[[aqi_col]] == 6 ~ "Hazardous",
      TRUE                 ~ "Unknown"
    ))
  
  # Pivot to wide so each AQI category has columns like Good_count, Good_percent, etc.
  aqi_wide <- aqi_summary %>%
    select(category_name, count, percent) %>%
    pivot_wider(
      names_from = category_name,
      values_from = c(count, percent),
      names_glue = "{category_name}_{.value}"
    )
  
  # Build a single-row tibble for this site
  final <- tibble(
    site = unique(data[[siteid_col]]),
    total_obs = total_obs,
    complete_obs = complete_obs
  ) %>%
    bind_cols(aqi_wide)
  
  final
}
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
# Returns: a tibble with R-squared, MAE, RMSE, NRMSE (as a percentage), slope, intercept,
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
#------------------------------------------------------------------------------
# Function: fit_evaluate_wf
# Description: 
#   Fits a tidymodels workflow on the training data, makes predictions on the testing data,
#   calculates residuals, computes predicted AQI categories (using aqiCategories()), and 
#   calculates performance metrics (RMSE, R-squared, MAE) using yardstick functions.
#
# Arguments:
#   wf       : A tidymodels workflow object (containing both the model and recipe).
#   wf_name  : A character string identifier for the workflow/model (for labeling outputs).
#   train_data: The training dataset.
#   test_data : The testing dataset.
#
# Returns:
#   A list containing:
#     - fit: The fitted workflow model.
#     - predictions: A tibble with the testing data augmented with predictions, residuals, 
#                    the workflow name, and predicted AQI category.
#     - metrics: A tibble of performance metrics (RMSE, R-squared, MAE) computed using yardstick.
#------------------------------------------------------------------------------
fit_evaluate_wf <- function(wf, wf_name, train_data, test_data) {
  # Fit the workflow on the training data
  fit_mod <- wf %>% fit(data = train_data)
  
  # Generate predictions on the testing data and compute residuals.
  # We also add a column for the model name and predicted AQI category.
  preds <- predict(fit_mod, new_data = test_data) %>% 
    bind_cols(test_data) %>% 
    mutate(
      residual = fem_avg - .pred,   # residual = observed minus predicted
      model = wf_name,
      predicted_aqi = aqiCategories(.pred, pollutant = "PM2.5", NAAQS = "PM2.5_2024")
    )
  
  # Calculate performance metrics using yardstick functions
  metrics_tbl <- preds %>%
    metrics(truth = fem_avg, estimate = .pred) %>%
    mutate(model = wf_name)
  
  # Return a list with the fitted model, predictions, and metrics
  list(
    fit = fit_mod,
    predictions = preds,
    metrics = metrics_tbl
  )
}
#------------------------------------------------------------------------------
# Function: append_predictions
# Description:
#   Given a fitted workflow (wf) and a dataset, this function generates predictions
#   and appends them as a new column to the dataset. The new column is named using
#   the string provided in new_col_name.
#
# Arguments:
#   wf          : A fitted tidymodels workflow object.
#   data        : A data frame on which to make predictions.
#   new_col_name: A string specifying the name for the new prediction column.
#
# Returns:
#   The input data frame with an added column containing the predicted values.
#------------------------------------------------------------------------------
append_predictions <- function(wf, data, new_col_name) {
  # Generate predictions using the fitted workflow on the provided data.
  preds <- predict(wf, new_data = data) %>% 
    pull(.pred)
  
  # Append the predictions as a new column with the specified name.
  data %>% 
    mutate(!!new_col_name := preds)
}
#------------------------------------------------------------------------------
# Function: plot_model_fit
# Description: Plots the relationship between a model's predicted FEM PM2.5 values and the actual
#              FEM PM2.5 measurements from the provided test dataset. It adds a best-fit line,
#              a 1:1 reference line, and annotates the plot with the Pearson correlation coefficient.
#
# Arguments:
# - test_data: A tibble containing the test dataset.
# - pred_col: A string specifying the name of the prediction column (predicted FEM PM2.5).
# - fem_col: A string specifying the name of the FEM PM2.5 column (default is "fem_avg").
#
# Returns:
# - A ggplot object.
#------------------------------------------------------------------------------
plot_model_fit <- function(test_data, pred_col, fem_col = "fem_avg") {
  # Calculate Pearson correlation between predicted and FEM values
  correlation <- cor(test_data[[pred_col]], test_data[[fem_col]], 
                     use = "complete.obs", method = "pearson")
  
  # Create the plot using tidy evaluation to reference the prediction column
  ggplot(test_data, aes(x = .data[[pred_col]], y = .data[[fem_col]])) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Model:", pred_col),
         x = "Predicted FEM PM2.5 (µg/m³)",
         y = "FEM PM2.5 (µg/m³)") +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, 
             label = paste("r =", round(correlation, 2)),
             hjust = 1.1, vjust = 1.1, size = 5, color = "blue")
}
