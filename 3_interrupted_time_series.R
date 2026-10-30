source("R/common.R")
analytic <- read_prepared()$analytic
monthly <- readr::read_csv("outputs/tables/cofepris_monthly_observed.csv",
                           show_col_types = FALSE) %>%
  mutate(month = as.Date(month))

# The model uses observed monthly volume. No smoothing is applied.
model_data <- monthly %>%
  mutate(
    time0 = row_number() - 1L,
    suspension = as.integer(month >= suspension_start),
    slope_suspension = pmax(0, time0 - match(suspension_start, month) + 1),
    reopening = as.integer(month >= reopening_start),
    slope_reopening = pmax(0, time0 - match(reopening_start, month) + 1),
    sin12 = sin(2 * pi * time0 / 12),
    cos12 = cos(2 * pi * time0 / 12),
    log_mg = log1p(total_mg)
  )
model_terms <- c("time0", "suspension", "slope_suspension",
                 "reopening", "slope_reopening", "sin12", "cos12")
fit_ar1 <- function(data, terms) {
  stats::arima(
    data$log_mg,
    xreg = as.matrix(data[, terms]),
    order = c(1, 0, 0),
    include.mean = TRUE,
    method = "ML"
  )
}
primary <- fit_ar1(model_data, model_terms)

coefficient_table <- function(fit) {
  tibble(
    term = names(fit$coef),
    estimate = as.numeric(fit$coef),
    se = sqrt(diag(fit$var.coef))
  ) %>%
    mutate(conf_low = estimate - qnorm(.975) * se,
           conf_high = estimate + qnorm(.975) * se)
}
write_table(coefficient_table(primary), "primary_ar1_coefficients.csv")

contrast <- function(coef, covariance, terms, critical = qnorm(.975)) {
  weights <- setNames(rep(0, length(coef)), names(coef))
  weights[names(terms)] <- terms
  estimate <- sum(weights * coef)
  se <- sqrt(drop(t(weights) %*% covariance %*% weights))
  c(estimate = estimate, se = se,
    low = estimate - critical * se,
    high = estimate + critical * se)
}

segment_slopes <- function(coef, covariance, time_unit = "month",
                           critical = qnorm(.975)) {
  bind_rows(
    tibble(segment = "Pre-suspension",
           !!!as.list(contrast(coef, covariance, c(time0 = 1), critical))),
    tibble(segment = "Suspension",
           !!!as.list(contrast(coef, covariance,
                               c(time0 = 1, slope_suspension = 1), critical))),
    tibble(segment = "Post-reopening",
           !!!as.list(contrast(coef, covariance,
                               c(time0 = 1, slope_suspension = 1,
                                 slope_reopening = 1), critical)))
  ) %>%
    mutate(
      time_unit = time_unit,
      percent_change = 100 * (exp(estimate) - 1),
      percent_low = 100 * (exp(low) - 1),
      percent_high = 100 * (exp(high) - 1),
      annual_percent_change = 100 * (exp(if_else(time_unit == "month", 12, 4) * estimate) - 1),
      annual_low = 100 * (exp(if_else(time_unit == "month", 12, 4) * low) - 1),
      annual_high = 100 * (exp(if_else(time_unit == "month", 12, 4) * high) - 1)
    )
}
primary_slopes <- segment_slopes(primary$coef, primary$var.coef)
write_table(primary_slopes, "primary_segment_slopes.csv")

level_effects <- bind_rows(
  tibble(interruption = "Suspension start",
         !!!as.list(contrast(primary$coef, primary$var.coef,
                             c(suspension = 1)))),
  tibble(interruption = "Reopening",
         !!!as.list(contrast(primary$coef, primary$var.coef,
                             c(reopening = 1))))
) %>%
  mutate(
    relative_change_percent = 100 * (exp(estimate) - 1),
    relative_low = 100 * (exp(low) - 1),
    relative_high = 100 * (exp(high) - 1)
  )
write_table(level_effects, "primary_level_effects.csv")

# Compare the fitted trajectory with a continuation of the earlier trend.
design_at <- function(date, comparison = c("observed", "pretrend", "suspension")) {
  comparison <- match.arg(comparison)
  row <- model_data %>% filter(month == as.Date(date))
  if (nrow(row) != 1) stop("Date absent from the monthly series: ", date)
  x <- c(
    ar1 = 0, time0 = row$time0, suspension = row$suspension,
    slope_suspension = row$slope_suspension,
    reopening = row$reopening, slope_reopening = row$slope_reopening,
    sin12 = row$sin12, cos12 = row$cos12, intercept = 1
  )[names(primary$coef)]
  if (comparison == "pretrend") {
    x[c("suspension", "slope_suspension", "reopening", "slope_reopening")] <- 0
  }
  if (comparison == "suspension") {
    x[c("reopening", "slope_reopening")] <- 0
  }
  x
}

set.seed(20260918)
draws <- MASS::mvrnorm(20000, mu = primary$coef, Sigma = primary$var.coef)
counterfactual_row <- function(date, comparison) {
  x_fitted <- design_at(date)
  x_other <- design_at(date, comparison)
  fitted <- exp(sum(x_fitted * primary$coef)) - 1
  other <- exp(sum(x_other * primary$coef)) - 1
  fitted_draws <- exp(drop(draws %*% x_fitted)) - 1
  other_draws <- exp(drop(draws %*% x_other)) - 1
  tibble(
    month = as.Date(date), comparison = comparison,
    fitted_mg = fitted, counterfactual_mg = other,
    absolute_difference_mg = fitted - other,
    abs_low = quantile(fitted_draws - other_draws, .025),
    abs_high = quantile(fitted_draws - other_draws, .975),
    relative_difference_percent = 100 * (fitted / other - 1),
    rel_low = quantile(100 * (fitted_draws / other_draws - 1), .025),
    rel_high = quantile(100 * (fitted_draws / other_draws - 1), .975)
  )
}
counterfactuals <- bind_rows(
  counterfactual_row("2022-12-01", "pretrend"),
  counterfactual_row("2023-08-01", "suspension"),
  counterfactual_row("2023-08-01", "pretrend"),
  counterfactual_row("2025-08-01", "pretrend")
)
write_table(counterfactuals, "counterfactual_contrasts.csv")

residuals_primary <- residuals(primary)
diagnostics <- tibble(
  diagnostic = c("AR1 coefficient", "Ljung-Box p value lag 12",
                 "ADF p value", "Residual variance"),
  value = c(
    primary$coef[["ar1"]],
    Box.test(residuals_primary, lag = 12, type = "Ljung-Box", fitdf = 1)$p.value,
    tseries::adf.test(residuals_primary, alternative = "stationary")$p.value,
    var(residuals_primary)
  )
)
write_table(diagnostics, "primary_diagnostics.csv")

# Sensitivity checks retain the same interruption dates and outcome scale.
ols <- lm(log_mg ~ time0 + suspension + slope_suspension +
            reopening + slope_reopening + sin12 + cos12,
          data = model_data)
hac_vcov <- sandwich::NeweyWest(ols, lag = 12, prewhite = FALSE, adjust = TRUE)
hac_coef <- coef(ols)
hac_critical <- qt(.975, df.residual(ols))
write_table(
  tibble(term = names(hac_coef), estimate = as.numeric(hac_coef),
         se = sqrt(diag(hac_vcov))) %>%
    mutate(conf_low = estimate - hac_critical * se,
           conf_high = estimate + hac_critical * se),
  "hac_sensitivity_coefficients.csv"
)

quadratic <- fit_ar1(
  model_data %>% mutate(time_sq = time0^2),
  c(model_terms, "time_sq")
)
write_table(coefficient_table(quadratic), "quadratic_sensitivity_coefficients.csv")

# With a quadratic trend, the secular slope varies over time. Report its
# average derivative within each study period.
quadratic_slopes <- bind_rows(
  tibble(segment = "Pre-suspension",
         !!!as.list(contrast(
           quadratic$coef, quadratic$var.coef,
           c(time0 = 1,
             time_sq = 2 * mean(model_data$time0[model_data$month < suspension_start]))
         ))),
  tibble(segment = "Suspension",
         !!!as.list(contrast(
           quadratic$coef, quadratic$var.coef,
           c(time0 = 1, slope_suspension = 1,
             time_sq = 2 * mean(model_data$time0[
               model_data$month >= suspension_start &
                 model_data$month < reopening_start]))
         ))),
  tibble(segment = "Post-reopening",
         !!!as.list(contrast(
           quadratic$coef, quadratic$var.coef,
           c(time0 = 1, slope_suspension = 1, slope_reopening = 1,
             time_sq = 2 * mean(model_data$time0[model_data$month >= reopening_start]))
         )))
) %>%
  mutate(
    time_unit = "month",
    percent_change = 100 * (exp(estimate) - 1),
    percent_low = 100 * (exp(low) - 1),
    percent_high = 100 * (exp(high) - 1),
    annual_percent_change = 100 * (exp(12 * estimate) - 1),
    annual_low = 100 * (exp(12 * low) - 1),
    annual_high = 100 * (exp(12 * high) - 1)
  )

daily_state <- analytic %>%
  mutate(state_group = replace_na(state, "Missing geography")) %>%
  group_by(state_group, month, capture_date) %>%
  summarise(n_records = n(), daily_mg = sum(total_mg), .groups = "drop") %>%
  group_by(state_group, month) %>%
  mutate(month_mg = sum(daily_mg),
         share = if_else(month_mg > 0, daily_mg / month_mg, 0)) %>%
  ungroup() %>%
  mutate(flag_bulk = n_records >= 20 & share >= .50)
bulk_days <- daily_state %>% filter(flag_bulk)
write_table(bulk_days, "suspected_bulk_upload_days.csv")

without_bulk <- analytic %>%
  mutate(state_group = replace_na(state, "Missing geography")) %>%
  anti_join(bulk_days %>% select(state_group, month, capture_date),
            by = c("state_group", "month", "capture_date")) %>%
  group_by(month) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop")
no_bulk_data <- model_data %>%
  select(-total_mg, -log_mg) %>%
  left_join(without_bulk, by = "month") %>%
  mutate(total_mg = replace_na(total_mg, 0), log_mg = log1p(total_mg))
no_bulk <- fit_ar1(no_bulk_data, model_terms)
write_table(coefficient_table(no_bulk), "bulk_exclusion_sensitivity_coefficients.csv")

quarterly <- monthly %>%
  mutate(quarter = floor_date(month, "quarter")) %>%
  group_by(quarter) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop") %>%
  arrange(quarter) %>%
  mutate(
    time0 = row_number() - 1L,
    suspension = as.integer(quarter >= floor_date(suspension_start, "quarter")),
    slope_suspension = pmax(0, time0 -
                              match(floor_date(suspension_start, "quarter"), quarter) + 1),
    reopening = as.integer(quarter >= floor_date(reopening_start, "quarter")),
    slope_reopening = pmax(0, time0 -
                             match(floor_date(reopening_start, "quarter"), quarter) + 1),
    log_mg = log1p(total_mg)
  )
quarter_terms <- c("time0", "suspension", "slope_suspension",
                   "reopening", "slope_reopening")
quarter_fit <- fit_ar1(quarterly, quarter_terms)
write_table(coefficient_table(quarter_fit), "quarterly_sensitivity_coefficients.csv")

complete_geo <- analytic %>%
  filter(!is.na(state), !is.na(municipality)) %>%
  group_by(month) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop")
geo_data <- model_data %>%
  select(-total_mg, -log_mg) %>%
  left_join(complete_geo, by = "month") %>%
  mutate(total_mg = replace_na(total_mg, 0), log_mg = log1p(total_mg))
geo_fit <- fit_ar1(geo_data, model_terms)
write_table(coefficient_table(geo_fit), "geography_sensitivity_coefficients.csv")

# S6 reports each model on its own time scale; quarterly slopes are per quarter.
model_summary <- bind_rows(
  primary_slopes %>% mutate(analysis = "Primary model"),
  segment_slopes(hac_coef, hac_vcov, critical = hac_critical) %>%
    mutate(analysis = "Newey-West standard errors"),
  quadratic_slopes %>%
    mutate(analysis = "Quadratic secular trend"),
  segment_slopes(quarter_fit$coef, quarter_fit$var.coef, "quarter") %>%
    mutate(analysis = "Quarterly aggregation"),
  segment_slopes(no_bulk$coef, no_bulk$var.coef) %>%
    mutate(analysis = "Exclusion of suspected bulk-upload dates"),
  segment_slopes(geo_fit$coef, geo_fit$var.coef) %>%
    mutate(analysis = "Exclusion of records without geography")
) %>%
  select(analysis, segment, time_unit, percent_change, percent_low,
         percent_high, annual_percent_change, annual_low, annual_high)
write_table(model_summary, "model_sensitivity_summary.csv")

message("Fitted the primary interrupted time series and five sensitivity models.")
