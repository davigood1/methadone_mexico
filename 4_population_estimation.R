source("R/common.R")

monthly <- readr::read_csv("outputs/tables/cofepris_monthly_observed.csv",
                           show_col_types = FALSE) %>%
  mutate(month = as.Date(month))
state_monthly <- readr::read_csv("outputs/tables/state_monthly_observed.csv",
                                 show_col_types = FALSE) %>%
  mutate(month = as.Date(month))

estimate_people <- function(data) {
  data %>%
    mutate(
      days = days_in_month(month),
      central_80mg_90pct = total_mg / (80 * days * .90),
      dose_40mg_90pct = total_mg / (40 * days * .90),
      dose_120mg_90pct = total_mg / (120 * days * .90),
      coverage_80mg_75pct = total_mg / (80 * days * .75),
      coverage_80mg_95pct = total_mg / (80 * days * .95)
    ) %>%
    select(-days)
}

national_population <- estimate_people(monthly)
write_table(national_population, "population_scenarios_monthly.csv")

national_period <- national_population %>%
  group_by(period) %>%
  summarise(
    across(central_80mg_90pct:coverage_80mg_95pct, median),
    .groups = "drop"
  )
write_table(national_period, "population_scenarios_period_summary.csv")

# Only states represented in the COFEPRIS extract are estimated. A zero month
# means no recorded volume for that state-month, not zero treatment need.
state_population <- state_monthly %>%
  group_by(state) %>%
  complete(
    month = seq(study_start, floor_date(study_end, "month"), by = "month"),
    fill = list(total_mg = 0, records = 0)
  ) %>%
  ungroup() %>%
  mutate(period = period_label(month)) %>%
  estimate_people() %>%
  arrange(state, month)
write_table(state_population, "state_population_scenarios_monthly.csv")

state_period <- state_population %>%
  group_by(state, period) %>%
  summarise(
    months = n(),
    median_monthly_mg = median(total_mg),
    median_estimated_people = median(central_80mg_90pct),
    dose_low = median(dose_120mg_90pct),
    dose_high = median(dose_40mg_90pct),
    coverage_low = median(coverage_80mg_95pct),
    coverage_high = median(coverage_80mg_75pct),
    .groups = "drop"
  ) %>%
  arrange(state, factor(period,
                        levels = c("Pre-suspension", "Suspension", "Post-reopening")))
write_table(state_period, "state_population_scenarios_period_summary.csv")

period_summary <- readr::read_csv("outputs/tables/period_summary.csv",
                                  show_col_types = FALSE)
table_1 <- period_summary %>%
  left_join(national_period, by = "period") %>%
  mutate(
    period = factor(period,
                    levels = c("Pre-suspension", "Suspension", "Post-reopening")),
    recorded_volume_million_mg = sprintf(
      "%.2f (%.2f-%.2f)",
      median_monthly_mg / 1e6, q1_monthly_mg / 1e6, q3_monthly_mg / 1e6
    ),
    central_estimated_people = round(central_80mg_90pct),
    dose_scenario_people = paste0(round(dose_120mg_90pct), "-",
                                  round(dose_40mg_90pct)),
    coverage_scenario_people = paste0(round(coverage_80mg_95pct), "-",
                                      round(coverage_80mg_75pct))
  ) %>%
  arrange(period) %>%
  transmute(
    period = as.character(period), months,
    recorded_volume_million_mg,
    central_estimated_people,
    dose_scenario_people,
    coverage_scenario_people
  )
write_table(table_1, "table_1.csv")

message("Wrote national and state population scenarios, including Table 1.")
