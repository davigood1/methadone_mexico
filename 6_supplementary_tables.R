source("R/common.R")
source("R/reporting_text.R")

# S1-S4 contain reporting text rather than estimates. The text is held here
# so the public run does not need a copy of the submitted Word supplement.
write_table(supplementary_table_s1, "supplementary_table_s1.csv")
write_table(supplementary_table_s2, "supplementary_table_s2.csv")
write_table(supplementary_table_s3, "supplementary_table_s3.csv")
write_table(supplementary_table_s4, "supplementary_table_s4.csv")

flow <- readr::read_csv("outputs/tables/record_flow.csv", show_col_types = FALSE)
expected_s5 <- c(
  flow$n[1] - flow$n[3] - flow$n[4],
  flow$n[2], flow$n[5], flow$n[6],
  flow$n[6] - flow$n[7], flow$n[7], flow$n[8]
)
reported_s5 <- as.integer(gsub(" ", "", supplementary_table_s5$records))
if (!identical(as.integer(expected_s5), reported_s5)) {
  stop("Supplementary Table S5 does not agree with the record flow.")
}
write_table(supplementary_table_s5, "supplementary_table_s5.csv")

format_percent <- function(estimate, low, high) {
  sprintf("%.2f%% (%.2f to %.2f)", estimate, low, high)
}
format_level <- function(estimate, low, high) {
  sprintf("%.2f (%.2f to %.2f)", estimate, low, high)
}

slopes <- readr::read_csv("outputs/tables/model_sensitivity_summary.csv",
                          show_col_types = FALSE) %>%
  mutate(
    segment = recode(segment,
                     "Pre-suspension" = "pre_suspension_slope",
                     "Suspension" = "suspension_period_slope",
                     "Post-reopening" = "post_reopening_slope"),
    value = format_percent(percent_change, percent_low, percent_high)
  ) %>%
  select(analysis, segment, value) %>%
  pivot_wider(names_from = segment, values_from = value)

coefficient_files <- c(
  "Primary model" = "primary_ar1_coefficients.csv",
  "Newey-West standard errors" = "hac_sensitivity_coefficients.csv",
  "Quadratic secular trend" = "quadratic_sensitivity_coefficients.csv",
  "Quarterly aggregation" = "quarterly_sensitivity_coefficients.csv",
  "Exclusion of suspected bulk-upload dates" =
    "bulk_exclusion_sensitivity_coefficients.csv",
  "Exclusion of records without geography" =
    "geography_sensitivity_coefficients.csv"
)
levels <- purrr::imap_dfr(coefficient_files, function(file, model_name) {
  readr::read_csv(file.path("outputs/tables", file),
                  show_col_types = FALSE) %>%
    filter(term %in% c("suspension", "reopening")) %>%
    mutate(
      analysis = model_name,
      term = recode(term,
                    suspension = "immediate_level_at_suspension",
                    reopening = "immediate_level_at_reopening"),
      value = format_level(estimate, conf_low, conf_high)
    ) %>%
    select(analysis, term, value) %>%
    pivot_wider(names_from = term, values_from = value)
})

table_s6 <- slopes %>%
  left_join(levels, by = "analysis") %>%
  mutate(analysis = factor(analysis, levels = names(coefficient_files))) %>%
  arrange(analysis) %>%
  mutate(analysis = as.character(analysis))
if (anyNA(table_s6)) stop("Supplementary Table S6 is incomplete.")
write_table(table_s6, "supplementary_table_s6.csv")

message("Wrote supplementary Tables S1-S6.")
