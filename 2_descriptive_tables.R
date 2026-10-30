source("R/common.R")
source("R/reference_data.R")
data <- read_prepared()
analytic <- data$analytic

monthly_formulation <- analytic %>%
  mutate(formulation = recode(formulation,
                              tablet = "Tablet",
                              oral_solution = "Oral solution")) %>%
  group_by(month, formulation) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop")
write_table(monthly_formulation, "monthly_formulation.csv")

monthly <- analytic %>%
  group_by(month) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop") %>%
  complete(month = seq(study_start, floor_date(study_end, "month"), by = "month"),
           fill = list(total_mg = 0)) %>%
  arrange(month) %>%
  mutate(period = period_label(month))
write_table(monthly, "cofepris_monthly_observed.csv")

period_summary <- monthly %>%
  group_by(period) %>%
  summarise(
    months = n(),
    mean_monthly_mg = mean(total_mg),
    median_monthly_mg = median(total_mg),
    q1_monthly_mg = quantile(total_mg, .25),
    q3_monthly_mg = quantile(total_mg, .75),
    period_total_mg = sum(total_mg),
    .groups = "drop"
  )
write_table(period_summary, "period_summary.csv")

state_monthly <- analytic %>%
  filter(!is.na(state)) %>%
  group_by(month, state) %>%
  summarise(total_mg = sum(total_mg), records = n(), .groups = "drop")
write_table(state_monthly, "state_monthly_observed.csv")

state_period <- analytic %>%
  filter(!is.na(state)) %>%
  group_by(period, state) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop") %>%
  group_by(period) %>%
  mutate(share = total_mg / sum(total_mg)) %>%
  ungroup()
write_table(state_period, "state_period_absolute_share.csv")

municipality_period <- analytic %>%
  filter(!is.na(state), !is.na(municipality)) %>%
  group_by(period, state, municipality) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop") %>%
  group_by(period) %>%
  mutate(share = total_mg / sum(total_mg)) %>%
  ungroup()
write_table(municipality_period, "municipality_period_absolute_share.csv")

geo_sensitivity <- tibble(
  analysis = c("National, including missing geography", "Complete geography only"),
  records = c(nrow(analytic),
              sum(!is.na(analytic$state) & !is.na(analytic$municipality))),
  total_mg = c(sum(analytic$total_mg),
               sum(analytic$total_mg[!is.na(analytic$state) &
                                       !is.na(analytic$municipality)]))
) %>% mutate(percent_difference_from_primary = 100 * (total_mg / first(total_mg) - 1))
write_table(geo_sensitivity, "geography_sensitivity.csv")
write_table(cij_annual, "cij_annual_observed.csv")
write_table(site_counts, "site_counts.csv")

message("Wrote national, state, municipality, and CIJ descriptive tables.")
