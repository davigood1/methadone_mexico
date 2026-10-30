source("R/common.R")

input <- "data/analysis_ready_prescriptions.csv"
if (!file.exists(input)) stop("Missing ", input)

expected_columns <- c(
  "capture_date", "state", "municipality", "formulation", "total_mg"
)
prescriptions <- readr::read_csv(input, show_col_types = FALSE)
if (!identical(names(prescriptions), expected_columns)) {
  stop("The release file must contain only: ", paste(expected_columns, collapse = ", "))
}

prescriptions <- prescriptions %>%
  mutate(
    capture_date = as.Date(capture_date),
    month = floor_date(capture_date, "month"),
    period = period_label(capture_date)
  )

if (anyNA(prescriptions$capture_date) ||
    any(prescriptions$capture_date < study_start | prescriptions$capture_date > study_end) ||
    any(!prescriptions$formulation %in% c("tablet", "oral_solution"))) {
  stop("The release file contains an invalid date or formulation.")
}
if (nrow(prescriptions) != 155373 ||
    sum(is.na(prescriptions$total_mg)) != 13 ||
    sum(is.na(prescriptions$state) | is.na(prescriptions$municipality)) != 14) {
  stop("Record counts differ from the documented revision dataset.")
}

analytic <- prescriptions %>% filter(!is.na(total_mg))
saveRDS(list(prescriptions = prescriptions, analytic = analytic),
        "outputs/cache/analysis_ready.rds")

# The first five stages describe cleaning performed on restricted source files.
# The remaining counts are checked against the shared analysis-ready file.
record_flow <- tibble(
  stage = c(
    "Methadone-labelled source records, all available years",
    "Duplicate records removed",
    "Non-methadone ENDOCODIL records removed",
    "Records before 1 January 2018 removed",
    "Records removed during oral-formulation and quantity cleaning",
    "Records in cleaned 2018-August 2025 file",
    "Records with valid date and milligram quantity in national analysis",
    "Records with complete geography in subnational analysis"
  ),
  n = c(
    222871, 575, 16, 66476, 431,
    nrow(prescriptions),
    nrow(analytic),
    sum(!is.na(analytic$state) & !is.na(analytic$municipality))
  )
)
write_table(record_flow, "record_flow.csv")

missing_audit <- bind_rows(
  prescriptions %>%
    filter(is.na(total_mg)) %>%
    count(year = year(capture_date), period, state, municipality, name = "n") %>%
    mutate(issue = "Missing derived milligram quantity"),
  analytic %>%
    filter(is.na(state) | is.na(municipality)) %>%
    count(year = year(capture_date), period, state, municipality, name = "n") %>%
    mutate(issue = "Missing state and municipality")
) %>% select(issue, year, period, state, municipality, n)
write_table(missing_audit, "missing_audit.csv")

message("Prepared ", nrow(analytic), " records with a derived milligram quantity.")
