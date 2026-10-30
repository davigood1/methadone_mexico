study_start <- as.Date("2018-01-01")
study_end <- as.Date("2025-08-31")
suspension_start <- as.Date("2022-12-01")
reopening_start <- as.Date("2023-08-01")

required_packages <- c(
  "tidyverse", "lubridate", "sandwich", "lmtest", "tseries",
  "MASS", "gridExtra", "sf", "ggrepel", "scales"
)
available <- suppressPackageStartupMessages(
  vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
)
missing_packages <- required_packages[!available]
if (length(missing_packages)) {
  stop("Install these R packages first: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

dir.create("outputs/cache", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

period_label <- function(date) {
  case_when(
    date < suspension_start ~ "Pre-suspension",
    date < reopening_start ~ "Suspension",
    TRUE ~ "Post-reopening"
  )
}

write_table <- function(data, name) {
  readr::write_csv(data, file.path("outputs/tables", name))
}

read_prepared <- function() {
  path <- "outputs/cache/analysis_ready.rds"
  if (!file.exists(path)) stop("Run 1_prepare_data.R first.")
  readRDS(path)
}
