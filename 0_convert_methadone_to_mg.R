#!/usr/bin/env Rscript
# Methadone record cleaning and conversion to milligrams after parsing
#
# This script starts with existing prescription records and parsed medicine
# lookup tables. It does not perform upstream medicine parsing, contact a model
# or external service, download data, or install packages. Parsed fields remain
# required inputs; this is not a replacement for the upstream parsing stage.
#
# Run from the repository root (paths can be absolute or relative):
# Rscript 0_convert_methadone_to_mg.R \
#   --records=Data/df_methadone.csv \
#   --tablet-lookup=Data/parsed/methadone_tablets.csv \
#   --solution-lookup=Data/parsed/methadone_solution.csv \
#   --output-dir=outputs/conversion
#
# Required record columns:
# med_pres, fecha_captura_portal, fecha (ISO date), cantidad (numeric),
# estado, municipio (original municipality labels, including numeric prefixes).
# Required lookup columns: med_pres, form, route, unit_type.
# Optional lookup column: n (frequency used for original lookup ordering).
#
# Jurisdiction names and rule precedence deliberately reproduce the executable
# conversion in the original analysis. Tijuana switches on 2023-04-03, not the
# different date in the original comments. Fallback thresholds below 20 and
# from 20 through 200 apply to ALL otherwise unassigned records. Only the
# fallback above 200 is limited to the eleven listed municipalities.
# These are operational unit assumptions, not independently validated units.
# Do not change these rules without assessing the effect on reported results.
#
# Outputs (source files are never modified):
# df_methadone_cleaned_2018-2025.csv: cleaned records, including missing total_mg
# cofepris_monthly_observed.csv: national volume by month, including zero months
# conversion_rule_audit.csv: record counts and milligrams by conversion rule
# record_flow.csv: sequential processing counts
# input_manifest.csv and sessionInfo.txt: input checksums and R environment
#
# Extra source columns and parsing payloads are not carried into the output.
# Review data-sharing permissions separately before publishing any data files.

required_packages <- c("readr", "dplyr", "stringr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install the required packages before running: ",
       paste(missing_packages, collapse = ", "), call. = FALSE)
}
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
allowed <- c("records", "tablet-lookup", "solution-lookup", "output-dir")
if ("--help" %in% args) {
  cat("Usage: Rscript 0_convert_methadone_to_mg.R ",
      "--records=FILE --tablet-lookup=FILE --solution-lookup=FILE ",
      "[--output-dir=DIR]\n", sep = "")
  quit(status = 0)
}
if (length(args) && any(!grepl("^--[^=]+=.+$", args))) {
  stop("Arguments must use --name=value. Use --help for usage.", call. = FALSE)
}
keys <- sub("^--([^=]+)=.*$", "\\1", args)
if (anyDuplicated(keys) || any(!keys %in% allowed)) {
  stop("Unknown or repeated argument. Use --help for usage.", call. = FALSE)
}
values <- setNames(sub("^--[^=]+=", "", args), keys)
arg_value <- function(name, default) {
  if (name %in% names(values)) unname(values[[name]]) else default
}
records_file <- arg_value("records", "Data/df_methadone.csv")
tablet_file <- arg_value("tablet-lookup", "Data/parsed/methadone_tablets.csv")
solution_file <- arg_value("solution-lookup", "Data/parsed/methadone_solution.csv")
output_dir <- arg_value("output-dir", "outputs/conversion")
input_files <- c(records = records_file, tablets = tablet_file,
                 solution = solution_file)
if (any(!file.exists(input_files))) {
  stop("Missing input file(s): ",
       paste(input_files[!file.exists(input_files)], collapse = ", "),
       call. = FALSE)
}

read_input <- function(path, required) {
  x <- read_csv(path, col_types = cols(.default = col_character()),
                na = c("", "NA"), show_col_types = FALSE, progress = FALSE)
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(basename(path), " is missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  x
}

record_columns <- c("med_pres", "fecha_captura_portal", "fecha", "cantidad",
                    "estado", "municipio")
records <- read_input(records_file, record_columns) %>%
  select(all_of(record_columns))
date_text <- records$fecha
records$fecha <- as.Date(date_text, format = "%Y-%m-%d")
if (any(!is.na(date_text) &
        (is.na(records$fecha) | format(records$fecha, "%Y-%m-%d") != date_text))) {
  stop("fecha must contain ISO dates (YYYY-MM-DD) or missing values.",
       call. = FALSE)
}
records$cantidad <- parse_double(records$cantidad, na = c("", "NA"))
if (nrow(problems(records$cantidad)) ||
    any(!is.na(records$cantidad) & !is.finite(records$cantidad))) {
  stop("cantidad contains invalid numeric values.", call. = FALSE)
}
if (anyNA(records$med_pres)) {
  stop("med_pres must not be missing in the source records.", call. = FALSE)
}

# Deterministic post-parsing lookup corrections. Dose and package-size fields
# are not needed: the final milligram conversion uses the fixed factors below.
read_lookup <- function(path, solution = FALSE) {
  x <- read_input(path, c("med_pres", "form", "route", "unit_type"))
  if (!"n" %in% names(x)) x$n <- NA_real_
  x <- x %>%
    mutate(n = parse_double(as.character(n), na = c("", "NA"))) %>%
    filter(!str_detect(med_pres, if (solution)
      "OXICODONA|OXY|MORFINA|FENTANIL|PALEXIA" else
      "OXICODONA|OXY|MORFINA|FENTANIL")) %>%
    mutate(med_pres = str_replace_all(med_pres, "MCG", "MG"),
           med_pres = str_replace_all(med_pres,
                                      "AMIDONE 40 GM", "AMIDONE 40 MG"))
  if (solution) {
    x <- x %>% mutate(unit_type = if_else(
      str_detect(med_pres,
        "10 MG ML|10M G 1ML|10MG/1ML|10MG 1ML|C/5|10MG / 10 ML|AMPULA|AMPULAS|AMPULES|AMPULE|AMPOLLETAS|AMP"),
      "ampoule", unit_type))
  } else {
    x <- x %>% mutate(unit_type = if_else(
      is.na(unit_type) & str_detect(med_pres, "TABLETA"),
      "tablet", unit_type))
  }
  x %>% select(med_pres, form, route, unit_type, n)
}
lookup <- bind_rows(read_lookup(solution_file, TRUE), read_lookup(tablet_file)) %>%
  arrange(desc(n)) %>%
  select(-n) %>% distinct()
if (anyDuplicated(lookup$med_pres)) {
  stop("Conflicting parsed classifications for the same med_pres; ",
       "resolve the lookup before running.", call. = FALSE)
}

flow <- tibble(stage = character(), records = integer())
record_stage <- function(label, x) {
  flow <<- bind_rows(flow, tibble(stage = label, records = nrow(x)))
  invisible(x)
}
record_stage("Source records, all years", records)
records <- records %>%
  arrange(med_pres, fecha_captura_portal) %>%
  distinct(med_pres, fecha_captura_portal, .keep_all = TRUE)
record_stage("After duplicate removal", records)
records <- records %>% filter(!str_detect(med_pres, "ENDOCODIL"))
record_stage("After non-methadone ENDOCODIL removal", records)
records <- records %>%
  filter(fecha >= as.Date("2018-01-01"), fecha <= as.Date("2025-08-31"))
record_stage("Within January 2018 to August 2025", records)
records <- records %>% left_join(lookup, by = "med_pres")
records <- records %>%
  filter(!unit_type %in% "ampoule", !form %in% "injectable")
record_stage("After ampoule and injectable exclusions", records)
records <- records %>%
  mutate(
    form = if_else((is.na(form) | form %in% "other") &
                     str_detect(med_pres, "TABLETA|TABLETAS|TAB|TABS"),
                   "tablet", form),
    form = if_else(form %in% "drop", "oral_solution", form),
    form = if_else(form %in% "other" &
                     str_detect(med_pres, "LIQUID|LIQUIDO|SOLUCION|SOLUCION ORAL|GOTAS|ML|FRAS"),
                   "oral_solution", form),
    form = if_else(form %in% c("other", "sublingual_tablet"), "tablet", form)
  ) %>%
  filter(!route %in% "intravenous") %>%
  mutate(route = if_else(is.na(route) | route %in% c("sublingual", "other"),
                         "oral", route))
record_stage("After route exclusions and standardisation", records)
records <- records %>% filter(!cantidad > 100000)
record_stage("After missing or extreme quantity exclusion", records)

mg_per_tablet <- 40
tablets_per_box <- 100
mg_per_ml <- 10
ml_per_bottle <- 30
mg_per_box <- mg_per_tablet * tablets_per_box
mg_per_bottle <- mg_per_ml * ml_per_bottle
tijuana_cutoff <- as.Date("2023-04-03")
sonora_cutoff <- as.Date("2024-04-13")
sonora_municipalities <- c("26. SAN LUIS RIO COLORADO", "26. HERMOSILLO")
direct_mg_above_200 <- c(
  "23. CANCUN", "31. MERIDA", "30. CORDOBA", "27. CENTRO", "21. PUEBLA",
  "15. NAUCALPAN DE JUAREZ", "25. CULIACAN", "19. MONTERREY",
  "19. SAN PEDRO GARZA GARCIA", "19. SAN NICOLAS DE LOS GARZA",
  "19. GUADALUPE"
)

# The first matching condition wins. The unused original "direct mg" list is
# intentionally absent: its corresponding condition was not executable.
cleaned <- records %>%
  mutate(conversion_rule = case_when(
    municipio == "2. MEXICALI" & form == "tablet" ~ "01_mexicali_tablet_mg",
    municipio == "2. MEXICALI" & form == "oral_solution" ~ "02_mexicali_solution_mg",
    municipio == "2. TIJUANA" & fecha < tijuana_cutoff ~ "03_tijuana_before_mg",
    municipio == "2. TIJUANA" & fecha >= tijuana_cutoff & form == "tablet" ~ "04_tijuana_boxes",
    municipio == "2. TIJUANA" & fecha >= tijuana_cutoff & form == "oral_solution" ~ "05_tijuana_bottles",
    municipio %in% sonora_municipalities & fecha < sonora_cutoff ~ "06_sonora_before_mg",
    municipio %in% sonora_municipalities & fecha >= sonora_cutoff & form == "tablet" ~ "07_sonora_boxes",
    municipio %in% sonora_municipalities & fecha >= sonora_cutoff & form == "oral_solution" ~ "08_sonora_bottles",
    cantidad < 20 & form == "oral_solution" ~ "09_fallback_bottles_below_20",
    cantidad < 20 & form == "tablet" ~ "10_fallback_boxes_below_20",
    cantidad >= 20 & cantidad <= 200 & form == "tablet" ~ "11_fallback_tablets_20_to_200",
    cantidad >= 20 & cantidad <= 200 & form == "oral_solution" ~ "12_fallback_ml_20_to_200",
    municipio %in% direct_mg_above_200 & cantidad > 200 ~ "13_listed_municipalities_above_200_mg",
    TRUE ~ "14_no_conversion_rule"
  ),
  conversion_factor = case_when(
    conversion_rule %in% c("01_mexicali_tablet_mg", "02_mexicali_solution_mg",
      "03_tijuana_before_mg", "06_sonora_before_mg",
      "13_listed_municipalities_above_200_mg") ~ 1,
    conversion_rule %in% c("04_tijuana_boxes", "07_sonora_boxes",
      "10_fallback_boxes_below_20") ~ mg_per_box,
    conversion_rule %in% c("05_tijuana_bottles", "08_sonora_bottles",
      "09_fallback_bottles_below_20") ~ mg_per_bottle,
    conversion_rule == "11_fallback_tablets_20_to_200" ~ mg_per_tablet,
    conversion_rule == "12_fallback_ml_20_to_200" ~ mg_per_ml,
    TRUE ~ NA_real_
  ), total_mg = cantidad * conversion_factor)

analytic <- cleaned %>% filter(!is.na(fecha), !is.na(total_mg))
record_stage("Cleaned records, including missing derived milligrams", cleaned)
record_stage("National analytic records", analytic)
record_stage("Subnational records with complete geography",
             analytic %>% filter(!is.na(estado), !is.na(municipio)))
rule_audit <- cleaned %>%
  group_by(conversion_rule, conversion_factor) %>%
  summarise(records = n(), records_with_mg = sum(!is.na(total_mg)),
            total_mg = sum(total_mg, na.rm = TRUE), .groups = "drop")
monthly <- analytic %>% mutate(month = as.Date(format(fecha, "%Y-%m-01"))) %>%
  group_by(month) %>% summarise(total_mg = sum(total_mg), .groups = "drop")
monthly <- tibble(month = seq(as.Date("2018-01-01"), as.Date("2025-08-01"),
                             by = "month")) %>%
  left_join(monthly, by = "month") %>%
  mutate(total_mg = coalesce(total_mg, 0))
stopifnot(nrow(monthly) == 92L,
          abs(sum(monthly$total_mg) - sum(analytic$total_mg)) < 1e-6)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_names <- c("df_methadone_cleaned_2018-2025.csv",
  "cofepris_monthly_observed.csv", "conversion_rule_audit.csv",
  "record_flow.csv", "input_manifest.csv", "sessionInfo.txt")
output_paths <- file.path(output_dir, output_names)
if (any(normalizePath(output_paths, mustWork = FALSE) %in%
        normalizePath(input_files, mustWork = TRUE))) {
  stop("Output paths must not overwrite an input file.", call. = FALSE)
}
if (any(file.exists(output_paths))) {
  stop("Output files already exist; choose a new --output-dir.", call. = FALSE)
}
write_csv(cleaned, output_paths[1])
write_csv(monthly, output_paths[2])
write_csv(rule_audit, output_paths[3])
write_csv(flow, output_paths[4])
manifest <- tibble(input = names(input_files),
                   file = basename(input_files),
                   md5 = unname(tools::md5sum(input_files)))
write_csv(manifest, output_paths[5])
writeLines(capture.output(sessionInfo()), output_paths[6])
cat("Cleaned records:", nrow(cleaned), "\n",
    "National analytic records:", nrow(analytic), "\n",
    "Records without derived milligrams:", sum(is.na(cleaned$total_mg)), "\n",
    "National total (mg):", format(sum(analytic$total_mg), scientific = FALSE),
    "\nOutputs:", output_dir, "\n")
