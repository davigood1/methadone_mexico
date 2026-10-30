if (!file.exists("1_prepare_data.R")) {
  stop("Run this command from the repository root.")
}

scripts <- c(
  "1_prepare_data.R",
  "2_descriptive_tables.R",
  "3_interrupted_time_series.R",
  "4_population_estimation.R",
  "5_figures.R",
  "6_supplementary_tables.R"
)
rscript <- file.path(R.home("bin"), "Rscript")

for (script in scripts) {
  message("\nRunning ", script)
  status <- system2(rscript, script)
  if (status != 0) stop(script, " failed with status ", status)
}
message("\nFinished. See outputs/tables and outputs/figures.")
