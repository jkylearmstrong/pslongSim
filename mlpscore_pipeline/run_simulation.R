# Ensure we propagate the correct parameter column from the Excel file
xls_path <- "simulation_params.xlsx"
if (file.exists(xls_path)) {
  # Read column names from Excel to inform the user
  cols <- colnames(readxl::read_excel(xls_path, n_max = 1))
  param_cols <- setdiff(cols, c("Parameter description", "Parameter"))
  
  # Determine active column
  active_col <- getOption("MLPScore.value_col", "Value")
  
  cat("========================================================\n")
  cat("Detected parameter columns in ", xls_path, ":\n", sep = "")
  for (col in param_cols) {
    prefix <- if (col == active_col) " -> " else "    "
    cat(prefix, "[", col, "]", if (col == active_col) " (ACTIVE)" else "", "\n", sep = "")
  }
  cat("========================================================\n")
  
  if (!active_col %in% param_cols) {
    warning("Active column '", active_col, "' not found in Excel sheets. Falling back to default 'Value'.")
    active_col <- "Value"
  }
  
  # Propagate via environment variables to targets callr/crew workers
  Sys.setenv(MLPSCORE_VALUE_COL = active_col)
}

# Ensure MLPScore is documented and installed locally
message("Documenting and installing package locally...")
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::document()
  devtools::install(upgrade = FALSE, quick = TRUE)
} else {
  if (requireNamespace("roxygen2", quietly = TRUE)) {
    roxygen2::roxygenize()
  }
  install.packages(".", repos = NULL, type = "source")
}

# Run the targets pipeline
targets::tar_make()

# Inform the user where the PDF reports are
cat("\n========================================================\n")
cat("Pipeline execution completed!\n")
cat("Generated PDF report:\n")
cat("  - Main Analysis Report:  ", file.path(getwd(), "analysis/analysis.pdf"), "\n")
cat("\n")
cat("Additional analyses are available as package vignettes:\n")
cat("  browseVignettes('MLPScore')\n")
cat("========================================================\n\n")