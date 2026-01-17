# Run from repo root with: Rscript api/run_api.R
if (!requireNamespace("plumber", quietly = TRUE)) {
  stop("Package 'plumber' is required. Install with install.packages('plumber').")
}
pr <- plumber::plumb("api/plumber.R")
pr$run(host = "0.0.0.0", port = 8000)
