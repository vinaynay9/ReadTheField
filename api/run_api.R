# Run from repo root with: Rscript api/run_api.R
get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, cmd_args, value = TRUE)
  if (length(match) > 0) {
    return(normalizePath(sub(file_arg, "", match[1])))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
  stop("Unable to determine script path to set working directory.")
}

repo_root <- Sys.getenv("READTHEFIELD_REPO_ROOT", "")
if (!nzchar(repo_root)) {
  script_path <- get_script_path()
  repo_root <- normalizePath(file.path(dirname(script_path), ".."))
}
repo_root <- normalizePath(repo_root, mustWork = TRUE)
options(READTHEFIELD_REPO_ROOT = repo_root)
Sys.setenv(READTHEFIELD_REPO_ROOT = repo_root)

message("Resolved repo root: ", repo_root)
message("Working directory: ", normalizePath(getwd()))
cache_dir <- file.path(repo_root, "data", "cache")
processed_dir <- file.path(repo_root, "data", "processed")
if (!dir.exists(cache_dir)) {
  stop("Required cache directory missing: ", cache_dir)
}
if (!dir.exists(processed_dir)) {
  stop("Required processed directory missing: ", processed_dir)
}
message("Cache directory OK: ", cache_dir)
message("Processed directory OK: ", processed_dir)

if (!requireNamespace("plumber", quietly = TRUE)) {
  stop("Package 'plumber' is required. Install with install.packages('plumber').")
}
plumber_path <- file.path(repo_root, "api", "plumber.R")
pr <- plumber::plumb(plumber_path)
pr$run(host = "0.0.0.0", port = 8000)
