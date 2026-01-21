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

script_path <- get_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(repo_root)
options(READTHEFIELD_REPO_ROOT = repo_root)

message("Resolved working directory: ", normalizePath(getwd()))
sanity_files <- c(
  "data/cache/player_directory.parquet",
  "data/cache/player_week_identity.parquet"
)
sanity_exists <- file.exists(sanity_files)
names(sanity_exists) <- sanity_files
message("Cache sanity check:")
for (path in names(sanity_exists)) {
  message("  ", path, ": ", ifelse(sanity_exists[[path]], "OK", "MISSING"))
}
if (any(!sanity_exists)) {
  warning(
    "Missing required caches. Run scripts/refresh_weekly_cache.R from repo root.",
    call. = FALSE
  )
}

if (!requireNamespace("plumber", quietly = TRUE)) {
  stop("Package 'plumber' is required. Install with install.packages('plumber').")
}
plumber_path <- file.path(repo_root, "api", "plumber.R")
pr <- plumber::plumb(plumber_path)
pr$run(host = "0.0.0.0", port = 8000)
