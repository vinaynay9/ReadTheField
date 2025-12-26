# QB v1 Target Schema Validation
#
# Canonical definition of QB v1 target schema to prevent schema drift.

#' Get canonical QB v1 target columns
get_qb_v1_targets <- function() {
  c(
    "target_pass_attempts_qb",
    "target_completions_qb",
    "target_pass_yards_qb",
    "target_pass_tds_qb",
    "target_interceptions_qb_thrown",
    "target_sacks_qb_taken",
    "target_qb_rush_attempts",
    "target_qb_rush_yards"
  )
}

#' Validate QB v1 target schema
validate_qb_v1_target_schema <- function(data, strict = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Cannot validate QB v1 schema: data is NULL or empty")
  }

  expected_targets <- get_qb_v1_targets()
  actual_cols <- names(data)

  missing_targets <- setdiff(expected_targets, actual_cols)
  if (length(missing_targets) > 0) {
    stop("Missing QB v1 target columns: ", paste(missing_targets, collapse = ", "),
         ". Expected targets: ", paste(expected_targets, collapse = ", "))
  }

  if (strict) {
    forbidden_targets <- c("target_carries", "target_receptions", "target_rec_yards", "target_rec_tds")
    present_forbidden <- intersect(forbidden_targets, actual_cols)
    if (length(present_forbidden) > 0) {
      stop("QB v1 schema violation: Found forbidden targets: ",
           paste(present_forbidden, collapse = ", "))
    }
  }

  invisible(NULL)
}

#' Resolve QB simulation output schema to canonical names
resolve_qb_simulation_schema <- function(draws_df) {
  if (is.null(draws_df) || nrow(draws_df) == 0) {
    stop("Cannot resolve schema: draws_df is NULL or empty")
  }

  # Map target_* columns to canonical output names without changing values.
  # This is a presentation schema step only; modeling remains unchanged.
  if ("target_pass_attempts_qb" %in% names(draws_df) && !"passing_attempts" %in% names(draws_df)) {
    draws_df$passing_attempts <- draws_df$target_pass_attempts_qb
  }
  if ("target_completions_qb" %in% names(draws_df) && !"passing_completions" %in% names(draws_df)) {
    draws_df$passing_completions <- draws_df$target_completions_qb
  }
  if ("target_pass_yards_qb" %in% names(draws_df) && !"passing_yards" %in% names(draws_df)) {
    draws_df$passing_yards <- draws_df$target_pass_yards_qb
  }
  if ("target_pass_tds_qb" %in% names(draws_df) && !"passing_tds" %in% names(draws_df)) {
    draws_df$passing_tds <- draws_df$target_pass_tds_qb
  }
  if ("target_interceptions_qb_thrown" %in% names(draws_df) && !"interceptions_thrown" %in% names(draws_df)) {
    draws_df$interceptions_thrown <- draws_df$target_interceptions_qb_thrown
  }
  if ("target_sacks_qb_taken" %in% names(draws_df) && !"qb_sacks_taken" %in% names(draws_df)) {
    draws_df$qb_sacks_taken <- draws_df$target_sacks_qb_taken
  }
  if ("target_qb_rush_attempts" %in% names(draws_df) && !"qb_rush_attempts" %in% names(draws_df)) {
    draws_df$qb_rush_attempts <- draws_df$target_qb_rush_attempts
  }
  if ("target_qb_rush_yards" %in% names(draws_df) && !"qb_rush_yards" %in% names(draws_df)) {
    draws_df$qb_rush_yards <- draws_df$target_qb_rush_yards
  }

  if ("pass_attempts" %in% names(draws_df) && !"passing_attempts" %in% names(draws_df)) {
    draws_df$passing_attempts <- draws_df$pass_attempts
  }
  if ("completions" %in% names(draws_df) && !"passing_completions" %in% names(draws_df)) {
    draws_df$passing_completions <- draws_df$completions
  }
  if ("pass_yards" %in% names(draws_df) && !"passing_yards" %in% names(draws_df)) {
    draws_df$passing_yards <- draws_df$pass_yards
  }
  if ("pass_tds" %in% names(draws_df) && !"passing_tds" %in% names(draws_df)) {
    draws_df$passing_tds <- draws_df$pass_tds
  }
  if ("interceptions" %in% names(draws_df) && !"interceptions_thrown" %in% names(draws_df)) {
    draws_df$interceptions_thrown <- draws_df$interceptions
  }
  if ("sacks_taken" %in% names(draws_df) && !"qb_sacks_taken" %in% names(draws_df)) {
    draws_df$qb_sacks_taken <- draws_df$sacks_taken
  }

  required_outputs <- c(
    "passing_attempts",
    "passing_completions",
    "passing_yards",
    "passing_tds",
    "interceptions_thrown",
    "qb_sacks_taken",
    "qb_rush_attempts",
    "qb_rush_yards"
  )
  for (col in required_outputs) {
    if (!col %in% names(draws_df)) {
      stop("Required output column '", col, "' is missing after schema resolution.")
    }
    if (!is.numeric(draws_df[[col]])) {
      stop("Output column '", col, "' is not numeric. Type: ", class(draws_df[[col]]), ".")
    }
  }

  draws_df
}
