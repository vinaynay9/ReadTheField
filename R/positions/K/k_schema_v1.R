# K v1 Target Schema Validation
#
# Canonical definition of K v1 target schema to prevent schema drift.

get_k_v1_targets <- function() {
  c(
    "target_fg_attempts_k",
    "target_fg_made_k",
    "target_pat_made_k"
  )
}

validate_k_v1_target_schema <- function(data, strict = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Cannot validate K v1 schema: data is NULL or empty")
  }

  expected_targets <- get_k_v1_targets()
  actual_cols <- names(data)

  missing_targets <- setdiff(expected_targets, actual_cols)
  if (length(missing_targets) > 0) {
    stop("Missing K v1 target columns: ", paste(missing_targets, collapse = ", "),
         ". Expected targets: ", paste(expected_targets, collapse = ", "))
  }

  if (strict) {
    forbidden_targets <- c("target_carries", "target_receptions", "target_pass_attempts_qb")
    present_forbidden <- intersect(forbidden_targets, actual_cols)
    if (length(present_forbidden) > 0) {
      stop("K v1 schema violation: Found forbidden targets: ",
           paste(present_forbidden, collapse = ", "))
    }
  }

  invisible(NULL)
}

resolve_k_simulation_schema <- function(draws_df) {
  if (is.null(draws_df) || nrow(draws_df) == 0) {
    stop("Cannot resolve schema: draws_df is NULL or empty")
  }

  # Map target_* columns to canonical output names without changing values.
  if ("target_fg_made_k" %in% names(draws_df) && !"fg_made" %in% names(draws_df)) {
    draws_df$fg_made <- draws_df$target_fg_made_k
  }
  if ("target_fg_attempts_k" %in% names(draws_df) && !"fg_attempts" %in% names(draws_df)) {
    draws_df$fg_attempts <- draws_df$target_fg_attempts_k
  }
  if ("target_pat_made_k" %in% names(draws_df) && !"pat_made" %in% names(draws_df)) {
    draws_df$pat_made <- draws_df$target_pat_made_k
  }

  required_outputs <- c("fg_attempts", "fg_made", "pat_made")
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
