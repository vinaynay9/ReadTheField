# TE v1 Target Schema Validation
#
# Canonical definition of TE v1 target schema to prevent schema drift.

#' Get canonical TE v1 target columns
get_te_v1_targets <- function() {
  c(
    "target_targets",
    "target_receptions",
    "target_rec_yards",
    "target_rec_tds"
  )
}

#' Validate TE v1 target schema
validate_te_v1_target_schema <- function(data, strict = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Cannot validate TE v1 schema: data is NULL or empty")
  }

  expected_targets <- get_te_v1_targets()
  actual_cols <- names(data)

  missing_targets <- setdiff(expected_targets, actual_cols)
  if (length(missing_targets) > 0) {
    stop("Missing TE v1 target columns: ", paste(missing_targets, collapse = ", "),
         ". Expected targets: ", paste(expected_targets, collapse = ", "))
  }

  if (strict) {
    forbidden_targets <- c("target_carries", "target_rush_tds", "target_rush_yards", "target_rushing_yards")
    present_forbidden <- intersect(forbidden_targets, actual_cols)
    if (length(present_forbidden) > 0) {
      stop("TE v1 schema violation: Found forbidden rushing targets: ",
           paste(present_forbidden, collapse = ", "))
    }
  }

  invisible(NULL)
}

#' Resolve TE simulation output schema to canonical names
resolve_te_simulation_schema <- function(draws_df) {
  if (is.null(draws_df) || nrow(draws_df) == 0) {
    stop("Cannot resolve schema: draws_df is NULL or empty")
  }

  if ("rec_yards" %in% names(draws_df) && !"receiving_yards" %in% names(draws_df)) {
    draws_df$receiving_yards <- draws_df$rec_yards
  }

  if ("rec_tds" %in% names(draws_df) && !"receiving_tds" %in% names(draws_df)) {
    draws_df$receiving_tds <- draws_df$rec_tds
  }

  if (!"total_touchdowns" %in% names(draws_df)) {
    if ("rec_tds" %in% names(draws_df)) {
      draws_df$total_touchdowns <- draws_df$rec_tds
    } else if ("receiving_tds" %in% names(draws_df)) {
      draws_df$total_touchdowns <- draws_df$receiving_tds
    } else {
      stop("Cannot create total_touchdowns: rec_tds missing.")
    }
  }

  if (!"total_yards" %in% names(draws_df)) {
    if ("receiving_yards" %in% names(draws_df)) {
      draws_df$total_yards <- draws_df$receiving_yards
    } else {
      stop("Cannot create total_yards: receiving_yards missing.")
    }
  }

  required_outputs <- c("receiving_yards", "total_yards", "total_touchdowns")
  for (col in required_outputs) {
    if (!col %in% names(draws_df)) {
      stop("Required output column '", col, "' is missing after schema resolution.")
    }
    if (!is.numeric(draws_df[[col]])) {
      stop("Output column '", col, "' is not numeric. Type: ", class(draws_df[[col]]), ".")
    }
  }

  return(draws_df)
}
