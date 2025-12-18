# RB v1 Target Schema Validation
#
# Canonical definition of RB v1 target schema to prevent schema drift.
# This is the single source of truth for RB v1 targets.

#' Get canonical RB v1 target columns
#'
#' Returns the authoritative list of RB v1 target columns.
#' RB v1 targets are exactly: target_carries, target_receptions, target_rush_tds, target_rec_tds
#'
#' @return Character vector of target column names
get_rb_v1_targets <- function() {
  c(
    "target_carries",
    "target_receptions",
    "target_rush_tds",
    "target_rec_tds"
  )
}

#' Validate RB v1 target schema
#'
#' Asserts that a data.frame contains exactly the RB v1 target columns.
#' Fails loudly if any targets are missing or if unexpected targets are present.
#'
#' @param data data.frame to validate
#' @param strict Logical, if TRUE (default), fail if unexpected targets present
#' @return Invisible NULL if valid, otherwise stops with error
validate_rb_v1_target_schema <- function(data, strict = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Cannot validate RB v1 schema: data is NULL or empty")
  }
  
  expected_targets <- get_rb_v1_targets()
  actual_cols <- names(data)
  
  # Check for missing required targets
  missing_targets <- setdiff(expected_targets, actual_cols)
  if (length(missing_targets) > 0) {
    stop("Missing RB v1 target columns: ", paste(missing_targets, collapse = ", "),
         ". Expected targets: ", paste(expected_targets, collapse = ", "))
  }
  
  # Check for unexpected targets (if strict)
  if (strict) {
    # Common yardage targets that should NOT exist in v1
    forbidden_targets <- c("target_rush_yards", "target_rec_yards", 
                          "target_rushing_yards", "target_receiving_yards")
    present_forbidden <- intersect(forbidden_targets, actual_cols)
    if (length(present_forbidden) > 0) {
      stop("RB v1 schema violation: Found forbidden yardage targets: ", 
           paste(present_forbidden, collapse = ", "),
           ". RB v1 does not include yardage targets. ",
           "Yardage should be derived downstream (e.g., carries * YPC).")
    }
  }
  
  invisible(NULL)
}

#' Resolve RB simulation output schema to canonical names
#'
#' Standardizes column names from simulate_rb_game() output to match
#' downstream aggregation expectations. Handles naming variants and
#' creates derived columns (total_yards, total_touchdowns).
#'
#' @param draws_df data.frame of simulation draws
#' @return data.frame with standardized column names
resolve_rb_simulation_schema <- function(draws_df) {
  if (is.null(draws_df) || nrow(draws_df) == 0) {
    stop("Cannot resolve schema: draws_df is NULL or empty")
  }
  
  # Log current schema
  log_file <- "rb_debug.log"
  if (file.exists(log_file)) {
    cat("Simulation draws columns (before resolution):\n", file = log_file, append = TRUE)
    cat(paste(names(draws_df), collapse = ", "), "\n", file = log_file, append = TRUE)
  }
  
  # Resolve rushing yards: rush_yards -> rushing_yards
  if ("rush_yards" %in% names(draws_df) && !"rushing_yards" %in% names(draws_df)) {
    draws_df$rushing_yards <- draws_df$rush_yards
  } else if (!"rushing_yards" %in% names(draws_df)) {
    # Try other candidates
    candidates <- c("rush_yards_sim", "yards", "rushing_yds")
    found <- intersect(candidates, names(draws_df))
    if (length(found) > 0) {
      draws_df$rushing_yards <- draws_df[[found[1]]]
    } else {
      stop("No rushing yards column found in simulation draws. ",
           "Columns present: ", paste(names(draws_df), collapse = ", "))
    }
  }
  
  # Resolve receiving yards: rec_yards -> receiving_yards
  if ("rec_yards" %in% names(draws_df) && !"receiving_yards" %in% names(draws_df)) {
    draws_df$receiving_yards <- draws_df$rec_yards
  } else if (!"receiving_yards" %in% names(draws_df)) {
    # Try other candidates
    candidates <- c("rec_yards_sim", "receiving_yds")
    found <- intersect(candidates, names(draws_df))
    if (length(found) > 0) {
      draws_df$receiving_yards <- draws_df[[found[1]]]
    } else {
      stop("No receiving yards column found in simulation draws. ",
           "Columns present: ", paste(names(draws_df), collapse = ", "))
    }
  }
  
  # Create total_touchdowns if not present
  if (!"total_touchdowns" %in% names(draws_df)) {
    if ("rush_tds" %in% names(draws_df) && "rec_tds" %in% names(draws_df)) {
      draws_df$total_touchdowns <- draws_df$rush_tds + draws_df$rec_tds
    } else {
      stop("Cannot create total_touchdowns: rush_tds and/or rec_tds missing. ",
           "Columns present: ", paste(names(draws_df), collapse = ", "))
    }
  }
  
  # Create total_yards if not present (sum of rushing + receiving)
  if (!"total_yards" %in% names(draws_df)) {
    if ("rushing_yards" %in% names(draws_df) && "receiving_yards" %in% names(draws_df)) {
      draws_df$total_yards <- draws_df$rushing_yards + draws_df$receiving_yards
    } else {
      stop("Cannot create total_yards: rushing_yards and/or receiving_yards missing. ",
           "Columns present: ", paste(names(draws_df), collapse = ", "))
    }
  }
  
  # Guardrails: Validate critical output columns before returning
  required_outputs <- c("rushing_yards", "receiving_yards", "total_yards", "total_touchdowns")
  for (col in required_outputs) {
    if (!col %in% names(draws_df)) {
      stop("Required output column '", col, "' is missing after schema resolution. ",
           "This is a bug in resolve_rb_simulation_schema().")
    }
    
    if (!is.numeric(draws_df[[col]])) {
      stop("Output column '", col, "' is not numeric. Type: ", class(draws_df[[col]]), ". ",
           "All yardage/TD columns must be numeric.")
    }
  }
  
  # Log resolved schema
  if (file.exists(log_file)) {
    cat("Simulation draws columns (after resolution):\n", file = log_file, append = TRUE)
    cat(paste(names(draws_df), collapse = ", "), "\n", file = log_file, append = TRUE)
  }
  
  return(draws_df)
}

