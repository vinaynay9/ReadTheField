# Print RB Simulation - Technical/Process Diagnostics
#
# This function prints technical details about the simulation process:
# - Number of simulations run
# - Training data details (which players, games, date ranges)
# - Model information
# - Defensive features used
# - Process diagnostics
#
# NO player-specific stat projections
# Focus: How was this simulation run?

#' Print RB simulation technical diagnostics
#'
#' Formats and prints technical/process details about the simulation:
#' training data, model info, number of simulations, defensive features.
#' Does NOT include player-specific stat projections.
#'
#' @param result List returned by run_rb_simulation()
print_rb_simulation <- function(result) {
  
  # Wrap entire function in tryCatch to prevent crashes
  tryCatch({
    # Safe helper functions
    safe_num <- function(x, digits = 2, default = "NA") {
      if (is.null(x) || length(x) == 0) return(default)
      if (is.na(x) || !is.numeric(x)) return(default)
      if (length(x) != 1) return(default)
      tryCatch({
        round(x, digits)
      }, error = function(e) {
        default
      })
    }
    
    safe_str <- function(x, default = "(missing)") {
      if (is.null(x) || length(x) == 0) return(default)
      if (is.na(x)) return(default)
      tryCatch({
        as.character(x)
      }, error = function(e) {
        default
      })
    }
    
    safe_interval <- function(p25, p50, p75, digits = 1) {
      p25_str <- safe_num(p25, digits = digits)
      p50_str <- safe_num(p50, digits = digits)
      p75_str <- safe_num(p75, digits = digits)
      paste(p25_str, "/", p50_str, "/", p75_str)
    }
    
    if (is.null(result) || is.null(result$metadata)) {
      cat("ERROR: Invalid result object provided\n")
      return(invisible(NULL))
    }
  
  metadata <- result$metadata
  diagnostics <- if (!is.null(result$diagnostics)) result$diagnostics else list()
  defensive_context <- if (!is.null(result$defensive_context)) result$defensive_context else list()
  
  # ============================================================================
  # Print header
  # ============================================================================
  
  cat("\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat("RB Simulation — Technical Diagnostics\n")
  cat("Player: ", safe_str(metadata$player_name, default = "(unknown)"), 
      " | Game: ", safe_str(metadata$team, default = "(unknown)"), 
      " vs ", safe_str(metadata$opponent, default = "(unknown)"), 
      " (", safe_str(metadata$game_date, default = "(unknown)"), ")\n", sep = "")
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  
  # ============================================================================
  # Print simulation parameters
  # ============================================================================
  
  cat("Simulation Parameters:\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("   Number of Monte Carlo simulations:", safe_num(metadata$n_sims, digits = 0, default = "N/A"), "\n")
  cat("   Game ID:", safe_str(metadata$game_id), "\n")
  cat("   Season:", safe_str(metadata$season), "| Week:", safe_str(metadata$week), "\n")
  cat("   Player ID:", safe_str(metadata$player_id), "\n")
  cat("\n")
  
  # ============================================================================
  # Print training data information
  # ============================================================================
  
  cat("Training Data Used:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (!is.null(diagnostics$training_data_range)) {
    train_info <- diagnostics$training_data_range
    min_date_str <- if (!is.null(train_info$min_date) && !is.na(train_info$min_date)) {
      as.character(train_info$min_date)
    } else {
      "(missing)"
    }
    max_date_str <- if (!is.null(train_info$max_date) && !is.na(train_info$max_date)) {
      as.character(train_info$max_date)
    } else {
      "(missing)"
    }
    cat("   Date range:", min_date_str, "to", max_date_str, "\n")
    
    seasons_str <- if (!is.null(train_info$seasons) && length(train_info$seasons) > 0) {
      paste(sort(train_info$seasons), collapse = ", ")
    } else {
      "(missing)"
    }
    cat("   Seasons included:", seasons_str, "\n")
    cat("   Number of unique players:", safe_num(train_info$n_players, digits = 0, default = "N/A"), "\n")
    cat("   Number of unique games:", safe_num(train_info$n_games, digits = 0, default = "N/A"), "\n")
  } else {
    cat("   Training data information not available\n")
  }
  
  cat("\n")
  
  # ============================================================================
  # Print model information
  # ============================================================================
  
  cat("Model Information:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  # Print model diagnostics (new format with per-target diagnostics)
  if (!is.null(diagnostics$model_diagnostics)) {
    model_diag <- diagnostics$model_diagnostics
    cat("   Model fitting diagnostics:\n")
    
    targets <- c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns")
    for (target in targets) {
      if (target %in% names(model_diag)) {
        diag <- model_diag[[target]]
        fit_type <- if (!is.null(diag$fit_type)) diag$fit_type else "(unknown)"
        n_rows <- safe_num(diag$n_rows_used, digits = 0, default = "N/A")
        cat("     ", target, ": ", fit_type, " (", n_rows, " rows)\n", sep = "")
      }
    }
  } else if (!is.null(diagnostics$model_info)) {
    # Legacy format
    model_info <- diagnostics$model_info
    if ("carries_model" %in% names(model_info)) {
      cat("   Carries model:", safe_str(model_info$carries_model), "\n")
    }
    if ("rush_yards_model" %in% names(model_info)) {
      cat("   Rush yards model:", safe_str(model_info$rush_yards_model), "\n")
    }
    if ("receptions_model" %in% names(model_info)) {
      cat("   Receptions model:", safe_str(model_info$receptions_model), "\n")
    }
  } else {
    cat("   Model information not available\n")
  }
  
  # Print baseline models (if any)
  if (!is.null(diagnostics$baseline_models) && length(diagnostics$baseline_models) > 0) {
    cat("   WARNING: The following models fell back to baseline:\n")
    for (model_name in diagnostics$baseline_models) {
      cat("     -", model_name, "\n")
    }
  }
  
  # Print null models (if any) - legacy check
  if (!is.null(diagnostics$null_models) && length(diagnostics$null_models) > 0) {
    cat("   WARNING: The following models are NULL:\n")
    for (model_name in diagnostics$null_models) {
      cat("     -", model_name, "\n")
    }
  }
  
  cat("\n")
  
  # ============================================================================
  # Print defensive features used
  # ============================================================================
  
  cat("Defensive Features Used in Models:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (!is.null(diagnostics$model_features)) {
    features <- diagnostics$model_features
    
    if (!is.null(features$rushing_yards_defensive) && length(features$rushing_yards_defensive) > 0) {
      cat("   Rush yards model includes:", 
          paste(features$rushing_yards_defensive, collapse = ", "), "\n")
    } else {
      cat("   Rush yards model: No defensive features\n")
    }
    
    # Check for other defensive features if they exist
    if (!is.null(features$rush_tds_defensive) && length(features$rush_tds_defensive) > 0) {
      cat("   Rush TDs model includes:", 
          paste(features$rush_tds_defensive, collapse = ", "), "\n")
    }
  } else {
    cat("   Defensive feature information not available\n")
  }
  
  cat("\n")
  
  # ============================================================================
  # Print defensive context values
  # ============================================================================
  
  cat("Defensive Context Values (Opponent's Last 5 Games):\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if ("opp_rush_yards_allowed_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_rush_yards_allowed_roll5
    cat("   Opponent rush yards allowed (avg):", safe_num(val, digits = 1, default = "(missing)"), "\n")
  } else {
    cat("   Opponent rush yards allowed (avg): (missing)\n")
  }
  
  if ("opp_sacks_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_sacks_roll5
    cat("   Opponent sacks (avg):", safe_num(val, digits = 1, default = "(missing)"), "\n")
  } else {
    cat("   Opponent sacks (avg): (missing)\n")
  }
  
  if ("opp_tfl_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_tfl_roll5
    cat("   Opponent tackles for loss (avg):", safe_num(val, digits = 1, default = "(missing)"), "\n")
  } else {
    cat("   Opponent tackles for loss (avg): (missing)\n")
  }
  
  if ("opp_points_allowed_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_points_allowed_roll5
    cat("   Opponent points allowed (avg):", safe_num(val, digits = 1, default = "(missing)"), "\n")
  } else {
    cat("   Opponent points allowed (avg): (missing)\n")
  }
  
  cat("\n")
  
  # ============================================================================
  # Print distribution statistics
  # ============================================================================
  
  cat("Simulation Distribution Statistics:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (!is.null(diagnostics$distribution_stats)) {
    dist_stats <- diagnostics$distribution_stats
    carries_mean <- safe_num(dist_stats$carries_mean, digits = 2, default = "N/A")
    carries_sd <- safe_num(dist_stats$carries_sd, digits = 2, default = "N/A")
    rush_yds_mean <- safe_num(dist_stats$rushing_yards_mean, digits = 2, default = "N/A")
    rush_yds_sd <- safe_num(dist_stats$rushing_yards_sd, digits = 2, default = "N/A")
    
    cat("   Carries - Mean:", carries_mean, ", SD:", carries_sd, "\n")
    cat("   Rush yards - Mean:", rush_yds_mean, ", SD:", rush_yds_sd, "\n")
  } else {
    cat("   Distribution statistics not available\n")
  }
  
  cat("\n")
  
  # ============================================================================
  # Print final confirmation
  # ============================================================================
  
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat("Technical diagnostics complete.\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  
  }, error = function(e) {
    cat("\nERROR: Printing diagnostics failed:", conditionMessage(e), "\n")
    cat("Result structure may be incomplete or corrupted.\n")
    cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  })
}

