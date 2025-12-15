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
  
  if (is.null(result) || is.null(result$metadata)) {
    stop("Invalid result object provided")
  }
  
  metadata <- result$metadata
  diagnostics <- result$diagnostics
  defensive_context <- result$defensive_context
  
  # ============================================================================
  # Print header
  # ============================================================================
  
  cat("\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat("RB Simulation — Technical Diagnostics\n")
  cat("Player: ", metadata$player_name, " | Game: ", metadata$team, " vs ", 
      metadata$opponent, " (", as.character(metadata$game_date), ")\n", sep = "")
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  
  # ============================================================================
  # Print simulation parameters
  # ============================================================================
  
  cat("Simulation Parameters:\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("   Number of Monte Carlo simulations:", metadata$n_sims, "\n")
  cat("   Game ID:", metadata$game_id, "\n")
  cat("   Season:", metadata$season, "| Week:", metadata$week, "\n")
  cat("   Player ID:", metadata$player_id, "\n")
  cat("\n")
  
  # ============================================================================
  # Print training data information
  # ============================================================================
  
  cat("Training Data Used:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (!is.null(diagnostics$training_data_range)) {
    train_info <- diagnostics$training_data_range
    cat("   Date range:", as.character(train_info$min_date), "to", 
        as.character(train_info$max_date), "\n")
    cat("   Seasons included:", paste(sort(train_info$seasons), collapse = ", "), "\n")
    cat("   Number of unique players:", train_info$n_players, "\n")
    cat("   Number of unique games:", train_info$n_games, "\n")
  } else {
    cat("   Training data information not available\n")
  }
  
  cat("\n")
  
  # ============================================================================
  # Print model information
  # ============================================================================
  
  cat("Model Information:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (!is.null(diagnostics$model_info)) {
    model_info <- diagnostics$model_info
    
    # Print model types
    if ("carries_model" %in% names(model_info)) {
      cat("   Carries model:", model_info$carries_model, "\n")
    }
    if ("rush_yards_model" %in% names(model_info)) {
      cat("   Rush yards model:", model_info$rush_yards_model, "\n")
    }
    if ("rush_tds_model" %in% names(model_info)) {
      cat("   Rush TDs model:", model_info$rush_tds_model, "\n")
    }
    if ("targets_model" %in% names(model_info)) {
      cat("   Targets model:", model_info$targets_model, "\n")
    }
    if ("receptions_model" %in% names(model_info)) {
      cat("   Receptions model:", model_info$receptions_model, "\n")
    }
    if ("rec_yards_model" %in% names(model_info)) {
      cat("   Receiving yards model:", model_info$rec_yards_model, "\n")
    }
    if ("rec_tds_model" %in% names(model_info)) {
      cat("   Receiving TDs model:", model_info$rec_tds_model, "\n")
    }
  } else {
    cat("   Model information not available\n")
  }
  
  # Print null models (if any)
  if (!is.null(diagnostics$null_models) && length(diagnostics$null_models) > 0) {
    cat("   WARNING: The following models failed to fit:\n")
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
    
    if (length(features$rush_yards_defensive) > 0) {
      cat("   Rush yards model includes:", 
          paste(features$rush_yards_defensive, collapse = ", "), "\n")
    } else {
      cat("   Rush yards model: No defensive features\n")
    }
    
    if (length(features$rush_tds_defensive) > 0) {
      cat("   Rush TDs model includes:", 
          paste(features$rush_tds_defensive, collapse = ", "), "\n")
    } else {
      cat("   Rush TDs model: No defensive features\n")
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
    cat("   Opponent rush yards allowed (avg):", 
        if (is.na(val)) "NA (missing)" else round(val, 1), "\n")
  }
  
  if ("opp_sacks_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_sacks_roll5
    cat("   Opponent sacks (avg):", 
        if (is.na(val)) "NA (missing)" else round(val, 1), "\n")
  }
  
  if ("opp_tfl_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_tfl_roll5
    cat("   Opponent tackles for loss (avg):", 
        if (is.na(val)) "NA (missing)" else round(val, 1), "\n")
  }
  
  if ("opp_points_allowed_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_points_allowed_roll5
    cat("   Opponent points allowed (avg):", 
        if (is.na(val)) "NA (missing)" else round(val, 1), "\n")
  }
  
  cat("\n")
  
  # ============================================================================
  # Print distribution statistics
  # ============================================================================
  
  cat("Simulation Distribution Statistics:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (!is.null(diagnostics$distribution_stats)) {
    dist_stats <- diagnostics$distribution_stats
    cat("   Carries - Mean:", round(dist_stats$carries_mean, 2), 
        ", SD:", round(dist_stats$carries_sd, 2), "\n")
    cat("   Rush yards - Mean:", round(dist_stats$rush_yards_mean, 2),
        ", SD:", round(dist_stats$rush_yards_sd, 2), "\n")
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
}

