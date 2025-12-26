# Print RB Simulation - Technical/Process Diagnostics
# TODO: Filename remains RB for legacy reasons; this printer is position-aware (RB/WR/TE).
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
  position <- if (!is.null(metadata$position) && length(metadata$position) > 0) metadata$position else "RB"
  position <- toupper(as.character(position))
  if (is.na(position) || position == "") position <- "RB"
  is_rb <- position == "RB"
  if (!is_rb && !exists("get_passing_defense_roll1_features")) {
    if (file.exists("R/positions/passing_defense_features.R")) {
      source("R/positions/passing_defense_features.R", local = TRUE)
    }
  }
  
  # ============================================================================
  # Print header
  # ============================================================================
  
  cat("\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat(paste0(position, " Simulation - Technical Diagnostics\n"))
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
  if (isTRUE(metadata$is_rookie)) {
    cat("   Rookie player detected: prior-season stats intentionally excluded\n")
  }
  if (isTRUE(metadata$current_season_in_progress)) {
    cat("   NOTE: Current season detected (in-progress). Rolling features may be incomplete.\n")
  }
  if (!is.null(diagnostics$availability)) {
    avail <- diagnostics$availability
    cat("   Availability policy:", safe_str(avail$policy, default = "(missing)"), "\n")
    cat("   Availability state:", safe_str(avail$state, default = "(missing)"), "\n")
    cat("   Counterfactual:", if (isTRUE(avail$counterfactual)) "YES" else "NO", "\n")
    if (isTRUE(metadata$counterfactual_mode)) {
      cat("   Counterfactual roster team:", safe_str(metadata$counterfactual_team, default = "(missing)"), "\n")
    }
    dropped_groups <- if (!is.null(avail$dropped_feature_groups) && length(avail$dropped_feature_groups) > 0) {
      paste(avail$dropped_feature_groups, collapse = ", ")
    } else {
      "none"
    }
    dropped_feats <- if (!is.null(avail$dropped_features) && length(avail$dropped_features) > 0) {
      avail$dropped_features
    } else {
      character(0)
    }
    cat("   Dropped feature groups:", dropped_groups, "\n")
    cat("   Dropped feature count:", length(dropped_feats), "\n")
  }
  if (!is.null(diagnostics$regime_selection)) {
    reg <- diagnostics$regime_selection
    cat("   Selected regime:", safe_str(reg$regime_selected, default = "(missing)"), "\n")
    if (isTRUE(reg$fallback_used)) {
      cat("   Availability policy fallback: counterfactual_prior regime selected\n")
      if (!is.null(reg$fallback_reason) && !is.na(reg$fallback_reason)) {
        cat("   Fallback reason:", safe_str(reg$fallback_reason, default = "(missing)"), "\n")
      }
    }
  }
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
  # Model inputs and selection (model_trace)
  # ============================================================================
  if (!is.null(diagnostics$model_trace)) {
    mt <- diagnostics$model_trace
    cat("Model Inputs and Selection:\n")
    cat(rep("-", 80), "\n", sep = "")
    cat("Season phase: ", safe_str(mt$phase, default = "(unknown)"), "\n", sep = "")
    
    if (!is.null(mt$targets) && length(mt$targets) > 0) {
      cat("\nTarget distributions:\n")
      for (nm in names(mt$targets)) {
        tt <- mt$targets[[nm]]
        cat("  ", nm, ": ", safe_str(tt$used_model, "(unknown)"),
            " (n=", safe_num(tt$n_train, digits = 0, default = "NA"),
            ", non-NA=", safe_num(tt$n_non_na, digits = 0, default = "NA"), ")\n", sep = "")
      }
    }
    
    if (!is.null(mt$features)) {
      feat <- mt$features
      cat("\nFeature inputs:\n")
      cat("  Total candidate features: ", safe_num(length(feat$candidate_features), digits = 0, default = "0"), "\n", sep = "")
      cat("  Used features: ", safe_num(length(feat$used_features), digits = 0, default = "0"), "\n", sep = "")
      dropped <- feat$dropped_features
      if (length(dropped) > 0) {
        cat("  Dropped (NA or unavailable): ", length(dropped), "\n    - ", paste(dropped, collapse = "\n    - "), "\n", sep = "")
      } else {
        cat("  Dropped (NA or unavailable): none\n")
      }
      if (!is.null(feat$na_features) && length(feat$na_features) > 0) {
        cat("  High-NA features (>90% NA): ", paste(feat$na_features, collapse = ", "), "\n", sep = "")
      }
    }
    
    cat("\n")
  }

  if (!is.null(diagnostics$feature_usage)) {
    fu <- diagnostics$feature_usage
    used_list <- if (!is.null(fu$used_features) && length(fu$used_features) > 0) {
      paste(fu$used_features, collapse = ", ")
    } else {
      "(none)"
    }
    cat("Final Model Features Used:\n")
    cat(rep("-", 80), "\n", sep = "")
    cat("  ", used_list, "\n", sep = "")
    cat("\n")
  }
  
  # ============================================================================
  # Print model information
  # ============================================================================
  
  cat("Model Information:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  # Print model diagnostics (per-target, regime-aware)
  if (!is.null(diagnostics$model_diagnostics)) {
    model_diag <- diagnostics$model_diagnostics
    cat("   Model fitting diagnostics:\n")

    resolve_targets <- function() {
      if (is_rb && exists("get_rb_v1_targets")) return(get_rb_v1_targets())
      if (!is_rb && position == "WR" && exists("get_wr_v1_targets")) return(get_wr_v1_targets())
      if (!is_rb && position == "TE" && exists("get_te_v1_targets")) return(get_te_v1_targets())
      NULL
    }

    resolve_model_key <- function(target, regime) {
      if (is_rb && exists("get_model_key")) return(get_model_key(target, regime))
      if (!is_rb && position == "WR" && exists("get_wr_model_key")) return(get_wr_model_key(target, regime))
      if (!is_rb && position == "TE" && exists("get_te_model_key")) return(get_te_model_key(target, regime))
      paste0(target, "__", regime)
    }

    targets <- resolve_targets()
    regime <- if (!is.null(diagnostics$regime_selection) &&
                  !is.null(diagnostics$regime_selection$regime_selected) &&
                  !is.na(diagnostics$regime_selection$regime_selected)) {
      diagnostics$regime_selection$regime_selected
    } else if (!is.null(diagnostics$model_trace) && !is.null(diagnostics$model_trace$phase)) {
      diagnostics$model_trace$phase
    } else {
      NA_character_
    }

    if (!is.null(targets) && !is.na(regime)) {
      for (target in targets) {
        model_key <- resolve_model_key(target, regime)
        if (model_key %in% names(model_diag)) {
          diag <- model_diag[[model_key]]
          fit_type <- if (!is.null(diag$fit_type)) diag$fit_type else "(unknown)"
          n_rows_val <- if (!is.null(diag$n_rows_final)) {
            diag$n_rows_final
          } else if (!is.null(diag$n_rows_used)) {
            diag$n_rows_used
          } else {
            NA_real_
          }
          n_rows <- safe_num(n_rows_val, digits = 0, default = "N/A")
          cat("     ", target, ": ", fit_type, " (", n_rows, " rows)\n", sep = "")
        }
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
  
  if (!is.null(diagnostics$defensive_features)) {
    def_diag <- diagnostics$defensive_features
    avail <- if (!is.null(def_diag$available)) def_diag$available else character(0)
    non_na <- if (!is.null(def_diag$non_na)) def_diag$non_na else character(0)
    cat("   Available defensive features: ", if (length(avail) > 0) paste(avail, collapse = ", ") else "(none)", "\n", sep = "")
    cat("   Non-NA defensive features: ", if (length(non_na) > 0) paste(non_na, collapse = ", ") else "(all NA)", "\n", sep = "")
  } else if (!is.null(diagnostics$model_features)) {
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
  
  # Offensive (QB-side) rolling context values
  cat("Offensive Context Values (QB Rolling, prior games only):\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("   Note: roll1 = most recent completed game before target; roll3/roll5 are prior-game averages.\n")

  off_context <- if (!is.null(result$offensive_context)) result$offensive_context else list()
  qb_rows <- list(
    "Pass Attempts (QB)" = c("target_pass_attempts_qb_roll1", "target_pass_attempts_qb_roll3", "target_pass_attempts_qb_roll5"),
    "Completion % (QB)" = c("target_completion_pct_qb_roll1", "target_completion_pct_qb_roll3", "target_completion_pct_qb_roll5"),
    "INTs Thrown (QB)" = c("target_interceptions_qb_thrown_roll1", "target_interceptions_qb_thrown_roll3", "target_interceptions_qb_thrown_roll5"),
    "Sacks Taken (QB)" = c("target_sacks_qb_taken_roll1", "target_sacks_qb_taken_roll3", "target_sacks_qb_taken_roll5")
  )
  raw_vals <- unlist(lapply(qb_rows, function(cols) lapply(cols, function(nm) off_context[[nm]])))
  raw_vals <- raw_vals[!vapply(raw_vals, is.null, logical(1))]
  if (length(raw_vals) > 0 && any(!vapply(raw_vals, function(v) is.numeric(v) || is.na(v), logical(1)))) {
    stop("PRINT ERROR: Offensive context values must be numeric before formatting.")
  }
  cat(sprintf("%-24s %10s %10s %10s\n", "Stat", "roll1", "roll3", "roll5"))
  cat(rep("-", 80), "\n", sep = "")
  format_ctx <- function(val) {
    if (is.null(val) || length(val) == 0 || is.na(val)) return("NA")
    as.character(format(round(as.numeric(val), 2), nsmall = 2, trim = TRUE))
  }
  for (lbl in names(qb_rows)) {
    cols <- qb_rows[[lbl]]
    vals <- vapply(cols, function(nm) {
      if (!is.null(off_context[[nm]])) format_ctx(off_context[[nm]]) else "NA"
    }, character(1))
    cat(sprintf("%-24s %10s %10s %10s\n", lbl, vals[1], vals[2], vals[3]))
  }
  cat("\n")

  # Defensive (defense-caused) rolling context values
  cat("Defensive Context Values (Rolling, prior games only):\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("   Note: roll1 = most recent completed game before target; roll3/roll5 are prior-game averages.\n")
  
  # Determine if defensive features should be available based on week
  week <- metadata$week
  def_available <- !is.null(week) && !is.na(week) && week >= 6
  na_reason <- if (!def_available) " (NA: early season, need 5 prior games)" else ""
  
  def_to_print <- if (is_rb) {
    c(
      "def_rush_yards_defense_allowed_roll1", "def_rush_yards_defense_allowed_roll5",
      "def_yards_per_rush_defense_allowed_roll1", "def_yards_per_rush_defense_allowed_roll5",
      "def_sacks_defense_forced_roll1", "def_sacks_defense_forced_roll5",
      "def_tackles_for_loss_defense_forced_roll1", "def_tackles_for_loss_defense_forced_roll5",
      "def_points_defense_allowed_roll1", "def_points_defense_allowed_roll5"
    )
  } else {
    c(
      get_passing_defense_roll1_features(),
      get_passing_defense_roll3_features(),
      get_passing_defense_roll5_features()
    )
  }
  for (nm in def_to_print) {
    label <- gsub("_", " ", nm, fixed = TRUE)
    if (nm %in% names(defensive_context)) {
      val <- defensive_context[[nm]]
      val_str <- if (is.na(val) && grepl("roll5$", nm) && !def_available) {
        paste0("NA", na_reason)
      } else {
        safe_num(val, digits = 2, default = "NA (data missing)")
      }
      cat("   ", label, ": ", val_str, "\n", sep = "")
    } else {
      cat("   ", label, ": (not computed)\n", sep = "")
    }
  }
  
  cat("\n")
  
  # ============================================================================
  # Print distribution statistics
  # ============================================================================
  
  cat("Simulation Distribution Statistics:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (!is.null(diagnostics$distribution_stats)) {
    dist_stats <- diagnostics$distribution_stats
    
    if (is_rb) {
      critical_stats <- c("carries_mean", "rushing_yards_mean", "receiving_yards_mean")
      for (stat in critical_stats) {
        if (is.null(dist_stats[[stat]])) {
          stop("PRINT ERROR: Distribution stat '", stat, "' is missing. ",
               "Cannot print distribution statistics. This indicates incomplete diagnostics.")
        }
        if (!is.numeric(dist_stats[[stat]])) {
          stop("PRINT ERROR: Distribution stat '", stat, "' is not numeric (type: ",
               class(dist_stats[[stat]]), "). Cannot print statistics.")
        }
      }
      carries_mean <- safe_num(dist_stats$carries_mean, digits = 2, default = "N/A")
      carries_sd <- safe_num(dist_stats$carries_sd, digits = 2, default = "N/A")
      rush_yds_mean <- safe_num(dist_stats$rushing_yards_mean, digits = 2, default = "N/A")
      rush_yds_sd <- safe_num(dist_stats$rushing_yards_sd, digits = 2, default = "N/A")
      rec_yds_mean <- safe_num(dist_stats$receiving_yards_mean, digits = 2, default = "N/A")
      rec_yds_sd <- safe_num(dist_stats$receiving_yards_sd, digits = 2, default = "N/A")
      cat("   Carries - Mean:", carries_mean, ", SD:", carries_sd, "\n")
      cat("   Rushing yards - Mean:", rush_yds_mean, ", SD:", rush_yds_sd, "\n")
      cat("   Receiving yards - Mean:", rec_yds_mean, ", SD:", rec_yds_sd, "\n")
    } else {
      critical_stats <- c("targets_mean", "receptions_mean", "receiving_yards_mean")
      for (stat in critical_stats) {
        if (is.null(dist_stats[[stat]])) {
          stop("PRINT ERROR: Distribution stat '", stat, "' is missing. ",
               "Cannot print distribution statistics. This indicates incomplete diagnostics.")
        }
        if (!is.numeric(dist_stats[[stat]])) {
          stop("PRINT ERROR: Distribution stat '", stat, "' is not numeric (type: ",
               class(dist_stats[[stat]]), "). Cannot print statistics.")
        }
      }
      targets_mean <- safe_num(dist_stats$targets_mean, digits = 2, default = "N/A")
      targets_sd <- safe_num(dist_stats$targets_sd, digits = 2, default = "N/A")
      receptions_mean <- safe_num(dist_stats$receptions_mean, digits = 2, default = "N/A")
      receptions_sd <- safe_num(dist_stats$receptions_sd, digits = 2, default = "N/A")
      rec_yds_mean <- safe_num(dist_stats$receiving_yards_mean, digits = 2, default = "N/A")
      rec_yds_sd <- safe_num(dist_stats$receiving_yards_sd, digits = 2, default = "N/A")
      cat("   Targets - Mean:", targets_mean, ", SD:", targets_sd, "\n")
      cat("   Receptions - Mean:", receptions_mean, ", SD:", receptions_sd, "\n")
      cat("   Receiving yards - Mean:", rec_yds_mean, ", SD:", rec_yds_sd, "\n")
    }
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

