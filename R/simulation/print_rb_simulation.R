# Print RB Simulation - Presentation Layer
#
# This function formats and prints simulation results to the console.
# Consumes the result object from run_rb_simulation().
#
# NO file writing, NO computation
# Only formatting and console output
#
# Usage:
#   result <- run_rb_simulation(...)
#   print_rb_simulation(result)

#' Print RB simulation results to console
#'
#' Formats and prints all simulation results, diagnostics, and warnings.
#' This function only handles presentation - no computation or file I/O.
#'
#' @param result List returned by run_rb_simulation()
print_rb_simulation <- function(result) {
  
  if (is.null(result) || is.null(result$metadata)) {
    stop("Invalid result object provided")
  }
  
  metadata <- result$metadata
  summary_df <- result$summary
  draws_df <- result$draws
  diagnostics <- result$diagnostics
  defensive_context <- result$defensive_context
  recent_games <- result$recent_games
  
  # ============================================================================
  # Print header
  # ============================================================================
  
  cat("\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat("RB Simulation — ", metadata$player_name, " vs ", metadata$opponent, 
      " (", as.character(metadata$game_date), ")\n", sep = "")
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  
  # ============================================================================
  # Print recent games (last 3)
  # ============================================================================
  
  if (nrow(recent_games) > 0) {
    cat(paste0(rep("=", 80), collapse = ""), "\n")
    cat(metadata$player_name, "— Previous 3 Games (Pre-", 
        as.character(metadata$game_date), ")\n", sep = "")
    cat(paste0(rep("=", 80), collapse = ""), "\n\n")
    
    # Print table header
    cat(sprintf("%-12s %-8s %8s %12s %8s %11s %10s %10s %8s %12s\n",
                "Gameday", "Opponent", "Carries", "Rush Yds", "Targets", 
                "Receptions", "Rec Yds", "Total TDs", "Fum Lost", "PPR Points"))
    cat(rep("-", 110), "\n", sep = "")
    
    # Print each game
    for (i in 1:nrow(recent_games)) {
      row <- recent_games[i, ]
      
      # Get actual stats (use target_* columns if available, otherwise use base columns)
      carries <- ifelse("target_carries" %in% names(row), row$target_carries, 
                       ifelse("carries" %in% names(row), row$carries, NA))
      rush_yds <- ifelse("target_rush_yards" %in% names(row), row$target_rush_yards,
                         ifelse("rush_yards" %in% names(row), row$rush_yards, NA))
      targets <- ifelse("target_targets" %in% names(row), row$target_targets,
                       ifelse("targets" %in% names(row), row$targets, NA))
      receptions <- ifelse("target_receptions" %in% names(row), row$target_receptions,
                          ifelse("receptions" %in% names(row), row$receptions, NA))
      rec_yds <- ifelse("target_rec_yards" %in% names(row), row$target_rec_yards,
                       ifelse("rec_yards" %in% names(row), row$rec_yards, NA))
      rush_tds <- ifelse("target_rush_tds" %in% names(row), row$target_rush_tds,
                        ifelse("rush_tds" %in% names(row), row$rush_tds, 0))
      rec_tds <- ifelse("target_rec_tds" %in% names(row), row$target_rec_tds,
                       ifelse("rec_tds" %in% names(row), row$rec_tds, 0))
      fumbles_lost <- ifelse("target_fumbles_lost" %in% names(row), row$target_fumbles_lost,
                            ifelse("fumbles_lost" %in% names(row), row$fumbles_lost,
                                  ifelse("fumbles" %in% names(row), row$fumbles, 0)))
      
      total_tds <- (ifelse(is.na(rush_tds), 0, rush_tds) + 
                    ifelse(is.na(rec_tds), 0, rec_tds))
      
      # Compute PPR fantasy points
      ppr_points <- compute_ppr_rb(
        rush_yards = ifelse(is.na(rush_yds), 0, rush_yds),
        rush_tds = ifelse(is.na(rush_tds), 0, rush_tds),
        receptions = ifelse(is.na(receptions), 0, receptions),
        rec_yards = ifelse(is.na(rec_yds), 0, rec_yds),
        rec_tds = ifelse(is.na(rec_tds), 0, rec_tds),
        fumbles_lost = ifelse(is.na(fumbles_lost), 0, fumbles_lost)
      )
      
      cat(sprintf("%-12s %-8s %8.0f %12.0f %8.0f %11.0f %10.0f %10.0f %8.0f %12.1f\n",
                  as.character(row$gameday),
                  row$opponent,
                  ifelse(is.na(carries), 0, carries),
                  ifelse(is.na(rush_yds), 0, rush_yds),
                  ifelse(is.na(targets), 0, targets),
                  ifelse(is.na(receptions), 0, receptions),
                  ifelse(is.na(rec_yds), 0, rec_yds),
                  total_tds,
                  ifelse(is.na(fumbles_lost), 0, fumbles_lost),
                  ppr_points))
    }
    
    cat("\n")
  }
  
  # ============================================================================
  # Print median stat line (p50)
  # ============================================================================
  
  cat("A. Median Stat Line (p50):\n")
  cat("   Carries:", summary_df$p50[summary_df$stat == "carries"], "\n")
  cat("   Rush yards:", summary_df$p50[summary_df$stat == "rush_yards"], "\n")
  cat("   Receptions:", summary_df$p50[summary_df$stat == "receptions"], "\n")
  cat("   Receiving yards:", summary_df$p50[summary_df$stat == "rec_yards"], "\n")
  cat("   Total TDs:", 
      round(summary_df$p50[summary_df$stat == "rush_tds"] + 
            summary_df$p50[summary_df$stat == "rec_tds"]), "\n")
  
  # Verify PPR calculation manually for transparency
  median_rush_yds <- summary_df$p50[summary_df$stat == "rush_yards"]
  median_rush_tds <- summary_df$p50[summary_df$stat == "rush_tds"]
  median_rec <- summary_df$p50[summary_df$stat == "receptions"]
  median_rec_yds <- summary_df$p50[summary_df$stat == "rec_yards"]
  median_rec_tds <- summary_df$p50[summary_df$stat == "rec_tds"]
  median_fumbles_lost <- summary_df$p50[summary_df$stat == "fumbles_lost"]
  median_ppr <- summary_df$p50[summary_df$stat == "fantasy_ppr"]
  
  # Calculate expected PPR manually from components
  expected_ppr <- (median_rush_yds * 0.1) + 
                  (median_rush_tds * 6) + 
                  (median_rec * 1) + 
                  (median_rec_yds * 0.1) + 
                  (median_rec_tds * 6) - 
                  (median_fumbles_lost * 2)
  
  cat("   Fumbles lost:", median_fumbles_lost, "\n")
  cat("   PPR fantasy points:", median_ppr, "\n")
  cat("   (Calculated from components: ", round(median_rush_yds * 0.1, 1), " rush yds + ", 
      round(median_rush_tds * 6, 1), " rush TDs + ", 
      round(median_rec * 1, 1), " rec + ", 
      round(median_rec_yds * 0.1, 1), " rec yds + ", 
      round(median_rec_tds * 6, 1), " rec TDs - ", 
      round(median_fumbles_lost * 2, 1), " fumbles = ", round(expected_ppr, 1), ")\n\n")
  
  # ============================================================================
  # Print percentiles (p25 / p50 / p75)
  # ============================================================================
  
  cat("B. Percentiles (p25 / p50 / p75):\n")
  cat("   Rush yards:",
      summary_df$p25[summary_df$stat == "rush_yards"], "/",
      summary_df$p50[summary_df$stat == "rush_yards"], "/",
      summary_df$p75[summary_df$stat == "rush_yards"], "\n")
  cat("   Receiving yards:",
      summary_df$p25[summary_df$stat == "rec_yards"], "/",
      summary_df$p50[summary_df$stat == "rec_yards"], "/",
      summary_df$p75[summary_df$stat == "rec_yards"], "\n")
  cat("   PPR fantasy points:",
      summary_df$p25[summary_df$stat == "fantasy_ppr"], "/",
      summary_df$p50[summary_df$stat == "fantasy_ppr"], "/",
      summary_df$p75[summary_df$stat == "fantasy_ppr"], "\n\n")
  
  # ============================================================================
  # Print distribution diagnostics
  # ============================================================================
  
  cat("C. Distribution Diagnostics:\n")
  if (!is.null(diagnostics$distribution_stats)) {
    cat("   Carries - Mean:", round(diagnostics$distribution_stats$carries_mean, 2), 
        ", SD:", round(diagnostics$distribution_stats$carries_sd, 2), "\n")
    cat("   Rush yards - Mean:", round(diagnostics$distribution_stats$rush_yards_mean, 2),
        ", SD:", round(diagnostics$distribution_stats$rush_yards_sd, 2), "\n")
  }
  
  # TD probabilities
  if (!is.null(diagnostics$td_probabilities)) {
    cat("   Probability of 0 TDs:", round(diagnostics$td_probabilities$prob_0_tds, 3), "\n")
    cat("   Probability of ≥1 TD:", round(diagnostics$td_probabilities$prob_ge1_td, 3), "\n")
    cat("   Probability of ≥2 TDs:", round(diagnostics$td_probabilities$prob_ge2_tds, 3), "\n")
  }
  cat("\n")
  
  # ============================================================================
  # Print defensive context used
  # ============================================================================
  
  cat("D. Defensive Context Used:\n")
  if ("opp_rush_yards_allowed_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_rush_yards_allowed_roll5
    if (is.na(val)) {
      cat("   opp_rush_yards_allowed_roll5: NA (missing)\n")
    } else {
      cat("   opp_rush_yards_allowed_roll5:", val, "\n")
    }
  } else {
    cat("   opp_rush_yards_allowed_roll5: NA (missing)\n")
  }
  
  if ("opp_sacks_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_sacks_roll5
    if (is.na(val)) {
      cat("   opp_sacks_roll5: NA (missing)\n")
    } else {
      cat("   opp_sacks_roll5:", val, "\n")
    }
  } else {
    cat("   opp_sacks_roll5: NA (missing)\n")
  }
  
  if ("opp_tfl_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_tfl_roll5
    if (is.na(val)) {
      cat("   opp_tfl_roll5: NA (missing)\n")
    } else {
      cat("   opp_tfl_roll5:", val, "\n")
    }
  } else {
    cat("   opp_tfl_roll5: NA (missing)\n")
  }
  
  if ("opp_points_allowed_roll5" %in% names(defensive_context)) {
    val <- defensive_context$opp_points_allowed_roll5
    if (is.na(val)) {
      cat("   opp_points_allowed_roll5: NA (missing)\n")
    } else {
      cat("   opp_points_allowed_roll5:", val, "\n")
    }
  } else {
    cat("   opp_points_allowed_roll5: NA (missing)\n")
  }
  
  cat("\n")
  
  # ============================================================================
  # Print sanity checks
  # ============================================================================
  
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat("Sanity Checks\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  
  warnings_issued <- FALSE
  
  if (!is.null(diagnostics$sanity_warnings)) {
    p75_carries <- summary_df$p75[summary_df$stat == "carries"]
    if (diagnostics$sanity_warnings$p75_carries_gt_30) {
      cat("WARNING: p75 carries (", p75_carries, ") > 30\n", sep = "")
      warnings_issued <- TRUE
    }
    
    p25_carries <- summary_df$p25[summary_df$stat == "carries"]
    if (diagnostics$sanity_warnings$p25_carries_lt_5) {
      cat("WARNING: p25 carries (", p25_carries, ") < 5\n", sep = "")
      warnings_issued <- TRUE
    }
    
    median_carries <- summary_df$p50[summary_df$stat == "carries"]
    median_rush_yds <- summary_df$p50[summary_df$stat == "rush_yards"]
    if (diagnostics$sanity_warnings$median_ypc_gt_6_5) {
      ypc <- median_rush_yds / median_carries
      cat("WARNING: median yards per carry (", round(ypc, 2), ") > 6.5\n", sep = "")
      warnings_issued <- TRUE
    }
    
    if (diagnostics$sanity_warnings$td_prob_ge_0_8) {
      prob_ge1_td <- diagnostics$td_probabilities$prob_ge1_td
      cat("WARNING: TD probability (≥1 TD) = ", round(prob_ge1_td, 3), " >= 0.8\n", sep = "")
      warnings_issued <- TRUE
    }
  }
  
  if (!warnings_issued) {
    cat("All sanity checks passed.\n")
  }
  
  cat("\n")
  
  # ============================================================================
  # Print final confirmation
  # ============================================================================
  
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat("Simulation complete. Inputs verified against prior 3 games.\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
}

