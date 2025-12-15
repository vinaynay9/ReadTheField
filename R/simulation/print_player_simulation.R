# Print Player Simulation - Player-Specific Results
#
# This function prints clean, player-focused simulation results:
# - Estimated stats (median, percentiles)
# - Confidence intervals
# - Recent game history
# - Fantasy point projections
#
# NO technical details, NO model diagnostics
# Focus: What will this player do?
#
# Dependencies:
#   - R/utils/ppr_scoring.R (for compute_ppr_rb)

#' Print player simulation results - clean stats-focused output
#'
#' Formats and prints player-specific simulation results:
#' estimated stats, confidence intervals, percentiles, recent games.
#' Does NOT include technical/process details.
#'
#' @param result List returned by run_rb_simulation() or simulate_player_game()
print_player_simulation <- function(result) {
  
  if (is.null(result) || is.null(result$metadata)) {
    stop("Invalid result object provided")
  }
  
  metadata <- result$metadata
  summary_df <- result$summary
  recent_games <- result$recent_games
  
  # ============================================================================
  # Print header
  # ============================================================================
  
  cat("\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat("Player Simulation — ", metadata$player_name, " vs ", metadata$opponent, 
      " (", as.character(metadata$game_date), ")\n", sep = "")
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  
  # ============================================================================
  # Print recent games (last 3)
  # ============================================================================
  
  if (nrow(recent_games) > 0) {
    cat("Recent Performance (Last 3 Games):\n")
    cat(rep("-", 80), "\n", sep = "")
    
    # Print table header
    cat(sprintf("%-12s %-8s %8s %12s %8s %11s %10s %10s %12s\n",
                "Date", "Opponent", "Carries", "Rush Yds", "Targets", 
                "Receptions", "Rec Yds", "Total TDs", "PPR Points"))
    cat(rep("-", 80), "\n", sep = "")
    
    # Print each game
    for (i in 1:nrow(recent_games)) {
      row <- recent_games[i, ]
      
      # Get actual stats
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
      
      total_tds <- (ifelse(is.na(rush_tds), 0, rush_tds) + 
                    ifelse(is.na(rec_tds), 0, rec_tds))
      
      # Compute PPR fantasy points
      ppr_points <- compute_ppr_rb(
        rush_yards = ifelse(is.na(rush_yds), 0, rush_yds),
        rush_tds = ifelse(is.na(rush_tds), 0, rush_tds),
        receptions = ifelse(is.na(receptions), 0, receptions),
        rec_yards = ifelse(is.na(rec_yds), 0, rec_yds),
        rec_tds = ifelse(is.na(rec_tds), 0, rec_tds)
      )
      
      cat(sprintf("%-12s %-8s %8.0f %12.0f %8.0f %11.0f %10.0f %10.0f %12.1f\n",
                  as.character(row$gameday),
                  row$opponent,
                  ifelse(is.na(carries), 0, carries),
                  ifelse(is.na(rush_yds), 0, rush_yds),
                  ifelse(is.na(targets), 0, targets),
                  ifelse(is.na(receptions), 0, receptions),
                  ifelse(is.na(rec_yds), 0, rec_yds),
                  total_tds,
                  ppr_points))
    }
    
    cat("\n")
  }
  
  # ============================================================================
  # Print projected stat line (median)
  # ============================================================================
  
  cat("Projected Stat Line (Median):\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("   Carries:", summary_df$p50[summary_df$stat == "carries"], "\n")
  cat("   Rush yards:", summary_df$p50[summary_df$stat == "rush_yards"], "\n")
  cat("   Rush TDs:", round(summary_df$p50[summary_df$stat == "rush_tds"], 2), "\n")
  cat("   Targets:", summary_df$p50[summary_df$stat == "targets"], "\n")
  cat("   Receptions:", summary_df$p50[summary_df$stat == "receptions"], "\n")
  cat("   Receiving yards:", summary_df$p50[summary_df$stat == "rec_yards"], "\n")
  cat("   Receiving TDs:", round(summary_df$p50[summary_df$stat == "rec_tds"], 2), "\n")
  cat("   Total TDs:", 
      round(summary_df$p50[summary_df$stat == "rush_tds"] + 
            summary_df$p50[summary_df$stat == "rec_tds"], 2), "\n")
  
  median_ppr <- summary_df$p50[summary_df$stat == "fantasy_ppr"]
  cat("   PPR Fantasy Points:", round(median_ppr, 1), "\n")
  cat("\n")
  
  # ============================================================================
  # Print confidence intervals (p25 / p50 / p75)
  # ============================================================================
  
  cat("Confidence Intervals (25th / 50th / 75th Percentiles):\n")
  cat(rep("-", 80), "\n", sep = "")
  
  # Carries
  cat("   Carries:",
      summary_df$p25[summary_df$stat == "carries"], "/",
      summary_df$p50[summary_df$stat == "carries"], "/",
      summary_df$p75[summary_df$stat == "carries"], "\n")
  
  # Rush yards
  cat("   Rush yards:",
      summary_df$p25[summary_df$stat == "rush_yards"], "/",
      summary_df$p50[summary_df$stat == "rush_yards"], "/",
      summary_df$p75[summary_df$stat == "rush_yards"], "\n")
  
  # Receptions
  cat("   Receptions:",
      summary_df$p25[summary_df$stat == "receptions"], "/",
      summary_df$p50[summary_df$stat == "receptions"], "/",
      summary_df$p75[summary_df$stat == "receptions"], "\n")
  
  # Receiving yards
  cat("   Receiving yards:",
      summary_df$p25[summary_df$stat == "rec_yards"], "/",
      summary_df$p50[summary_df$stat == "rec_yards"], "/",
      summary_df$p75[summary_df$stat == "rec_yards"], "\n")
  
  # Total TDs
  rush_tds_p25 <- summary_df$p25[summary_df$stat == "rush_tds"]
  rush_tds_p50 <- summary_df$p50[summary_df$stat == "rush_tds"]
  rush_tds_p75 <- summary_df$p75[summary_df$stat == "rush_tds"]
  rec_tds_p25 <- summary_df$p25[summary_df$stat == "rec_tds"]
  rec_tds_p50 <- summary_df$p50[summary_df$stat == "rec_tds"]
  rec_tds_p75 <- summary_df$p75[summary_df$stat == "rec_tds"]
  
  cat("   Total TDs:",
      round(rush_tds_p25 + rec_tds_p25, 2), "/",
      round(rush_tds_p50 + rec_tds_p50, 2), "/",
      round(rush_tds_p75 + rec_tds_p75, 2), "\n")
  
  # PPR Fantasy Points
  cat("   PPR Fantasy Points:",
      round(summary_df$p25[summary_df$stat == "fantasy_ppr"], 1), "/",
      round(summary_df$p50[summary_df$stat == "fantasy_ppr"], 1), "/",
      round(summary_df$p75[summary_df$stat == "fantasy_ppr"], 1), "\n")
  
  cat("\n")
  
  # ============================================================================
  # Print TD probability summary
  # ============================================================================
  
  if (!is.null(result$diagnostics$td_probabilities)) {
    cat("Touchdown Probabilities:\n")
    cat(rep("-", 80), "\n", sep = "")
    cat("   Probability of 0 TDs:", 
        sprintf("%.1f%%", result$diagnostics$td_probabilities$prob_0_tds * 100), "\n")
    cat("   Probability of ≥1 TD:", 
        sprintf("%.1f%%", result$diagnostics$td_probabilities$prob_ge1_td * 100), "\n")
    cat("   Probability of ≥2 TDs:", 
        sprintf("%.1f%%", result$diagnostics$td_probabilities$prob_ge2_tds * 100), "\n")
    cat("\n")
  }
  
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
}

