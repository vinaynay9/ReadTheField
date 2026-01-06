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
    
    get_summary_value <- function(summary_df, stat_name, percentile = "p50") {
      if (is.null(summary_df) || nrow(summary_df) == 0) return(NA_real_)
      if (!percentile %in% names(summary_df)) return(NA_real_)
      if (!"stat" %in% names(summary_df)) return(NA_real_)
      rows <- summary_df[summary_df$stat == stat_name, , drop = FALSE]
      if (nrow(rows) == 0) return(NA_real_)
      val <- rows[[percentile]][1]
      if (is.null(val) || length(val) == 0) return(NA_real_)
      val
    }
    
    if (is.null(result) || is.null(result$metadata)) {
      cat("ERROR: Invalid result object provided\n")
      return(invisible(NULL))
    }
  
  metadata <- result$metadata
  summary_df <- if (!is.null(result$summary) && nrow(result$summary) > 0) result$summary else data.frame()
  recent_games <- if (!is.null(result$recent_games) && nrow(result$recent_games) > 0) result$recent_games else data.frame()
  position <- if (!is.null(metadata$position) && length(metadata$position) > 0) metadata$position else "RB"
  position <- toupper(as.character(position))
  if (is.na(position) || position == "") position <- "RB"
  is_rb <- position == "RB"
  is_wrte <- position %in% c("WR", "TE")
  is_qb <- position == "QB"
  is_k <- position == "K"
  
  # Normalize summary stat names and derive totals if missing
  if (nrow(summary_df) > 0 && "stat" %in% names(summary_df) && (is_rb || is_wrte)) {
    rename_map <- c(
      rush_yards = "rushing_yards",
      rec_yards = "receiving_yards",
      rush_tds = "rushing_tds",
      rec_tds = "receiving_tds",
      receiving_tds = "receiving_tds"
    )
    for (nm in names(rename_map)) {
      summary_df$stat[summary_df$stat == nm] <- rename_map[[nm]]
    }
    if (!"total_touchdowns" %in% summary_df$stat) {
      if (all(c("rushing_tds", "receiving_tds") %in% summary_df$stat) &&
          !is.null(result$draws)) {
        td_vec <- result$draws$rush_tds + result$draws$rec_tds
        summary_df <- rbind(
          summary_df,
          data.frame(
            stat = "total_touchdowns",
            p25 = stats::quantile(td_vec, 0.25, na.rm = TRUE),
            p50 = stats::quantile(td_vec, 0.50, na.rm = TRUE),
            p75 = stats::quantile(td_vec, 0.75, na.rm = TRUE)
          )
        )
      } else if ("receiving_tds" %in% summary_df$stat) {
        # WR/TE summary uses receiving_tds; total_tds equals receiving_tds.
        td_rows <- summary_df[summary_df$stat == "receiving_tds", , drop = FALSE]
        if (nrow(td_rows) > 0) {
          summary_df <- rbind(
            summary_df,
            data.frame(
              stat = "total_touchdowns",
              p25 = td_rows$p25[1],
              p50 = td_rows$p50[1],
              p75 = td_rows$p75[1]
            )
          )
        }
      }
    }
  }
  
  # HARD STOP: Validate summary dataframe has required stats
  if (nrow(summary_df) > 0) {
    required_stats <- if (is_rb) {
      c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns")
    } else if (is_wrte) {
      c("targets", "receptions", "receiving_yards", "total_touchdowns")
    } else if (is_qb) {
      c(
        "target_pass_attempts_qb",
        "target_pass_yards_qb",
        "target_pass_tds_qb",
        "target_interceptions_qb_thrown",
        "target_sacks_qb_taken",
        "target_qb_rush_yards",
        "target_qb_rush_tds"
      )
    } else if (is_k) {
      c("target_fg_made_k", "target_pat_made_k")
    } else {
      character(0)
    }
    if ("stat" %in% names(summary_df)) {
      available_stats <- summary_df$stat
      missing_stats <- setdiff(required_stats, available_stats)
      if (length(missing_stats) > 0) {
        stop("PRINT ERROR: Summary dataframe is missing required stats: ", 
             paste(missing_stats, collapse = ", "), 
             ". Available stats: ", paste(available_stats, collapse = ", "), 
             ". This indicates the summary was not computed on the resolved schema.")
      }
    }
    
    # Validate percentile columns exist
    required_cols <- c("stat", "p25", "p50", "p75")
    missing_cols <- setdiff(required_cols, names(summary_df))
    if (length(missing_cols) > 0) {
      stop("PRINT ERROR: Summary dataframe is missing required columns: ", 
           paste(missing_cols, collapse = ", "), 
           ". Cannot print confidence intervals.")
    }
    
    # Validate percentile columns are numeric
    for (col in c("p25", "p50", "p75")) {
      if (!is.numeric(summary_df[[col]])) {
        stop("PRINT ERROR: Summary column '", col, "' is not numeric (type: ", 
             class(summary_df[[col]]), "). Cannot print percentiles.")
      }
    }
  }
  
  # ============================================================================
  # Print header
  # ============================================================================
  
  cat("\n")
  if (!is.null(metadata$forced_counterfactual_notice) && nzchar(metadata$forced_counterfactual_notice)) {
    cat("WARNING: ", metadata$forced_counterfactual_notice, "\n", sep = "")
  }
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  cat("Player Simulation — ", safe_str(metadata$player_name, default = "(unknown)"), 
      " vs ", safe_str(metadata$opponent, default = "(unknown)"), 
      " (", safe_str(metadata$game_date, default = "(unknown)"), ")\n", sep = "")
  if (isTRUE(metadata$counterfactual_mode) ||
      (!is.null(result$diagnostics) &&
         !is.null(result$diagnostics$availability) &&
         isTRUE(result$diagnostics$availability$counterfactual))) {
    cf_team <- if (!is.null(metadata$counterfactual_team) && !is.na(metadata$counterfactual_team)) {
      paste0(" (team ", metadata$counterfactual_team, ")")
    } else {
      ""
    }
    cat("COUNTERFACTUAL SIMULATION (NOT OBSERVED)", cf_team, "\n")
  }
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  
  # ============================================================================
  # Print recent games (last 3)
  # ============================================================================
  
  cat("Recent Performance (Last 3 Games):\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (is_rb) {
    cat(sprintf("%-12s %-8s %8s %12s %8s %11s %10s %10s %12s\n",
                "Date", "Opponent", "Carries", "Rush Yds", "Targets",
                "Receptions", "Rec Yds", "Total TDs", "PPR Points"))
    cat(rep("-", 80), "\n", sep = "")
  } else if (is_wrte) {
    cat(sprintf("%-12s %-8s %8s %11s %10s %10s %10s %10s %12s\n",
                "Date", "Opponent", "Targets", "Receptions", "Rec Yds",
                "Air Yds", "Rec TDs", "Total TDs", "PPR Points"))
    cat(rep("-", 80), "\n", sep = "")
  } else if (is_qb) {
    cat(sprintf("%-12s %-8s %10s %11s %9s %9s %8s %10s\n",
                "Date", "Opponent", "Pass Att", "Pass Yds", "Pass TDs",
                "INTs", "Sacks", "Rush Yds"))
    cat(rep("-", 80), "\n", sep = "")
  } else if (is_k) {
    cat(sprintf("%-12s %-8s %8s %9s %8s %9s %12s\n",
                "Date", "Opponent", "FG Att", "FG Made", "PAT Att", "PAT Made", "PPR Points"))
    cat(rep("-", 80), "\n", sep = "")
  }

  if (nrow(recent_games) > 0) {
    for (i in 1:min(3, nrow(recent_games))) {
      row <- recent_games[i, ]

      targets <- if ("target_targets" %in% names(row)) {
        val <- row$target_targets
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("targets" %in% names(row)) {
        val <- row$targets
        ifelse(is.na(val), 0, as.numeric(val))
      } else {
        0
      }

      receptions <- if ("target_receptions" %in% names(row)) {
        val <- row$target_receptions
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("receptions" %in% names(row)) {
        val <- row$receptions
        ifelse(is.na(val), 0, as.numeric(val))
      } else {
        0
      }

      rec_yds <- if ("target_receiving_yards" %in% names(row)) {
        val <- row$target_receiving_yards
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("target_rec_yards" %in% names(row)) {
        val <- row$target_rec_yards
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("rec_yards" %in% names(row)) {
        val <- row$rec_yards
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("receiving_yards" %in% names(row)) {
        val <- row$receiving_yards
        ifelse(is.na(val), 0, as.numeric(val))
      } else {
        0
      }

      air_yds <- if ("air_yards" %in% names(row)) {
        val <- row$air_yards
        ifelse(is.na(val), 0, as.numeric(val))
      } else {
        0
      }

      rush_yds <- if ("target_rushing_yards" %in% names(row)) {
        val <- row$target_rushing_yards
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("target_rush_yards" %in% names(row)) {
        val <- row$target_rush_yards
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("rush_yards" %in% names(row)) {
        val <- row$rush_yards
        ifelse(is.na(val), 0, as.numeric(val))
      } else {
        0
      }

      carries <- if ("target_carries" %in% names(row)) {
        val <- row$target_carries
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("carries" %in% names(row)) {
        val <- row$carries
        ifelse(is.na(val), 0, as.numeric(val))
      } else {
        0
      }

      rush_tds <- if ("rushing_tds" %in% names(row)) {
        val <- row$rushing_tds
        ifelse(is.na(val), 0, as.numeric(val))
      } else {
        0
      }

      rec_tds <- if ("receiving_tds" %in% names(row)) {
        val <- row$receiving_tds
        ifelse(is.na(val), 0, as.numeric(val))
      } else if ("target_rec_tds" %in% names(row)) {
        val <- row$target_rec_tds
        ifelse(is.na(val), 0, as.numeric(val))
      } else {
        0
      }
      total_tds <- rush_tds + rec_tds

      ppr_points <- 0
      if (is_rb && exists("compute_ppr_rb")) {
        tryCatch({
          ppr_points <- compute_ppr_rb(
            rush_yards = rush_yds,
            rush_tds = rush_tds,
            receptions = receptions,
            rec_yards = rec_yds,
            rec_tds = rec_tds
          )
          if (is.null(ppr_points) || is.na(ppr_points) || length(ppr_points) == 0) {
            ppr_points <- 0
          }
        }, error = function(e) {
          ppr_points <<- 0
        })
      } else if (!is_rb && exists("compute_ppr_wrte")) {
        tryCatch({
          ppr_points <- compute_ppr_wrte(
            receptions = receptions,
            rec_yards = rec_yds,
            rec_tds = rec_tds
          )
          if (is.null(ppr_points) || is.na(ppr_points) || length(ppr_points) == 0) {
            ppr_points <- 0
          }
        }, error = function(e) {
          ppr_points <<- 0
        })
      }

      gameday_str <- if ("gameday" %in% names(row) && !is.na(row$gameday)) {
        as.character(row$gameday)
      } else if ("game_date" %in% names(row) && !is.na(row$game_date)) {
        as.character(row$game_date)
      } else {
        "(missing)"
      }

      opponent_str <- if ("opponent" %in% names(row) && !is.na(row$opponent)) {
        as.character(row$opponent)
      } else {
        "(missing)"
      }

      if (is_rb) {
        cat(sprintf("%-12s %-8s %8.0f %12.0f %8.0f %11.0f %10.0f %10.0f %12.1f\n",
                    gameday_str,
                    opponent_str,
                    carries,
                    rush_yds,
                    targets,
                    receptions,
                    rec_yds,
                    total_tds,
                    ppr_points))
      } else if (is_wrte) {
        cat(sprintf("%-12s %-8s %8.0f %11.0f %10.0f %10.0f %10.0f %10.0f %12.1f\n",
                    gameday_str,
                    opponent_str,
                    targets,
                    receptions,
                    rec_yds,
                    air_yds,
                    rec_tds,
                    total_tds,
                    ppr_points))
      } else if (is_qb) {
        pass_attempts <- if ("target_pass_attempts_qb" %in% names(row)) {
          ifelse(is.na(row$target_pass_attempts_qb), 0, as.numeric(row$target_pass_attempts_qb))
        } else if ("attempts" %in% names(row)) {
          ifelse(is.na(row$attempts), 0, as.numeric(row$attempts))
        } else {
          0
        }
        pass_yds <- if ("target_pass_yards_qb" %in% names(row)) {
          ifelse(is.na(row$target_pass_yards_qb), 0, as.numeric(row$target_pass_yards_qb))
        } else if ("passing_yards" %in% names(row)) {
          ifelse(is.na(row$passing_yards), 0, as.numeric(row$passing_yards))
        } else {
          0
        }
        pass_tds <- if ("target_pass_tds_qb" %in% names(row)) {
          ifelse(is.na(row$target_pass_tds_qb), 0, as.numeric(row$target_pass_tds_qb))
        } else if ("passing_tds" %in% names(row)) {
          ifelse(is.na(row$passing_tds), 0, as.numeric(row$passing_tds))
        } else {
          0
        }
        ints <- if ("target_interceptions_qb_thrown" %in% names(row)) {
          ifelse(is.na(row$target_interceptions_qb_thrown), 0, as.numeric(row$target_interceptions_qb_thrown))
        } else if ("passing_interceptions" %in% names(row)) {
          ifelse(is.na(row$passing_interceptions), 0, as.numeric(row$passing_interceptions))
        } else {
          0
        }
        sacks <- if ("target_sacks_qb_taken" %in% names(row)) {
          ifelse(is.na(row$target_sacks_qb_taken), 0, as.numeric(row$target_sacks_qb_taken))
        } else if ("sacks_suffered" %in% names(row)) {
          ifelse(is.na(row$sacks_suffered), 0, as.numeric(row$sacks_suffered))
        } else {
          0
        }
        rush_yds_qb <- if ("target_qb_rush_yards" %in% names(row)) {
          ifelse(is.na(row$target_qb_rush_yards), 0, as.numeric(row$target_qb_rush_yards))
        } else if ("rushing_yards" %in% names(row)) {
          ifelse(is.na(row$rushing_yards), 0, as.numeric(row$rushing_yards))
        } else {
          0
        }

        cat(sprintf("%-12s %-8s %10.0f %11.0f %9.0f %9.0f %8.0f %10.0f\n",
                    gameday_str,
                    opponent_str,
                    pass_attempts,
                    pass_yds,
                    pass_tds,
                    ints,
                    sacks,
                    rush_yds_qb))
      } else if (is_k) {
        fg_att <- if ("target_fg_attempts_k" %in% names(row)) {
          ifelse(is.na(row$target_fg_attempts_k), 0, as.numeric(row$target_fg_attempts_k))
        } else if ("fg_att" %in% names(row)) {
          ifelse(is.na(row$fg_att), 0, as.numeric(row$fg_att))
        } else {
          0
        }
        fg_made <- if ("target_fg_made_k" %in% names(row)) {
          ifelse(is.na(row$target_fg_made_k), 0, as.numeric(row$target_fg_made_k))
        } else if ("fg_made" %in% names(row)) {
          ifelse(is.na(row$fg_made), 0, as.numeric(row$fg_made))
        } else {
          0
        }
        pat_att <- if ("target_pat_attempts_k" %in% names(row)) {
          ifelse(is.na(row$target_pat_attempts_k), 0, as.numeric(row$target_pat_attempts_k))
        } else if ("pat_att" %in% names(row)) {
          ifelse(is.na(row$pat_att), 0, as.numeric(row$pat_att))
        } else {
          0
        }
        pat_made <- if ("target_pat_made_k" %in% names(row)) {
          ifelse(is.na(row$target_pat_made_k), 0, as.numeric(row$target_pat_made_k))
        } else if ("pat_made" %in% names(row)) {
          ifelse(is.na(row$pat_made), 0, as.numeric(row$pat_made))
        } else {
          0
        }
        k_ppr <- 0
        if (exists("compute_ppr_k")) {
          tryCatch({
            k_ppr <- compute_ppr_k(fg_made = fg_made, xp_made = pat_made)
            if (is.null(k_ppr) || is.na(k_ppr) || length(k_ppr) == 0) {
              k_ppr <- 0
            }
          }, error = function(e) {
            k_ppr <<- 0
          })
        }
        cat(sprintf("%-12s %-8s %8.0f %9.0f %8.0f %9.0f %12.1f\n",
                    gameday_str,
                    opponent_str,
                    fg_att,
                    fg_made,
                    pat_att,
                    pat_made,
                    k_ppr))
      }
    }
  } else {
    if (is_rb) {
      cat(sprintf("%-12s %-8s %8s %12s %8s %11s %10s %10s %12s\n",
                  "(missing)", "(missing)", "0", "0", "0", "0", "0", "0", "0.0"))
    } else if (is_wrte) {
      cat(sprintf("%-12s %-8s %8s %11s %10s %10s %10s %10s %12s\n",
                  "(missing)", "(missing)", "0", "0", "0", "0", "0", "0", "0.0"))
    } else if (is_qb) {
      cat(sprintf("%-12s %-8s %10s %11s %9s %9s %8s %10s\n",
                  "(missing)", "(missing)", "0", "0", "0", "0", "0", "0"))
    } else if (is_k) {
      cat(sprintf("%-12s %-8s %8s %9s %8s %9s %12s\n",
                  "(missing)", "(missing)", "0", "0", "0", "0", "0.0"))
    }
  }
  
  cat("\n")
  
  # ============================================================================
  # Print projected stat line (median)
  # ============================================================================
  
  cat("Projected Stat Line (Median):\n")
  cat(rep("-", 80), "\n", sep = "")
  
  carries_p50 <- get_summary_value(summary_df, "carries", "p50")
  rush_yds_p50 <- get_summary_value(summary_df, "rushing_yards", "p50")
  targets_p50 <- get_summary_value(summary_df, "targets", "p50")
  receptions_p50 <- get_summary_value(summary_df, "receptions", "p50")
  rec_yds_p50 <- get_summary_value(summary_df, "receiving_yards", "p50")
  total_tds_p50 <- get_summary_value(summary_df, "total_touchdowns", "p50")
  qb_pass_att_p50 <- get_summary_value(summary_df, "target_pass_attempts_qb", "p50")
  qb_pass_yds_p50 <- get_summary_value(summary_df, "target_pass_yards_qb", "p50")
  qb_pass_tds_p50 <- get_summary_value(summary_df, "target_pass_tds_qb", "p50")
  qb_ints_p50 <- get_summary_value(summary_df, "target_interceptions_qb_thrown", "p50")
  qb_sacks_p50 <- get_summary_value(summary_df, "target_sacks_qb_taken", "p50")
  qb_rush_yds_p50 <- get_summary_value(summary_df, "target_qb_rush_yards", "p50")
  k_fg_made_p50 <- get_summary_value(summary_df, "target_fg_made_k", "p50")
  k_pat_made_p50 <- get_summary_value(summary_df, "target_pat_made_k", "p50")
  
  if (is_rb) {
    cat("   Carries:", safe_num(carries_p50, digits = 0), "\n")
    cat("   Rush yards:", safe_num(rush_yds_p50, digits = 0), "\n")
    cat("   Receptions:", safe_num(receptions_p50, digits = 0), "\n")
    cat("   Receiving yards:", safe_num(rec_yds_p50, digits = 0), "\n")
    cat("   Total TDs:", safe_num(total_tds_p50, digits = 2), "\n")
  } else if (is_wrte) {
    cat("   Targets:", safe_num(targets_p50, digits = 0), "\n")
    cat("   Receptions:", safe_num(receptions_p50, digits = 0), "\n")
    cat("   Receiving yards:", safe_num(rec_yds_p50, digits = 0), "\n")
    cat("   Total TDs:", safe_num(total_tds_p50, digits = 2), "\n")
  } else if (is_qb) {
    cat("   Pass attempts:", safe_num(qb_pass_att_p50, digits = 0), "\n")
    cat("   Pass yards:", safe_num(qb_pass_yds_p50, digits = 0), "\n")
    cat("   Pass TDs:", safe_num(qb_pass_tds_p50, digits = 0), "\n")
    cat("   Interceptions:", safe_num(qb_ints_p50, digits = 0), "\n")
    cat("   Sacks taken:", safe_num(qb_sacks_p50, digits = 0), "\n")
    cat("   Rush yards:", safe_num(qb_rush_yds_p50, digits = 0), "\n")
  } else if (is_k) {
    cat("   FG made:", safe_num(k_fg_made_p50, digits = 0), "\n")
    cat("   PAT made:", safe_num(k_pat_made_p50, digits = 0), "\n")
  }
  
  # PPR fantasy points are derived from draw-level scoring summary when available
  ppr_p50 <- get_summary_value(summary_df, "fantasy_ppr", "p50")
  if ("stat" %in% names(summary_df) && "fantasy_ppr" %in% summary_df$stat) {
    cat("   PPR Fantasy Points:", safe_num(ppr_p50, digits = 1), "\n")
  }
  cat("\n")
  
  # ============================================================================
  # Print confidence intervals (p25 / p50 / p75)
  # ============================================================================
  
  cat("Confidence Intervals (25th / 50th / 75th Percentiles):\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (is_rb || is_wrte) {
    # Carries / Targets
    carries_p25 <- get_summary_value(summary_df, "carries", "p25")
    carries_p50 <- get_summary_value(summary_df, "carries", "p50")
    carries_p75 <- get_summary_value(summary_df, "carries", "p75")
    targets_p25 <- get_summary_value(summary_df, "targets", "p25")
    targets_p50 <- get_summary_value(summary_df, "targets", "p50")
    targets_p75 <- get_summary_value(summary_df, "targets", "p75")

    if (is_rb) {
      if (!is.na(carries_p25) && !is.na(carries_p50) && !is.na(carries_p75)) {
        if (carries_p25 > carries_p50 || carries_p50 > carries_p75) {
          stop("CONFIDENCE INTERVAL ERROR: Carries intervals out of order (p25=", carries_p25,
               ", p50=", carries_p50, ", p75=", carries_p75, ").")
        }
      }
      cat("   Carries:", safe_interval(carries_p25, carries_p50, carries_p75, digits = 0), "\n")
    } else {
      if (!is.na(targets_p25) && !is.na(targets_p50) && !is.na(targets_p75)) {
        if (targets_p25 > targets_p50 || targets_p50 > targets_p75) {
          stop("CONFIDENCE INTERVAL ERROR: Targets intervals out of order (p25=", targets_p25,
               ", p50=", targets_p50, ", p75=", targets_p75, ").")
        }
      }
      cat("   Targets:", safe_interval(targets_p25, targets_p50, targets_p75, digits = 0), "\n")
    }

    # Rush yards / Receiving yards
    rush_yds_p25 <- get_summary_value(summary_df, "rushing_yards", "p25")
    rush_yds_p50 <- get_summary_value(summary_df, "rushing_yards", "p50")
    rush_yds_p75 <- get_summary_value(summary_df, "rushing_yards", "p75")
    if (is_rb) {
      if (!is.na(rush_yds_p25) && !is.na(rush_yds_p50) && !is.na(rush_yds_p75)) {
        if (rush_yds_p25 > rush_yds_p50 || rush_yds_p50 > rush_yds_p75) {
          stop("CONFIDENCE INTERVAL ERROR: Rushing yards intervals out of order (p25=", rush_yds_p25,
               ", p50=", rush_yds_p50, ", p75=", rush_yds_p75, ").")
        }
      }
      cat("   Rush yards:", safe_interval(rush_yds_p25, rush_yds_p50, rush_yds_p75, digits = 0), "\n")
    }
    
    # Receptions
    rec_p25 <- get_summary_value(summary_df, "receptions", "p25")
    rec_p50 <- get_summary_value(summary_df, "receptions", "p50")
    rec_p75 <- get_summary_value(summary_df, "receptions", "p75")
    cat("   Receptions:", safe_interval(rec_p25, rec_p50, rec_p75, digits = 0), "\n")
    
    # Receiving yards
    rec_yds_p25 <- get_summary_value(summary_df, "receiving_yards", "p25")
    rec_yds_p50 <- get_summary_value(summary_df, "receiving_yards", "p50")
    rec_yds_p75 <- get_summary_value(summary_df, "receiving_yards", "p75")
    cat("   Receiving yards:", safe_interval(rec_yds_p25, rec_yds_p50, rec_yds_p75, digits = 0), "\n")
    
    # Total TDs
    total_tds_p25 <- get_summary_value(summary_df, "total_touchdowns", "p25")
    total_tds_p50 <- get_summary_value(summary_df, "total_touchdowns", "p50")
    total_tds_p75 <- get_summary_value(summary_df, "total_touchdowns", "p75")
    cat("   Total TDs:", safe_interval(total_tds_p25, total_tds_p50, total_tds_p75, digits = 2), "\n")
    
    # PPR Fantasy Points
    ppr_p25 <- get_summary_value(summary_df, "fantasy_ppr", "p25")
    ppr_p50 <- get_summary_value(summary_df, "fantasy_ppr", "p50")
    ppr_p75 <- get_summary_value(summary_df, "fantasy_ppr", "p75")
    cat("   PPR Fantasy Points:", safe_interval(ppr_p25, ppr_p50, ppr_p75, digits = 1), "\n")
  } else if (is_qb) {
    qb_pass_att_p25 <- get_summary_value(summary_df, "target_pass_attempts_qb", "p25")
    qb_pass_att_p50 <- get_summary_value(summary_df, "target_pass_attempts_qb", "p50")
    qb_pass_att_p75 <- get_summary_value(summary_df, "target_pass_attempts_qb", "p75")
    qb_pass_yds_p25 <- get_summary_value(summary_df, "target_pass_yards_qb", "p25")
    qb_pass_yds_p50 <- get_summary_value(summary_df, "target_pass_yards_qb", "p50")
    qb_pass_yds_p75 <- get_summary_value(summary_df, "target_pass_yards_qb", "p75")
    qb_pass_tds_p25 <- get_summary_value(summary_df, "target_pass_tds_qb", "p25")
    qb_pass_tds_p50 <- get_summary_value(summary_df, "target_pass_tds_qb", "p50")
    qb_pass_tds_p75 <- get_summary_value(summary_df, "target_pass_tds_qb", "p75")
    qb_ints_p25 <- get_summary_value(summary_df, "target_interceptions_qb_thrown", "p25")
    qb_ints_p50 <- get_summary_value(summary_df, "target_interceptions_qb_thrown", "p50")
    qb_ints_p75 <- get_summary_value(summary_df, "target_interceptions_qb_thrown", "p75")
    qb_sacks_p25 <- get_summary_value(summary_df, "target_sacks_qb_taken", "p25")
    qb_sacks_p50 <- get_summary_value(summary_df, "target_sacks_qb_taken", "p50")
    qb_sacks_p75 <- get_summary_value(summary_df, "target_sacks_qb_taken", "p75")
    qb_rush_yds_p25 <- get_summary_value(summary_df, "target_qb_rush_yards", "p25")
    qb_rush_yds_p50 <- get_summary_value(summary_df, "target_qb_rush_yards", "p50")
    qb_rush_yds_p75 <- get_summary_value(summary_df, "target_qb_rush_yards", "p75")
    cat("   Pass attempts:", safe_interval(qb_pass_att_p25, qb_pass_att_p50, qb_pass_att_p75, digits = 0), "\n")
    cat("   Pass yards:", safe_interval(qb_pass_yds_p25, qb_pass_yds_p50, qb_pass_yds_p75, digits = 0), "\n")
    cat("   Pass TDs:", safe_interval(qb_pass_tds_p25, qb_pass_tds_p50, qb_pass_tds_p75, digits = 0), "\n")
    cat("   Interceptions:", safe_interval(qb_ints_p25, qb_ints_p50, qb_ints_p75, digits = 0), "\n")
    cat("   Sacks taken:", safe_interval(qb_sacks_p25, qb_sacks_p50, qb_sacks_p75, digits = 0), "\n")
    cat("   Rush yards:", safe_interval(qb_rush_yds_p25, qb_rush_yds_p50, qb_rush_yds_p75, digits = 0), "\n")
  } else if (is_k) {
    k_fg_p25 <- get_summary_value(summary_df, "target_fg_made_k", "p25")
    k_fg_p50 <- get_summary_value(summary_df, "target_fg_made_k", "p50")
    k_fg_p75 <- get_summary_value(summary_df, "target_fg_made_k", "p75")
    k_pat_p25 <- get_summary_value(summary_df, "target_pat_made_k", "p25")
    k_pat_p50 <- get_summary_value(summary_df, "target_pat_made_k", "p50")
    k_pat_p75 <- get_summary_value(summary_df, "target_pat_made_k", "p75")
    cat("   FG made:", safe_interval(k_fg_p25, k_fg_p50, k_fg_p75, digits = 0), "\n")
    cat("   PAT made:", safe_interval(k_pat_p25, k_pat_p50, k_pat_p75, digits = 0), "\n")
    ppr_p25 <- get_summary_value(summary_df, "fantasy_ppr", "p25")
    ppr_p50 <- get_summary_value(summary_df, "fantasy_ppr", "p50")
    ppr_p75 <- get_summary_value(summary_df, "fantasy_ppr", "p75")
    if (!all(is.na(c(ppr_p25, ppr_p50, ppr_p75)))) {
      cat("   PPR Fantasy Points:", safe_interval(ppr_p25, ppr_p50, ppr_p75, digits = 1), "\n")
    }
  }
  
  cat("\n")
  
  # ============================================================================
  # Print TD probability summary
  # ============================================================================
  
  if (!is.null(result$diagnostics) && !is.null(result$diagnostics$td_probabilities)) {
    td_probs <- result$diagnostics$td_probabilities
    cat("Touchdown Probabilities:\n")
    cat(rep("-", 80), "\n", sep = "")
    
    prob_0 <- if (!is.null(td_probs$prob_0_tds)) td_probs$prob_0_tds else NA_real_
    prob_ge1 <- if (!is.null(td_probs$prob_ge1_td)) td_probs$prob_ge1_td else NA_real_
    prob_ge2 <- if (!is.null(td_probs$prob_ge2_tds)) td_probs$prob_ge2_tds else NA_real_
    
    cat("   Probability of 0 TDs:", 
        if (!is.na(prob_0)) sprintf("%.1f%%", prob_0 * 100) else "(missing)", "\n")
    cat("   Probability of ≥1 TD:", 
        if (!is.na(prob_ge1)) sprintf("%.1f%%", prob_ge1 * 100) else "(missing)", "\n")
    cat("   Probability of ≥2 TDs:", 
        if (!is.na(prob_ge2)) sprintf("%.1f%%", prob_ge2 * 100) else "(missing)", "\n")
    cat("\n")
  }
  
  cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  
  }, error = function(e) {
    cat("\nERROR: Printing failed:", conditionMessage(e), "\n")
    cat("Result structure may be incomplete or corrupted.\n")
    cat(paste0(rep("=", 80), collapse = ""), "\n\n")
  })
}

