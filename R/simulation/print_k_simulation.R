# Print K Simulation - Player-Focused Summary
#
# Prints K simulation summaries only (no model diagnostics).

#' Print K simulation summary output
#'
#' @param draws data.frame of simulation draws or result list containing $draws
#' @param context optional list with metadata (player_name, team, opponent, season, week, n_sims)
print_k_simulation <- function(draws, context = NULL) {
  tryCatch({
    if (is.list(draws) && !is.null(draws$draws)) {
      context <- draws
      draws <- draws$draws
    }
    if (is.null(draws) || nrow(draws) == 0) {
      cat("ERROR: No K draws to print\n")
      return(invisible(NULL))
    }

    safe_num <- function(x, digits = 1, default = "NA") {
      if (is.null(x) || length(x) == 0) return(default)
      if (is.na(x) || !is.numeric(x)) return(default)
      if (length(x) != 1) return(default)
      tryCatch(round(x, digits), error = function(e) default)
    }

    get_vec <- function(primary, fallback = NULL) {
      if (primary %in% names(draws)) return(as.numeric(draws[[primary]]))
      if (!is.null(fallback) && fallback %in% names(draws)) return(as.numeric(draws[[fallback]]))
      rep(NA_real_, nrow(draws))
    }

    fg_made <- get_vec("fg_made", "target_fg_made_k")
    fg_att <- get_vec("fg_attempts", "target_fg_attempts_k")
    pat_made <- get_vec("pat_made", "target_pat_made_k")

    fantasy_ppr <- if (exists("compute_ppr_k")) {
      compute_ppr_k(fg_made = fg_made, xp_made = pat_made)
    } else {
      rep(NA_real_, nrow(draws))
    }

    stat_summary <- function(x) {
      list(
        mean = mean(x, na.rm = TRUE),
        median = stats::median(x, na.rm = TRUE),
        p10 = stats::quantile(x, 0.10, na.rm = TRUE),
        p25 = stats::quantile(x, 0.25, na.rm = TRUE),
        p75 = stats::quantile(x, 0.75, na.rm = TRUE),
        p90 = stats::quantile(x, 0.90, na.rm = TRUE)
      )
    }

    rows <- list(
      "FG Made" = stat_summary(fg_made),
      "FG Attempts" = stat_summary(fg_att),
      "XP Made" = stat_summary(pat_made),
      "Fantasy Points" = stat_summary(fantasy_ppr)
    )

    metadata <- if (!is.null(context$metadata)) context$metadata else list()
    header_player <- if (!is.null(metadata$player_name)) metadata$player_name else "(unknown)"
    header_team <- if (!is.null(metadata$team)) metadata$team else "(unknown)"
    header_opp <- if (!is.null(metadata$opponent)) metadata$opponent else "(unknown)"
    header_season <- if (!is.null(metadata$season)) metadata$season else "(unknown)"
    header_week <- if (!is.null(metadata$week)) metadata$week else "(unknown)"
    header_sims <- if (!is.null(metadata$n_sims)) metadata$n_sims else nrow(draws)
    header_game_id <- if (!is.null(metadata$game_id)) metadata$game_id else NA_character_

    cat("\n")
    cat(paste0(rep("=", 80), collapse = ""), "\n")
    cat("K Simulation Summary\n")
    cat("Player: ", header_player, " | Season ", header_season, " Week ", header_week, "\n", sep = "")
    cat("Matchup: ", header_team, " vs ", header_opp, " | Sims: ", header_sims, "\n", sep = "")
    if (!is.na(header_game_id) && nzchar(header_game_id)) {
      cat("Game ID: ", header_game_id, "\n", sep = "")
    }
    cat(paste0(rep("=", 80), collapse = ""), "\n\n")

    cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s\n",
                "Stat", "Mean", "Median", "P10", "P25", "P75", "P90"))
    cat(rep("-", 80), "\n", sep = "")
    for (lbl in names(rows)) {
      r <- rows[[lbl]]
      cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s\n",
                  lbl,
                  safe_num(r$mean), safe_num(r$median),
                  safe_num(r$p10), safe_num(r$p25),
                  safe_num(r$p75), safe_num(r$p90)))
    }
    cat("\n")
  }, error = function(e) {
    cat("\nERROR: K summary printing failed:", conditionMessage(e), "\n")
  })
}
