# Print WR Simulation - Player-Focused Summary
#
# Prints WR simulation summaries only (no model diagnostics).

#' Print WR simulation summary output
#'
#' @param draws data.frame of simulation draws or result list containing $draws
#' @param context optional list with metadata (player_name, team, opponent, season, week, n_sims)
print_wr_simulation <- function(draws, context = NULL) {
  tryCatch({
    if (is.list(draws) && !is.null(draws$draws)) {
      context <- draws
      draws <- draws$draws
    }
    if (is.null(draws) || nrow(draws) == 0) {
      cat("ERROR: No WR draws to print\n")
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

    targets <- get_vec("targets", "target_targets")
    receptions <- get_vec("receptions", "target_receptions")
    rec_yards <- get_vec("receiving_yards", "target_rec_yards")
    rec_tds <- get_vec("receiving_tds", "target_rec_tds")

    fantasy_ppr <- if (exists("compute_ppr_wrte")) {
      compute_ppr_wrte(
        receptions = receptions,
        rec_yards = rec_yards,
        rec_tds = rec_tds
      )
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
      "Targets" = stat_summary(targets),
      "Receptions" = stat_summary(receptions),
      "Receiving Yards" = stat_summary(rec_yards),
      "Receiving TDs" = stat_summary(rec_tds),
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
    cat("WR Simulation Summary\n")
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
      digits <- if (lbl == "Receiving TDs") 2 else 1
      cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s\n",
                  lbl,
                  safe_num(r$mean, digits = digits), safe_num(r$median, digits = digits),
                  safe_num(r$p10, digits = digits), safe_num(r$p25, digits = digits),
                  safe_num(r$p75, digits = digits), safe_num(r$p90, digits = digits)))
    }
    cat("\n")
  }, error = function(e) {
    cat("\nERROR: WR summary printing failed:", conditionMessage(e), "\n")
  })
}
