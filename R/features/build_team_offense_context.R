# Build Team Offense Context (lagged team-level signals)
#
# Computes team-level offensive context from weekly player stats and produces
# lag-1 (previous-week) features for downstream RB modeling. All features are
# strictly lagged; Week 1 values are NA. No imputation is performed.

suppressPackageStartupMessages({
  library(dplyr)
})

#' Build team offense context (lag-1) from weekly player stats
#'
#' @param seasons Integer vector of seasons
#' @param season_type Season type (default "REG")
#' @param write_cache Logical, write parquet to data/processed/team_offense_context.parquet
#' @return data.frame with team-level context and roll1 features
build_team_offense_context <- function(seasons,
                                       season_type = "REG",
                                       write_cache = TRUE) {
  if (missing(seasons) || length(seasons) == 0) {
    stop("seasons vector is required to build team offense context")
  }

  # Ensure helpers for nflreadr loading are available
  if (!exists("load_weekly_player_stats_from_nflreadr") ||
      !exists("select_first_available") ||
      !exists("normalize_position")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("Missing R/data/build_weekly_player_layers.R for loading weekly player stats")
    }
  }

  stats <- load_weekly_player_stats_from_nflreadr(seasons, season_type)
  if (is.null(stats) || nrow(stats) == 0) {
    stop("No weekly player stats available for seasons: ", paste(seasons, collapse = ", "))
  }

  positions <- normalize_position(select_first_available(stats, c("position", "position_group"), NA))
  teams <- toupper(as.character(select_first_available(stats, c("team", "team_abbr", "recent_team", "team_name"), NA)))
  seasons_vec <- as.integer(stats$season)
  weeks <- as.integer(stats$week)

  qb_pass_attempts <- as.numeric(select_first_available(stats, c("attempts", "pass_attempts", "passing_attempts"), 0))
  qb_pass_yards <- as.numeric(select_first_available(stats, c("passing_yards", "pass_yards"), 0))
  qb_pass_tds <- as.numeric(select_first_available(stats, c("passing_tds", "pass_tds", "pass_touchdowns"), 0))

  rb_carries <- as.numeric(select_first_available(stats, c("rush_attempts", "carries", "rushing_attempts"), 0))
  rb_targets <- as.numeric(select_first_available(stats, c("targets"), 0))

  wr_targets <- as.numeric(select_first_available(stats, c("targets"), 0))
  wr_receiving_yards <- as.numeric(select_first_available(stats, c("receiving_yards", "rec_yards"), 0))
  wr_air_yards <- as.numeric(select_first_available(stats, c("air_yards", "rec_air_yards", "receiving_air_yards"), NA))

  qb_df <- data.frame(
    team = teams,
    season = seasons_vec,
    week = weeks,
    position = positions,
    pass_attempts = qb_pass_attempts,
    pass_yards = qb_pass_yards,
    pass_tds = qb_pass_tds,
    stringsAsFactors = FALSE
  ) %>% filter(position == "QB", !is.na(team), team != "", !is.na(season), !is.na(week))

  rb_df <- data.frame(
    team = teams,
    season = seasons_vec,
    week = weeks,
    position = positions,
    carries = rb_carries,
    targets = rb_targets,
    stringsAsFactors = FALSE
  ) %>% filter(position == "RB", !is.na(team), team != "", !is.na(season), !is.na(week))

  wr_df <- data.frame(
    team = teams,
    season = seasons_vec,
    week = weeks,
    position = positions,
    targets = wr_targets,
    receiving_yards = wr_receiving_yards,
    air_yards = wr_air_yards,
    stringsAsFactors = FALSE
  ) %>% filter(position == "WR", !is.na(team), team != "", !is.na(season), !is.na(week))

  # Aggregations ---------------------------------------------------------------
  qb_agg <- qb_df %>%
    group_by(team, season, week) %>%
    summarise(
      team_qb_pass_attempts = sum(pass_attempts, na.rm = TRUE),
      team_qb_pass_yards = sum(pass_yards, na.rm = TRUE),
      team_qb_pass_tds = sum(pass_tds, na.rm = TRUE),
      .groups = "drop"
    )

  rb_agg <- rb_df %>%
    group_by(team, season, week) %>%
    summarise(
      team_rb_carries_total = sum(carries, na.rm = TRUE),
      team_rb_targets_total = sum(targets, na.rm = TRUE),
      team_rb_carry_share_top1 = {
        total <- sum(carries, na.rm = TRUE)
        if (total > 0) max(carries, na.rm = TRUE) / total else NA_real_
      },
      team_rb_carry_share_top2 = {
        total <- sum(carries, na.rm = TRUE)
        if (total > 0) {
          carries_vec <- sort(carries, decreasing = TRUE, na.last = TRUE)
          sum(head(carries_vec[!is.na(carries_vec)], 2)) / total
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )

  wr_agg <- wr_df %>%
    group_by(team, season, week) %>%
    summarise(
      team_wr_targets_total = sum(targets, na.rm = TRUE),
      team_wr_receiving_yards = sum(receiving_yards, na.rm = TRUE),
      team_wr_air_yards = if (all(is.na(air_yards))) NA_real_ else sum(air_yards, na.rm = TRUE),
      .groups = "drop"
    )

  key_rows <- bind_rows(
    qb_agg %>% select(team, season, week),
    rb_agg %>% select(team, season, week),
    wr_agg %>% select(team, season, week)
  ) %>%
    distinct()

  if (nrow(key_rows) == 0) {
    stop("No team-week rows found while building team offense context")
  }

  team_offense_context <- key_rows %>%
    left_join(qb_agg, by = c("team", "season", "week")) %>%
    left_join(rb_agg, by = c("team", "season", "week")) %>%
    left_join(wr_agg, by = c("team", "season", "week"))

  # Derive lag-1 features (previous week only), enforcing season boundaries
  value_cols <- setdiff(names(team_offense_context), c("team", "season", "week"))
  team_offense_context <- team_offense_context %>%
    group_by(team, season) %>%
    arrange(week, .by_group = TRUE) %>%
    mutate(
      across(
        all_of(value_cols),
        ~ dplyr::lag(.x, 1),
        .names = "{.col}_roll1"
      )
    ) %>%
    ungroup() %>%
    mutate(
      across(
        ends_with("_roll1"),
        ~ dplyr::if_else(week == 1, NA_real_, .)
      )
    )

  dupes <- duplicated(team_offense_context[, c("team", "season", "week")])
  if (any(dupes)) {
    stop("Duplicate rows detected in team offense context for (team, season, week)")
  }

  if (write_cache) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to write team offense context. Install with install.packages('arrow').")
    }
    output_path <- file.path("data", "processed", "team_offense_context.parquet")
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(team_offense_context, output_path)
  }

  team_offense_context
}
