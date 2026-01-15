# Report Schema v1
#
# Defines the API-safe report contract and validation helpers.

get_report_schema_version_v1 <- function() {
  "v1"
}

get_report_required_draws_v1 <- function(position) {
  switch(
    toupper(position),
    RB = c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns", "total_yards", "fantasy_ppr"),
    WR = c("targets", "receptions", "receiving_yards", "receiving_tds", "total_touchdowns", "total_yards", "fantasy_ppr"),
    TE = c("targets", "receptions", "receiving_yards", "receiving_tds", "total_touchdowns", "total_yards", "fantasy_ppr"),
    QB = c("passing_attempts", "passing_yards", "passing_tds", "interceptions_thrown",
           "qb_sacks_taken", "qb_rush_attempts", "qb_rush_yards", "qb_rush_tds"),
    K = c("fg_attempts", "fg_made", "pat_made", "fantasy_ppr"),
    character(0)
  )
}

get_report_required_summary_v1 <- function(position) {
  switch(
    toupper(position),
    RB = c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns", "fantasy_ppr"),
    WR = c("targets", "receptions", "receiving_yards", "receiving_tds", "total_touchdowns", "fantasy_ppr"),
    TE = c("targets", "receptions", "receiving_yards", "receiving_tds", "total_touchdowns", "fantasy_ppr"),
    QB = c("passing_attempts", "passing_yards", "passing_tds", "interceptions_thrown",
           "qb_sacks_taken", "qb_rush_attempts", "qb_rush_yards", "qb_rush_tds"),
    K = c("fg_attempts", "fg_made", "pat_made", "fantasy_ppr"),
    character(0)
  )
}

validate_report_schema_v1 <- function(result) {
  if (is.null(result) || !is.list(result)) {
    stop("Report schema v1: result must be a list.", call. = FALSE)
  }

  required_top <- c("metadata", "summary", "draws", "recent_games", "diagnostics")
  missing_top <- setdiff(required_top, names(result))
  if (length(missing_top) > 0) {
    stop("Report schema v1: missing top-level fields: ", paste(missing_top, collapse = ", "), call. = FALSE)
  }

  metadata <- result$metadata
  if (is.null(metadata) || !is.list(metadata)) {
    stop("Report schema v1: metadata must be a list.", call. = FALSE)
  }

  required_meta <- c("player_id", "season", "week", "n_sims", "position", "availability_policy",
                     "report_schema_version", "error_code")
  missing_meta <- setdiff(required_meta, names(metadata))
  if (length(missing_meta) > 0) {
    stop("Report schema v1: metadata missing required fields: ", paste(missing_meta, collapse = ", "), call. = FALSE)
  }
  if (!identical(metadata$report_schema_version, get_report_schema_version_v1())) {
    stop("Report schema v1: report_schema_version must be 'v1'.", call. = FALSE)
  }

  position <- metadata$position
  if (is.null(position) || !nzchar(as.character(position))) {
    stop("Report schema v1: metadata.position is required.", call. = FALSE)
  }
  position <- toupper(as.character(position))

  draws <- result$draws
  if (is.null(draws) || !is.data.frame(draws) || nrow(draws) == 0) {
    stop("Report schema v1: draws must be a non-empty data.frame.", call. = FALSE)
  }
  required_draws <- get_report_required_draws_v1(position)
  missing_draws <- setdiff(required_draws, names(draws))
  if (length(missing_draws) > 0) {
    stop("Report schema v1: draws missing required columns: ", paste(missing_draws, collapse = ", "), call. = FALSE)
  }
  for (col in required_draws) {
    if (!is.numeric(draws[[col]])) {
      stop("Report schema v1: draws column '", col, "' must be numeric.", call. = FALSE)
    }
  }

  summary <- result$summary
  if (is.null(summary) || !is.data.frame(summary) || nrow(summary) == 0) {
    stop("Report schema v1: summary must be a non-empty data.frame.", call. = FALSE)
  }
  required_summary_cols <- c("stat", "p25", "p50", "p75")
  missing_summary_cols <- setdiff(required_summary_cols, names(summary))
  if (length(missing_summary_cols) > 0) {
    stop("Report schema v1: summary missing required columns: ",
         paste(missing_summary_cols, collapse = ", "), call. = FALSE)
  }
  required_stats <- get_report_required_summary_v1(position)
  missing_stats <- setdiff(required_stats, summary$stat)
  if (length(missing_stats) > 0) {
    stop("Report schema v1: summary missing required stats: ", paste(missing_stats, collapse = ", "), call. = FALSE)
  }
  for (col in c("p25", "p50", "p75")) {
    if (!is.numeric(summary[[col]])) {
      stop("Report schema v1: summary column '", col, "' must be numeric.", call. = FALSE)
    }
  }

  recent_games <- result$recent_games
  if (is.null(recent_games) || !is.data.frame(recent_games)) {
    stop("Report schema v1: recent_games must be a data.frame (can be empty).", call. = FALSE)
  }

  diagnostics <- result$diagnostics
  if (is.null(diagnostics) || !is.list(diagnostics)) {
    stop("Report schema v1: diagnostics must be a list (can be empty).", call. = FALSE)
  }

  invisible(TRUE)
}
