# Passing Defense Feature Helpers
#
# Centralized definitions for passing-focused defensive rolling features
# used by WR/TE pipelines.

get_passing_defense_roll1_features <- function() {
  c(
    "opp_pass_yards_allowed_roll1",
    "opp_yards_per_pass_allowed_roll1",
    "opp_points_allowed_roll1",
    "opp_sacks_roll1",
    "opp_tfl_roll1",
    "opp_interceptions_roll1",
    "opp_passes_defended_roll1"
  )
}

get_passing_defense_roll3_features <- function() {
  c(
    "def_interceptions_roll3",
    "def_passes_defended_roll3"
  )
}

get_passing_defense_roll5_features <- function() {
  c(
    "opp_pass_yards_allowed_roll5",
    "opp_yards_per_pass_allowed_roll5",
    "opp_points_allowed_roll5",
    "opp_sacks_roll5",
    "opp_tfl_roll5",
    "def_interceptions_roll5",
    "def_passes_defended_roll5"
  )
}

get_passing_defense_all_features <- function() {
  c(
    get_passing_defense_roll1_features(),
    get_passing_defense_roll3_features(),
    get_passing_defense_roll5_features()
  )
}
