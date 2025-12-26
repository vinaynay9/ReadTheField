# Passing Defense Feature Helpers
#
# Centralized definitions for passing-focused defensive rolling features
# used by WR/TE pipelines.

get_passing_defense_roll1_features <- function() {
  c(
    "def_pass_yards_defense_allowed_roll1",
    "def_pass_attempts_defense_allowed_roll1",
    "def_yards_per_pass_defense_allowed_roll1",
    "def_points_defense_allowed_roll1",
    "def_sacks_defense_forced_roll1",
    "def_tackles_for_loss_defense_forced_roll1",
    "def_interceptions_defense_caught_roll1",
    "def_passes_defended_defense_forced_roll1"
  )
}

get_passing_defense_roll3_features <- function() {
  c(
    "def_pass_yards_defense_allowed_roll3",
    "def_pass_attempts_defense_allowed_roll3",
    "def_yards_per_pass_defense_allowed_roll3",
    "def_points_defense_allowed_roll3",
    "def_sacks_defense_forced_roll3",
    "def_tackles_for_loss_defense_forced_roll3",
    "def_interceptions_defense_caught_roll3",
    "def_passes_defended_defense_forced_roll3"
  )
}

get_passing_defense_roll5_features <- function() {
  c(
    "def_pass_yards_defense_allowed_roll5",
    "def_pass_attempts_defense_allowed_roll5",
    "def_yards_per_pass_defense_allowed_roll5",
    "def_points_defense_allowed_roll5",
    "def_sacks_defense_forced_roll5",
    "def_tackles_for_loss_defense_forced_roll5",
    "def_interceptions_defense_caught_roll5",
    "def_passes_defended_defense_forced_roll5"
  )
}

get_passing_defense_all_features <- function() {
  c(
    get_passing_defense_roll1_features(),
    get_passing_defense_roll3_features(),
    get_passing_defense_roll5_features()
  )
}
