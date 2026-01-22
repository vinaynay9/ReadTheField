# Simulation Bootstrap - Load All Required Dependencies
#
# This file sources all required data, model, and utility files for RB simulation.
# It contains no execution logic - only dependency loading.
#
# Usage:
#   source(file.path(getOption("READTHEFIELD_REPO_ROOT"), "R/simulation/bootstrap_simulation.R"))
#
# This ensures all simulation functions are available without relying on
# RStudio, global environment, or prior scripts.

message("Bootstrap start: loading simulation dependencies.")

repo_root <- getOption("READTHEFIELD_REPO_ROOT")
if (is.null(repo_root) || !nzchar(repo_root)) {
  stop("READTHEFIELD_REPO_ROOT not set. Use an entrypoint that sets it before bootstrapping.")
}
repo_root <- normalizePath(repo_root, mustWork = TRUE)
message("Bootstrap repo root resolved: ", repo_root)

repo_path <- function(...) {
  file.path(repo_root, ...)
}

source_repo <- function(path, local = TRUE) {
  source(repo_path(path), local = .GlobalEnv)
}

# ============================================================================
# CACHE HELPERS
# ============================================================================

if (!exists("build_game_key")) {
  if (file.exists(repo_path("R/utils/cache_helpers.R"))) {
    source_repo("R/utils/cache_helpers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/utils/cache_helpers.R not found")
  }
}
message("Bootstrap cache helpers loaded.")

# ============================================================================
# RB V1 SCHEMA (CRITICAL: LOAD FIRST)
# ============================================================================

# Load RB v1 schema (required for all RB simulations)
if (file.exists(repo_path("R/utils/rb_schema_v1.R"))) {
  source_repo("R/utils/rb_schema_v1.R", local = TRUE)
} else {
  stop("Missing R/utils/rb_schema_v1.R — RB v1 schema is required for simulation")
}

# Hard guardrail
if (!exists("get_rb_v1_targets")) {
  stop("RB v1 schema not loaded: get_rb_v1_targets() missing")
}

# Load RB v1 regime system (required for time-aware modeling)
if (file.exists(repo_path("R/utils/rb_regime_v1.R"))) {
  source_repo("R/utils/rb_regime_v1.R", local = TRUE)
} else {
  stop("Missing R/utils/rb_regime_v1.R — RB v1 regime system is required for simulation")
}

# Hard guardrail
if (!exists("get_rb_regimes")) {
  stop("RB v1 regime system not loaded: get_rb_regimes() missing")
}
if (!exists("get_model_key")) {
  stop("RB v1 regime system not loaded: get_model_key() missing")
}

# ============================================================================
# WR/TE V1 SCHEMAS + REGIMES
# ============================================================================

if (file.exists(repo_path("R/positions/passing_defense_features.R"))) {
  source_repo("R/positions/passing_defense_features.R", local = TRUE)
} else {
  stop("Missing R/positions/passing_defense_features.R - passing defense helpers required")
}

if (file.exists(repo_path("R/positions/WR/wr_schema_v1.R"))) {
  source_repo("R/positions/WR/wr_schema_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/WR/wr_schema_v1.R - WR v1 schema is required for simulation")
}
if (!exists("get_wr_v1_targets")) {
  stop("WR v1 schema not loaded: get_wr_v1_targets() missing")
}
if (file.exists(repo_path("R/positions/WR/wr_regime_v1.R"))) {
  source_repo("R/positions/WR/wr_regime_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/WR/wr_regime_v1.R - WR v1 regime system is required for simulation")
}
if (!exists("get_wr_regimes")) {
  stop("WR v1 regime system not loaded: get_wr_regimes() missing")
}
if (!exists("get_wr_model_key")) {
  stop("WR v1 regime system not loaded: get_wr_model_key() missing")
}

if (file.exists(repo_path("R/positions/TE/te_schema_v1.R"))) {
  source_repo("R/positions/TE/te_schema_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/TE/te_schema_v1.R - TE v1 schema is required for simulation")
}
if (!exists("get_te_v1_targets")) {
  stop("TE v1 schema not loaded: get_te_v1_targets() missing")
}
if (file.exists(repo_path("R/positions/TE/te_regime_v1.R"))) {
  source_repo("R/positions/TE/te_regime_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/TE/te_regime_v1.R - TE v1 regime system is required for simulation")
}
if (!exists("get_te_regimes")) {
  stop("TE v1 regime system not loaded: get_te_regimes() missing")
}
if (!exists("get_te_model_key")) {
  stop("TE v1 regime system not loaded: get_te_model_key() missing")
}

# ============================================================================
# QB/K V1 SCHEMAS + REGIMES
# ============================================================================

if (file.exists(repo_path("R/positions/QB/qb_schema_v1.R"))) {
  source_repo("R/positions/QB/qb_schema_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/QB/qb_schema_v1.R - QB v1 schema is required for simulation")
}
if (!exists("get_qb_v1_targets")) {
  stop("QB v1 schema not loaded: get_qb_v1_targets() missing")
}
if (file.exists(repo_path("R/positions/QB/qb_regime_v1.R"))) {
  source_repo("R/positions/QB/qb_regime_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/QB/qb_regime_v1.R - QB v1 regime system is required for simulation")
}
if (!exists("get_qb_regimes")) {
  stop("QB v1 regime system not loaded: get_qb_regimes() missing")
}
if (!exists("get_qb_model_key")) {
  stop("QB v1 regime system not loaded: get_qb_model_key() missing")
}

if (file.exists(repo_path("R/positions/K/k_schema_v1.R"))) {
  source_repo("R/positions/K/k_schema_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/K/k_schema_v1.R - K v1 schema is required for simulation")
}

# ============================================================================
# PRINTING HELPERS (POSITION-AGNOSTIC)
# ============================================================================

if (file.exists(repo_path("R/simulation/print_player_simulation.R"))) {
  source_repo("R/simulation/print_player_simulation.R", local = TRUE)
} else {
  stop("Missing R/simulation/print_player_simulation.R - print helpers required")
}
if (file.exists(repo_path("R/simulation/print_rb_simulation.R"))) {
  source_repo("R/simulation/print_rb_simulation.R", local = TRUE)
}
if (file.exists(repo_path("R/simulation/print_wr_simulation.R"))) {
  source_repo("R/simulation/print_wr_simulation.R", local = TRUE)
}
if (file.exists(repo_path("R/simulation/print_qb_simulation.R"))) {
  source_repo("R/simulation/print_qb_simulation.R", local = TRUE)
}
if (file.exists(repo_path("R/simulation/print_k_simulation.R"))) {
  source_repo("R/simulation/print_k_simulation.R", local = TRUE)
}
if (file.exists(repo_path("R/simulation/write_rb_simulation.R"))) {
  source_repo("R/simulation/write_rb_simulation.R", local = TRUE)
}
if (file.exists(repo_path("R/simulation/warning_policy_v1.R"))) {
  source_repo("R/simulation/warning_policy_v1.R", local = TRUE)
}
if (file.exists(repo_path("R/simulation/report_schema_v1.R"))) {
  source_repo("R/simulation/report_schema_v1.R", local = TRUE)
}
if (file.exists(repo_path("R/simulation/simulate_player_game_v1.R"))) {
  source_repo("R/simulation/simulate_player_game_v1.R", local = TRUE)
}
if (file.exists(repo_path("R/simulation/validate_simulation_request.R"))) {
  source_repo("R/simulation/validate_simulation_request.R", local = TRUE)
}
if (!exists("get_k_v1_targets")) {
  stop("K v1 schema not loaded: get_k_v1_targets() missing")
}
if (file.exists(repo_path("R/positions/K/k_regime_v1.R"))) {
  source_repo("R/positions/K/k_regime_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/K/k_regime_v1.R - K v1 regime system is required for simulation")
}
if (!exists("get_k_regimes")) {
  stop("K v1 regime system not loaded: get_k_regimes() missing")
}
if (!exists("get_k_model_key")) {
  stop("K v1 regime system not loaded: get_k_model_key() missing")
}

required_schema_fns <- c(
  "get_rb_v1_targets",
  "get_wr_v1_targets",
  "get_te_v1_targets",
  "get_qb_v1_targets",
  "get_k_v1_targets"
)
missing_schema_fns <- required_schema_fns[!vapply(required_schema_fns, exists, logical(1))]
if (length(missing_schema_fns) > 0) {
  stop("Schema functions not loaded: ", paste(missing_schema_fns, collapse = ", "))
}
message("Bootstrap schema files loaded.")

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

# PPR scoring functions (for print functions)
if (!exists("compute_ppr_rb")) {
  if (file.exists(repo_path("R/utils/ppr_scoring.R"))) {
    source_repo("R/utils/ppr_scoring.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/utils/ppr_scoring.R not found")
  }
}

if (!exists("get_available_seasons_from_cache")) {
  if (file.exists(repo_path("R/utils/cache_helpers.R"))) {
    source_repo("R/utils/cache_helpers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: get_available_seasons_from_cache not loaded")
  }
}

# ============================================================================
# SCHEMA LOADING
# ============================================================================

schema_files <- c(
  "R/utils/rb_schema_v1.R",
  "R/positions/WR/wr_schema_v1.R",
  "R/positions/TE/te_schema_v1.R",
  "R/positions/QB/qb_schema_v1.R",
  "R/positions/K/k_schema_v1.R"
)

for (path in schema_files) {
  if (!file.exists(repo_path(path))) {
    stop("Missing required schema file: ", path)
  }
  source_repo(path, local = TRUE)
}

required_schema_fns <- c(
  "get_rb_v1_targets",
  "get_wr_v1_targets",
  "get_te_v1_targets",
  "get_qb_v1_targets",
  "get_k_v1_targets"
)
missing_schema_fns <- required_schema_fns[!vapply(required_schema_fns, exists, logical(1))]
if (length(missing_schema_fns) > 0) {
  stop("Schema functions not loaded: ", paste(missing_schema_fns, collapse = ", "))
}

# ============================================================================
# DATA LAYER FUNCTIONS
# ============================================================================

# Schedule loading (for future game resolution)
if (!exists("load_schedules")) {
  if (file.exists(repo_path("R/data/load_schedules.R"))) {
    source_repo("R/data/load_schedules.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/data/load_schedules.R not found")
  }
}

# Player directory and weekly stats cache readers
if (!exists("read_player_directory_cache")) {
  if (file.exists(repo_path("R/data/build_weekly_player_layers.R"))) {
    source_repo("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/data/build_weekly_player_layers.R not found")
  }
}
if (!exists("read_player_dim_cache")) {
  if (file.exists(repo_path("R/data/build_player_dim.R"))) {
    source_repo("R/data/build_player_dim.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/data/build_player_dim.R not found")
  }
}

if (!exists("read_player_week_identity_cache")) {
  if (file.exists(repo_path("R/data/build_weekly_player_layers.R"))) {
    source_repo("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_player_week_identity_cache not loaded")
  }
}

if (!exists("read_rb_weekly_features_cache")) {
  if (file.exists(repo_path("R/data/build_weekly_player_layers.R"))) {
    source_repo("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_rb_weekly_features_cache not loaded")
  }
}

if (!exists("read_wr_weekly_features_cache")) {
  if (file.exists(repo_path("R/data/build_weekly_player_layers.R"))) {
    source_repo("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_wr_weekly_features_cache not loaded")
  }
}

if (!exists("read_te_weekly_features_cache")) {
  if (file.exists(repo_path("R/data/build_weekly_player_layers.R"))) {
    source_repo("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_te_weekly_features_cache not loaded")
  }
}

if (!exists("read_qb_weekly_features_cache")) {
  if (file.exists(repo_path("R/data/build_weekly_player_layers.R"))) {
    source_repo("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_qb_weekly_features_cache not loaded")
  }
}

if (!exists("read_k_weekly_features_cache")) {
  if (file.exists(repo_path("R/data/build_weekly_player_layers.R"))) {
    source_repo("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_k_weekly_features_cache not loaded")
  }
}

# ============================================================================
# FEATURE BUILDERS (CACHE REFRESH SUPPORT)
# ============================================================================

if (!exists("build_team_defense_game_stats")) {
  if (file.exists(repo_path("R/data/build_team_defense_game_stats.R"))) {
    source_repo("R/data/build_team_defense_game_stats.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/data/build_team_defense_game_stats.R not found")
  }
}

if (!exists("build_team_defense_features")) {
  if (file.exists(repo_path("R/features/build_team_defense_features.R"))) {
    source_repo("R/features/build_team_defense_features.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/features/build_team_defense_features.R not found")
  }
}

if (!exists("build_team_offense_context")) {
  if (file.exists(repo_path("R/features/build_team_offense_context.R"))) {
    source_repo("R/features/build_team_offense_context.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/features/build_team_offense_context.R not found")
  }
}

if (!exists("build_qb_game_stats")) {
  if (file.exists(repo_path("R/data/build_qb_game_stats.R"))) {
    source_repo("R/data/build_qb_game_stats.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/data/build_qb_game_stats.R not found")
  }
}

if (!exists("build_qb_features")) {
  if (file.exists(repo_path("R/features/build_qb_features.R"))) {
    source_repo("R/features/build_qb_features.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/features/build_qb_features.R not found")
  }
}

if (!exists("build_prior_season_player_stats")) {
  if (file.exists(repo_path("R/features/build_prior_season_player_stats.R"))) {
    source_repo("R/features/build_prior_season_player_stats.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/features/build_prior_season_player_stats.R not found")
  }
}
message("Bootstrap feature builders loaded.")

# ============================================================================
# CACHE VALIDATION
# ============================================================================

skip_cache_check <- isTRUE(getOption("READTHEFIELD_SKIP_CACHE_CHECK", FALSE))
if (!skip_cache_check) {
  required_cache_files <- c(
    "data/cache/player_directory.parquet",
    "data/cache/player_week_identity.parquet",
    "data/cache/rb_weekly_stats.parquet",
    "data/cache/wr_weekly_stats.parquet",
    "data/cache/te_weekly_stats.parquet",
    "data/cache/qb_weekly_stats.parquet",
    "data/cache/k_weekly_stats.parquet",
    "data/processed/player_dim.parquet",
    "data/processed/rb_weekly_features.parquet",
    "data/processed/wr_weekly_features.parquet",
    "data/processed/te_weekly_features.parquet",
    "data/processed/qb_weekly_features.parquet",
    "data/processed/k_weekly_features.parquet"
  )
  missing_cache <- required_cache_files[!file.exists(repo_path(required_cache_files))]
  if (length(missing_cache) > 0) {
    stop(
      "Required caches missing: ",
      paste(missing_cache, collapse = ", "),
      ". Run scripts/refresh_weekly_cache.R."
    )
  }
}

# ============================================================================
# ASSEMBLY FUNCTIONS
# ============================================================================

# RB feature assembly (for training data preparation)
if (!exists("assemble_rb_weekly_features")) {
  if (file.exists(repo_path("R/assemble/assemble_rb_training_data.R"))) {
    source_repo("R/assemble/assemble_rb_training_data.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/assemble/assemble_rb_training_data.R not found")
  }
}

if (!exists("assemble_wr_weekly_features")) {
  if (file.exists(repo_path("R/positions/WR/assemble_wr_training_data.R"))) {
    source_repo("R/positions/WR/assemble_wr_training_data.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/assemble_wr_training_data.R not found")
  }
}

if (!exists("assemble_te_weekly_features")) {
  if (file.exists(repo_path("R/positions/TE/assemble_te_training_data.R"))) {
    source_repo("R/positions/TE/assemble_te_training_data.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/assemble_te_training_data.R not found")
  }
}

if (!exists("assemble_qb_weekly_features")) {
  if (file.exists(repo_path("R/positions/QB/assemble_qb_training_data.R"))) {
    source_repo("R/positions/QB/assemble_qb_training_data.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/QB/assemble_qb_training_data.R not found")
  }
}

if (!exists("assemble_k_weekly_features")) {
  if (file.exists(repo_path("R/positions/K/assemble_k_training_data.R"))) {
    source_repo("R/positions/K/assemble_k_training_data.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/K/assemble_k_training_data.R not found")
  }
}

# ============================================================================
# MODEL FITTING FUNCTIONS
# ============================================================================

# RB model fitting
if (!exists("fit_rb_models")) {
  if (file.exists(repo_path("R/models/fit_rb_models.R"))) {
    source_repo("R/models/fit_rb_models.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/models/fit_rb_models.R not found")
  }
}

if (!exists("fit_wr_models")) {
  if (file.exists(repo_path("R/positions/WR/fit_wr_models.R"))) {
    source_repo("R/positions/WR/fit_wr_models.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/fit_wr_models.R not found")
  }
}

if (!exists("fit_te_models")) {
  if (file.exists(repo_path("R/positions/TE/fit_te_models.R"))) {
    source_repo("R/positions/TE/fit_te_models.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/fit_te_models.R not found")
  }
}

if (!exists("fit_qb_models")) {
  if (file.exists(repo_path("R/positions/QB/fit_qb_models.R"))) {
    source_repo("R/positions/QB/fit_qb_models.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/QB/fit_qb_models.R not found")
  }
}

if (!exists("fit_k_models")) {
  if (file.exists(repo_path("R/positions/K/fit_k_models.R"))) {
    source_repo("R/positions/K/fit_k_models.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/K/fit_k_models.R not found")
  }
}

# ============================================================================
# SIMULATION CORE FUNCTIONS
# ============================================================================

# RB game simulation (Monte Carlo)
if (!exists("simulate_rb_game")) {
  if (file.exists(repo_path("R/simulation/simulate_rb_game.R"))) {
    source_repo("R/simulation/simulate_rb_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/simulate_rb_game.R not found")
  }
}

if (!exists("simulate_wr_game")) {
  if (file.exists(repo_path("R/positions/WR/simulate_wr_game.R"))) {
    source_repo("R/positions/WR/simulate_wr_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/simulate_wr_game.R not found")
  }
}

if (!exists("simulate_te_game")) {
  if (file.exists(repo_path("R/positions/TE/simulate_te_game.R"))) {
    source_repo("R/positions/TE/simulate_te_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/simulate_te_game.R not found")
  }
}

if (!exists("simulate_qb_game")) {
  if (file.exists(repo_path("R/positions/QB/simulate_qb_game.R"))) {
    source_repo("R/positions/QB/simulate_qb_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/QB/simulate_qb_game.R not found")
  }
}

if (!exists("simulate_k_game")) {
  if (file.exists(repo_path("R/positions/K/simulate_k_game.R"))) {
    source_repo("R/positions/K/simulate_k_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/K/simulate_k_game.R not found")
  }
}

# Player-game resolution
if (!exists("resolve_player_game")) {
  if (file.exists(repo_path("R/simulation/resolve_player_game.R"))) {
    source_repo("R/simulation/resolve_player_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/resolve_player_game.R not found")
  }
}
if (!exists("resolve_player_search")) {
  if (file.exists(repo_path("R/resolve/resolve_player_search.R"))) {
    source_repo("R/resolve/resolve_player_search.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/resolve/resolve_player_search.R not found")
  }
}

if (!exists("canonicalize_name")) {
  if (file.exists(repo_path("R/simulation/resolve_player_game.R"))) {
    source_repo("R/simulation/resolve_player_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: canonicalize_name not loaded")
  }
}

# Simulation mode policy
if (!exists("simulation_mode_policy")) {
  if (file.exists(repo_path("R/simulation/simulation_mode_policy.R"))) {
    source_repo("R/simulation/simulation_mode_policy.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/simulation_mode_policy.R not found")
  }
}

# Availability policy (counterfactual controls)
if (!exists("validate_availability_policy")) {
  if (file.exists(repo_path("R/simulation/availability_policy.R"))) {
    source_repo("R/simulation/availability_policy.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/availability_policy.R not found")
  }
}

# Feature row builder (availability-aware)
if (!exists("build_rb_feature_row_for_simulation")) {
  if (file.exists(repo_path("R/simulation/build_rb_feature_row_for_simulation.R"))) {
    source_repo("R/simulation/build_rb_feature_row_for_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_rb_feature_row_for_simulation.R not found")
  }
}

if (!exists("build_wr_feature_row_for_simulation")) {
  if (file.exists(repo_path("R/positions/WR/build_wr_feature_row_for_simulation.R"))) {
    source_repo("R/positions/WR/build_wr_feature_row_for_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/build_wr_feature_row_for_simulation.R not found")
  }
}

if (!exists("build_te_feature_row_for_simulation")) {
  if (file.exists(repo_path("R/positions/TE/build_te_feature_row_for_simulation.R"))) {
    source_repo("R/positions/TE/build_te_feature_row_for_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/build_te_feature_row_for_simulation.R not found")
  }
}

if (!exists("build_qb_feature_row_for_simulation")) {
  if (file.exists(repo_path("R/positions/QB/build_qb_feature_row_for_simulation.R"))) {
    source_repo("R/positions/QB/build_qb_feature_row_for_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/QB/build_qb_feature_row_for_simulation.R not found")
  }
}

if (!exists("build_k_feature_row_for_simulation")) {
  if (file.exists(repo_path("R/positions/K/build_k_feature_row_for_simulation.R"))) {
    source_repo("R/positions/K/build_k_feature_row_for_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/K/build_k_feature_row_for_simulation.R not found")
  }
}

# Future feature row builder
if (!exists("build_future_rb_feature_row")) {
  if (file.exists(repo_path("R/simulation/build_future_rb_feature_row.R"))) {
    source_repo("R/simulation/build_future_rb_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_rb_feature_row.R not found")
  }
}
if (!exists("build_future_wr_feature_row")) {
  if (file.exists(repo_path("R/simulation/build_future_wr_feature_row.R"))) {
    source_repo("R/simulation/build_future_wr_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_wr_feature_row.R not found")
  }
}
if (!exists("build_future_te_feature_row")) {
  if (file.exists(repo_path("R/simulation/build_future_te_feature_row.R"))) {
    source_repo("R/simulation/build_future_te_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_te_feature_row.R not found")
  }
}

if (!exists("build_future_qb_feature_row")) {
  if (file.exists(repo_path("R/simulation/build_future_qb_feature_row.R"))) {
    source_repo("R/simulation/build_future_qb_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_qb_feature_row.R not found")
  }
}

if (!exists("build_future_k_feature_row")) {
  if (file.exists(repo_path("R/simulation/build_future_k_feature_row.R"))) {
    source_repo("R/simulation/build_future_k_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_k_feature_row.R not found")
  }
}

# RB simulation runner
if (!exists("run_rb_simulation")) {
  if (file.exists(repo_path("R/simulation/run_rb_simulation.R"))) {
    source_repo("R/simulation/run_rb_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/run_rb_simulation.R not found")
  }
}

if (!exists("run_wr_simulation")) {
  if (file.exists(repo_path("R/positions/WR/run_wr_simulation.R"))) {
    source_repo("R/positions/WR/run_wr_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/run_wr_simulation.R not found")
  }
}

if (!exists("run_te_simulation")) {
  if (file.exists(repo_path("R/positions/TE/run_te_simulation.R"))) {
    source_repo("R/positions/TE/run_te_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/run_te_simulation.R not found")
  }
}

if (!exists("run_qb_simulation")) {
  if (file.exists(repo_path("R/positions/QB/run_qb_simulation.R"))) {
    source_repo("R/positions/QB/run_qb_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/QB/run_qb_simulation.R not found")
  }
}

if (!exists("run_k_simulation")) {
  if (file.exists(repo_path("R/positions/K/run_k_simulation.R"))) {
    source_repo("R/positions/K/run_k_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/K/run_k_simulation.R not found")
  }
}

# High-level player simulation
if (!exists("simulate_player_game")) {
  if (file.exists(repo_path("R/simulation/simulate_player_game.R"))) {
    source_repo("R/simulation/simulate_player_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/simulate_player_game.R not found")
  }
}

# ============================================================================
# VALIDATION: Ensure all critical functions are loaded
# ============================================================================

required_functions <- c(
  "get_rb_v1_targets",
  "get_rb_regimes",
  "get_model_key",
  "get_wr_v1_targets",
  "get_wr_regimes",
  "get_wr_model_key",
  "get_te_v1_targets",
  "get_te_regimes",
  "get_te_model_key",
  "get_qb_v1_targets",
  "get_qb_regimes",
  "get_qb_model_key",
  "get_k_v1_targets",
  "get_k_regimes",
  "get_k_model_key",
  "get_passing_defense_roll1_features",
  "get_passing_defense_roll3_features",
  "get_passing_defense_roll5_features",
  "get_passing_defense_all_features",
  "build_game_key",
  "read_parquet_cache",
  "get_available_seasons_from_cache",
  "load_schedules",
  "read_player_directory_cache",
  "read_player_dim_cache",
  "read_player_week_identity_cache",
  "read_rb_weekly_stats_cache",
  "read_rb_weekly_features_cache",
  "read_wr_weekly_stats_cache",
  "read_wr_weekly_features_cache",
  "read_te_weekly_stats_cache",
  "read_te_weekly_features_cache",
  "read_qb_weekly_stats_cache",
  "read_qb_weekly_features_cache",
  "read_k_weekly_stats_cache",
  "read_k_weekly_features_cache",
  "assemble_rb_weekly_features",
  "assemble_wr_weekly_features",
  "assemble_te_weekly_features",
  "assemble_qb_weekly_features",
  "assemble_k_weekly_features",
  "fit_rb_models",
  "fit_wr_models",
  "fit_te_models",
  "fit_qb_models",
  "fit_k_models",
  "simulate_rb_game",
  "simulate_wr_game",
  "simulate_te_game",
  "simulate_qb_game",
  "simulate_k_game",
  "validate_rb_models",
  "resolve_player_game",
  "resolve_player_search",
  "canonicalize_name",
  "simulation_mode_policy",
  "validate_availability_policy",
  "is_counterfactual_policy",
  "describe_policy",
  "availability_note",
  "build_rb_feature_row_for_simulation",
  "build_wr_feature_row_for_simulation",
  "build_te_feature_row_for_simulation",
  "build_qb_feature_row_for_simulation",
  "build_k_feature_row_for_simulation",
  "build_future_rb_feature_row",
  "build_future_wr_feature_row",
  "build_future_te_feature_row",
  "build_future_qb_feature_row",
  "build_future_k_feature_row",
  "run_rb_simulation",
  "run_wr_simulation",
  "run_te_simulation",
  "run_qb_simulation",
  "run_k_simulation",
  "simulate_player_game",
  "simulate_player_game_v1",
  "validate_simulation_request",
  "compute_ppr_rb"
)

missing_functions <- required_functions[!sapply(required_functions, exists)]

if (length(missing_functions) > 0) {
  stop("Simulation bootstrap incomplete: The following functions are not loaded: ",
       paste(missing_functions, collapse = ", "))
}
