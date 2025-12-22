# Simulation Bootstrap - Load All Required Dependencies
#
# This file sources all required data, model, and utility files for RB simulation.
# It contains no execution logic - only dependency loading.
#
# Usage:
#   source("R/simulation/bootstrap_simulation.R")
#
# This ensures all simulation functions are available without relying on
# RStudio, global environment, or prior scripts.

# ============================================================================
# RB V1 SCHEMA (CRITICAL: LOAD FIRST)
# ============================================================================

# Load RB v1 schema (required for all RB simulations)
if (file.exists("R/utils/rb_schema_v1.R")) {
  source("R/utils/rb_schema_v1.R", local = TRUE)
} else {
  stop("Missing R/utils/rb_schema_v1.R — RB v1 schema is required for simulation")
}

# Hard guardrail
if (!exists("get_rb_v1_targets")) {
  stop("RB v1 schema not loaded: get_rb_v1_targets() missing")
}

# Load RB v1 regime system (required for time-aware modeling)
if (file.exists("R/utils/rb_regime_v1.R")) {
  source("R/utils/rb_regime_v1.R", local = TRUE)
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

if (file.exists("R/positions/passing_defense_features.R")) {
  source("R/positions/passing_defense_features.R", local = TRUE)
} else {
  stop("Missing R/positions/passing_defense_features.R - passing defense helpers required")
}

if (file.exists("R/positions/WR/wr_schema_v1.R")) {
  source("R/positions/WR/wr_schema_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/WR/wr_schema_v1.R - WR v1 schema is required for simulation")
}
if (!exists("get_wr_v1_targets")) {
  stop("WR v1 schema not loaded: get_wr_v1_targets() missing")
}
if (file.exists("R/positions/WR/wr_regime_v1.R")) {
  source("R/positions/WR/wr_regime_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/WR/wr_regime_v1.R - WR v1 regime system is required for simulation")
}
if (!exists("get_wr_regimes")) {
  stop("WR v1 regime system not loaded: get_wr_regimes() missing")
}
if (!exists("get_wr_model_key")) {
  stop("WR v1 regime system not loaded: get_wr_model_key() missing")
}

if (file.exists("R/positions/TE/te_schema_v1.R")) {
  source("R/positions/TE/te_schema_v1.R", local = TRUE)
} else {
  stop("Missing R/positions/TE/te_schema_v1.R - TE v1 schema is required for simulation")
}
if (!exists("get_te_v1_targets")) {
  stop("TE v1 schema not loaded: get_te_v1_targets() missing")
}
if (file.exists("R/positions/TE/te_regime_v1.R")) {
  source("R/positions/TE/te_regime_v1.R", local = TRUE)
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
# UTILITY FUNCTIONS
# ============================================================================

# Cache helpers (game keys, season utilities)
if (!exists("build_game_key")) {
  if (file.exists("R/utils/cache_helpers.R")) {
    source("R/utils/cache_helpers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/utils/cache_helpers.R not found")
  }
}

# PPR scoring functions (for print functions)
if (!exists("compute_ppr_rb")) {
  if (file.exists("R/utils/ppr_scoring.R")) {
    source("R/utils/ppr_scoring.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/utils/ppr_scoring.R not found")
  }
}

if (!exists("get_available_seasons_from_cache")) {
  if (file.exists("R/utils/cache_helpers.R")) {
    source("R/utils/cache_helpers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: get_available_seasons_from_cache not loaded")
  }
}

# ============================================================================
# DATA LAYER FUNCTIONS
# ============================================================================

# Schedule loading (for future game resolution)
if (!exists("load_schedules")) {
  if (file.exists("R/data/load_schedules.R")) {
    source("R/data/load_schedules.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/data/load_schedules.R not found")
  }
}

# Player directory and weekly stats cache readers
if (!exists("read_player_directory_cache")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/data/build_weekly_player_layers.R not found")
  }
}
if (!exists("read_player_dim_cache")) {
  if (file.exists("R/data/build_player_dim.R")) {
    source("R/data/build_player_dim.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/data/build_player_dim.R not found")
  }
}

if (!exists("read_player_week_identity_cache")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_player_week_identity_cache not loaded")
  }
}

if (!exists("read_rb_weekly_features_cache")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_rb_weekly_features_cache not loaded")
  }
}

if (!exists("read_wr_weekly_features_cache")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_wr_weekly_features_cache not loaded")
  }
}

if (!exists("read_te_weekly_features_cache")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: read_te_weekly_features_cache not loaded")
  }
}

# ============================================================================
# ASSEMBLY FUNCTIONS
# ============================================================================

# RB feature assembly (for training data preparation)
if (!exists("assemble_rb_weekly_features")) {
  if (file.exists("R/assemble/assemble_rb_training_data.R")) {
    source("R/assemble/assemble_rb_training_data.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/assemble/assemble_rb_training_data.R not found")
  }
}

if (!exists("assemble_wr_weekly_features")) {
  if (file.exists("R/positions/WR/assemble_wr_training_data.R")) {
    source("R/positions/WR/assemble_wr_training_data.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/assemble_wr_training_data.R not found")
  }
}

if (!exists("assemble_te_weekly_features")) {
  if (file.exists("R/positions/TE/assemble_te_training_data.R")) {
    source("R/positions/TE/assemble_te_training_data.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/assemble_te_training_data.R not found")
  }
}

# ============================================================================
# MODEL FITTING FUNCTIONS
# ============================================================================

# RB model fitting
if (!exists("fit_rb_models")) {
  if (file.exists("R/models/fit_rb_models.R")) {
    source("R/models/fit_rb_models.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/models/fit_rb_models.R not found")
  }
}

if (!exists("fit_wr_models")) {
  if (file.exists("R/positions/WR/fit_wr_models.R")) {
    source("R/positions/WR/fit_wr_models.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/fit_wr_models.R not found")
  }
}

if (!exists("fit_te_models")) {
  if (file.exists("R/positions/TE/fit_te_models.R")) {
    source("R/positions/TE/fit_te_models.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/fit_te_models.R not found")
  }
}

# ============================================================================
# SIMULATION CORE FUNCTIONS
# ============================================================================

# RB game simulation (Monte Carlo)
if (!exists("simulate_rb_game")) {
  if (file.exists("R/simulation/simulate_rb_game.R")) {
    source("R/simulation/simulate_rb_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/simulate_rb_game.R not found")
  }
}

if (!exists("simulate_wr_game")) {
  if (file.exists("R/positions/WR/simulate_wr_game.R")) {
    source("R/positions/WR/simulate_wr_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/simulate_wr_game.R not found")
  }
}

if (!exists("simulate_te_game")) {
  if (file.exists("R/positions/TE/simulate_te_game.R")) {
    source("R/positions/TE/simulate_te_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/simulate_te_game.R not found")
  }
}

# Player-game resolution
if (!exists("resolve_player_game")) {
  if (file.exists("R/simulation/resolve_player_game.R")) {
    source("R/simulation/resolve_player_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/resolve_player_game.R not found")
  }
}
if (!exists("resolve_player_search")) {
  if (file.exists("R/resolve/resolve_player_search.R")) {
    source("R/resolve/resolve_player_search.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/resolve/resolve_player_search.R not found")
  }
}

if (!exists("canonicalize_name")) {
  if (file.exists("R/simulation/resolve_player_game.R")) {
    source("R/simulation/resolve_player_game.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: canonicalize_name not loaded")
  }
}

# Simulation mode policy
if (!exists("simulation_mode_policy")) {
  if (file.exists("R/simulation/simulation_mode_policy.R")) {
    source("R/simulation/simulation_mode_policy.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/simulation_mode_policy.R not found")
  }
}

# Availability policy (counterfactual controls)
if (!exists("validate_availability_policy")) {
  if (file.exists("R/simulation/availability_policy.R")) {
    source("R/simulation/availability_policy.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/availability_policy.R not found")
  }
}

# Feature row builder (availability-aware)
if (!exists("build_rb_feature_row_for_simulation")) {
  if (file.exists("R/simulation/build_rb_feature_row_for_simulation.R")) {
    source("R/simulation/build_rb_feature_row_for_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_rb_feature_row_for_simulation.R not found")
  }
}

if (!exists("build_wr_feature_row_for_simulation")) {
  if (file.exists("R/positions/WR/build_wr_feature_row_for_simulation.R")) {
    source("R/positions/WR/build_wr_feature_row_for_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/build_wr_feature_row_for_simulation.R not found")
  }
}

if (!exists("build_te_feature_row_for_simulation")) {
  if (file.exists("R/positions/TE/build_te_feature_row_for_simulation.R")) {
    source("R/positions/TE/build_te_feature_row_for_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/build_te_feature_row_for_simulation.R not found")
  }
}

# Future feature row builder
if (!exists("build_future_rb_feature_row")) {
  if (file.exists("R/simulation/build_future_rb_feature_row.R")) {
    source("R/simulation/build_future_rb_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_rb_feature_row.R not found")
  }
}
if (!exists("build_future_wr_feature_row")) {
  if (file.exists("R/simulation/build_future_wr_feature_row.R")) {
    source("R/simulation/build_future_wr_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_wr_feature_row.R not found")
  }
}
if (!exists("build_future_te_feature_row")) {
  if (file.exists("R/simulation/build_future_te_feature_row.R")) {
    source("R/simulation/build_future_te_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_te_feature_row.R not found")
  }
}

# RB simulation runner
if (!exists("run_rb_simulation")) {
  if (file.exists("R/simulation/run_rb_simulation.R")) {
    source("R/simulation/run_rb_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/run_rb_simulation.R not found")
  }
}

if (!exists("run_wr_simulation")) {
  if (file.exists("R/positions/WR/run_wr_simulation.R")) {
    source("R/positions/WR/run_wr_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/WR/run_wr_simulation.R not found")
  }
}

if (!exists("run_te_simulation")) {
  if (file.exists("R/positions/TE/run_te_simulation.R")) {
    source("R/positions/TE/run_te_simulation.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/positions/TE/run_te_simulation.R not found")
  }
}

# High-level player simulation
if (!exists("simulate_player_game")) {
  if (file.exists("R/simulation/simulate_player_game.R")) {
    source("R/simulation/simulate_player_game.R", local = TRUE)
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
  "get_passing_defense_roll1_features",
  "get_passing_defense_roll3_features",
  "get_passing_defense_roll5_features",
  "get_passing_defense_all_features",
  "build_game_key",
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
  "assemble_rb_weekly_features",
  "assemble_wr_weekly_features",
  "assemble_te_weekly_features",
  "fit_rb_models",
  "fit_wr_models",
  "fit_te_models",
  "simulate_rb_game",
  "simulate_wr_game",
  "simulate_te_game",
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
  "build_future_rb_feature_row",
  "build_future_wr_feature_row",
  "build_future_te_feature_row",
  "run_rb_simulation",
  "run_wr_simulation",
  "run_te_simulation",
  "simulate_player_game",
  "compute_ppr_rb"
)

missing_functions <- required_functions[!sapply(required_functions, exists)]

if (length(missing_functions) > 0) {
  stop("Simulation bootstrap incomplete: The following functions are not loaded: ",
       paste(missing_functions, collapse = ", "))
}

