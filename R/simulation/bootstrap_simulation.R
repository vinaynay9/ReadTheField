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

# Future feature row builder
if (!exists("build_future_rb_feature_row")) {
  if (file.exists("R/simulation/build_future_rb_feature_row.R")) {
    source("R/simulation/build_future_rb_feature_row.R", local = TRUE)
  } else {
    stop("Simulation bootstrap incomplete: R/simulation/build_future_rb_feature_row.R not found")
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
  "build_game_key",
  "get_available_seasons_from_cache",
  "load_schedules",
  "read_player_directory_cache",
  "read_player_dim_cache",
  "read_player_week_identity_cache",
  "read_rb_weekly_features_cache",
  "assemble_rb_weekly_features",
  "fit_rb_models",
  "simulate_rb_game",
  "validate_rb_models",
  "resolve_player_game",
  "resolve_player_search",
  "canonicalize_name",
  "simulation_mode_policy",
  "build_future_rb_feature_row",
  "run_rb_simulation",
  "simulate_player_game",
  "compute_ppr_rb"
)

missing_functions <- required_functions[!sapply(required_functions, exists)]

if (length(missing_functions) > 0) {
  stop("Simulation bootstrap incomplete: The following functions are not loaded: ",
       paste(missing_functions, collapse = ", "))
}

