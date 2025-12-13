# Train Baseline Models

# This file contains functions for training baseline predictive models on assembled feature data.
# It implements standard modeling approaches including linear models, tree-based methods, and ensemble techniques.
# The module handles hyperparameter configuration, cross-validation setup, and model training orchestration.
# Baseline models serve as performance benchmarks and starting points for more sophisticated modeling approaches.
# Trained models are saved with metadata to the model registry for version tracking and deployment.
# This module provides the core functionality for converting feature data into predictive models ready for validation and use.

# IMPORTANT: Position-Specific Model Training
# - Models must be trained separately for each position_group: QB, RB, WR, TE, K
# - WR models MUST be trained ONLY on WR player-games: filter(position_group == "WR")
# - TE models MUST be trained ONLY on TE player-games: filter(position_group == "TE")
# - Do NOT pool WR and TE training data together
# - Each position group requires its own model instance
# - See docs/schema/modeling_base_v1.md for complete schema specification