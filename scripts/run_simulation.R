# Run Simulation

# This script orchestrates the complete simulation workflow for generating probabilistic predictions for matchups.
# It coordinates bootstrap resampling, model selection, simulation execution, and distribution summarization.
# The script handles simulation configuration including matchup definitions, model versions, and simulation parameters.
# Simulation execution produces probabilistic outcome distributions that can be used for decision-making and analysis.
# The script supports batch simulation of multiple matchups and generates aggregated results for reporting.
# This script serves as the primary interface for generating probabilistic forecasts using trained models and simulation methods.

# IMPORTANT: Position-Specific Model Selection
# - Select model based on player's position_group: QB, RB, WR, TE, K
# - WR players use WR model, TE players use TE model - never mix
# - Ensure correct model is loaded for each position_group
# - See docs/schema/modeling_base_v1.md for complete schema specification

