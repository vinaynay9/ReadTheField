# Validate Models

# This script orchestrates the complete model validation workflow including time split validation and calibration checks.
# It coordinates execution of validation modules to assess model performance, calibration, and interval coverage quality.
# The script loads trained models from the registry and evaluates them using appropriate validation strategies.
# Validation execution generates comprehensive performance reports, diagnostic outputs, and calibration assessments.
# The script supports validation of multiple model versions for comparison and selection of best-performing models.
# This script provides the primary interface for assessing model quality and ensuring models meet performance standards before deployment.

# IMPORTANT: Position-Specific Validation
# - Validate each model separately: QB, RB, WR, TE, K
# - WR models validated ONLY on WR player-games: filter(position_group == "WR")
# - TE models validated ONLY on TE player-games: filter(position_group == "TE")
# - Do NOT pool WR and TE validation data
# - Each position group requires separate validation metrics
# - See docs/schema/modeling_base_v1.md for complete schema specification