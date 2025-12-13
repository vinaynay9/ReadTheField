# Assemble Features

# This file contains functions for combining individual feature sets into complete modeling-ready feature matrices.
# It handles the alignment and joining of player features, defense features, and game context features into unified datasets.
# The module manages missing value imputation, feature scaling, and data type conversions required for model compatibility.
# Feature assembly ensures temporal alignment so that features correspond to the correct time periods for each observation.
# The output produces final feature matrices that serve as inputs to model training and prediction workflows.
# This module acts as the final stage of feature engineering, preparing all features for consumption by modeling components.

# IMPORTANT: Position Group Filtering
# - position_group must be one of: QB, RB, WR, TE, K
# - WR and TE are SEPARATE position groups - do NOT pool them together
# - All filtering by position_group must be explicit: filter(position_group == "WR") or filter(position_group == "TE")
# - Feature engineering code can be shared, but filtering must be position-specific
# - See docs/schema/modeling_base_v1.md for complete schema specification