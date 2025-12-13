# Time Split Validation

# This file contains functions for performing time-based cross-validation on trained models.
# It implements temporal splitting strategies that respect chronological ordering to prevent data leakage from future to past.
# The module supports various validation schemes including expanding window, rolling window, and blocked time series splits.
# Time split validation evaluates model performance across different time periods to assess temporal stability and generalization.
# The validation process generates performance metrics and diagnostic outputs for each time period and fold.
# This module ensures that model evaluation reflects realistic deployment scenarios where models predict future outcomes based on past data.
