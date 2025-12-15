# Bootstrap Resampling

# This file contains functions for generating bootstrap resamples of training data for ensemble model development.
# It implements various bootstrap strategies including standard bootstrap, block bootstrap for time series, and stratified bootstrap.
# The module creates multiple training sets by resampling with replacement to capture uncertainty in model training.
# Bootstrap resampling enables construction of ensemble models and quantification of model uncertainty from training variability.
# The resampling process generates multiple training datasets that can be used to train ensemble members or assess training stability.
# This module supports uncertainty quantification by propagating training data variability through the modeling process.

