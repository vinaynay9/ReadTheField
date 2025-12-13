# Train Models

# This script orchestrates the complete model training workflow from feature assembly through model registration.
# It coordinates execution of feature engineering modules, model training functions, and model registry operations.
# The script handles configuration management, data loading, and orchestration of the training pipeline stages.
# Model training execution produces trained model objects that are saved to the model registry with full metadata.
# The script supports training multiple model types and configurations in a single execution for comparison purposes.
# This script serves as the primary interface for retraining models when new data becomes available or when model updates are needed.

# IMPORTANT: Position Group Separation
# - Train 5 separate models: QB, RB, WR, TE, K
# - Each model must filter training data by position_group explicitly
# - WR and TE models are completely independent - no data pooling
# - Example: train_qb_model(filter(df, position_group == "QB"))
# - See docs/schema/modeling_base_v1.md for complete schema specification