# Saved Models Directory

This directory stores serialized model objects and associated artifacts produced by the model training pipeline. Each saved model includes the trained model object, metadata about training configuration, and references to the data and features used during training. Models are organized by version identifiers to support model versioning and comparison workflows. The directory structure enables the validation and simulation modules to load specific model versions for evaluation and prediction generation. Saved models serve as the source of truth for model artifacts that will be consumed by downstream components including the API and frontend interfaces.

