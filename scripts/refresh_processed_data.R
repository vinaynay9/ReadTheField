# Refresh Processed Data

# This script orchestrates the transformation of raw data into processed weekly aggregations ready for feature engineering.
# It executes data processing pipelines that convert raw event-level or game-level data into standardized weekly datasets.
# The script generates processed files for players, defenses, and game contexts according to defined aggregation rules.
# Processed data refresh ensures that feature engineering modules have access to current standardized data formats.
# The script handles incremental updates and full refreshes depending on data availability and processing requirements.
# This script bridges the gap between raw data storage and feature engineering by producing clean, aggregated datasets.

