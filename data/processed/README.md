# Processed Data Directory

This directory contains standardized weekly aggregations derived from raw data sources. The player_weekly.parquet file contains weekly aggregated statistics for individual players. The defense_weekly.parquet file contains weekly aggregated statistics for defensive units. The game_context.parquet file contains game-level contextual information. The modeling_base.parquet file contains the final joined dataset combining all processed sources into a unified modeling-ready format. These processed files serve as inputs to the feature engineering pipeline and are refreshed whenever new raw data is ingested.

