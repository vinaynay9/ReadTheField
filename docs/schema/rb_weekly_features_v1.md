# RB Weekly Features Schema v1

**Version:** 1.0  
**Last Updated:** 2025  
**Source:** `data/processed/rb_weekly_features.parquet`

## Overview

This document defines the schema for RB weekly features used in the ReadTheField simulation pipeline. Features are computed once during cache refresh and stored in parquet format. All rolling features use strictly lagged windows (no current-game leakage).

## Key Constraints

- **One row per (player_id, season, week)**: Each player-game observation is unique
- **Hard season boundary**: Week 1 has no prior data, rolling features are NA
- **No cross-season leakage**: Rolling windows do not span seasons
- **No temporal leakage**: Rolling features at week t use only weeks < t
- **Regular season only**: No preseason or playoff data

## Identity Columns

| Column | Type | Description | Notes |
|--------|------|-------------|-------|
| `player_id` | character | Unique player identifier | Required, no NA |
| `player_name` | character | Player display name | |
| `season` | integer | NFL season year | Required, no NA |
| `week` | integer | Week number (1-18) | Required, no NA |
| `team` | character | Team abbreviation | |
| `opponent` | character | Opponent team abbreviation | |
| `game_key` | character | Deterministic game identifier | |
| `game_date` | Date | Game date | |
| `gameday` | Date | Alias for game_date | |
| `home_away` | character | "HOME" or "AWAY" | |
| `is_home` | integer | 1 if home, 0 if away | |
| `position` | character | Always "RB" | |

## Rolling Feature Columns

All rolling features use lagged windows: the current observation is never included.

### Rolling Windows

- **3-game window**: Mean over previous 3 games
- **5-game window**: Mean over previous 5 games
- **7-game window**: Mean over previous 7 games
- **10-game window**: Mean over previous 10 games

### Volume Features

| Column | Window | Description | Units | Season Boundary Behavior |
|--------|--------|-------------|-------|---------------------------|
| `carries_roll3` | 3 | Mean carries over last 3 games | count | NA for weeks 1-3 |
| `carries_roll5` | 5 | Mean carries over last 5 games | count | NA for weeks 1-5 |
| `carries_roll7` | 7 | Mean carries over last 7 games | count | NA for weeks 1-7 |
| `carries_roll10` | 10 | Mean carries over last 10 games | count | NA for weeks 1-10 |
| `targets_roll3` | 3 | Mean targets over last 3 games | count | NA for weeks 1-3 |
| `targets_roll5` | 5 | Mean targets over last 5 games | count | NA for weeks 1-5 |
| `targets_roll7` | 7 | Mean targets over last 7 games | count | NA for weeks 1-7 |
| `targets_roll10` | 10 | Mean targets over last 10 games | count | NA for weeks 1-10 |

### Yardage Features

| Column | Window | Description | Units | Season Boundary Behavior |
|--------|--------|-------------|-------|---------------------------|
| `rush_yards_roll3` | 3 | Mean rushing yards over last 3 games | yards | NA for weeks 1-3 |
| `rush_yards_roll7` | 7 | Mean rushing yards over last 7 games | yards | NA for weeks 1-7 |
| `rush_yards_roll10` | 10 | Mean rushing yards over last 10 games | yards | NA for weeks 1-10 |
| `rec_yards_roll3` | 3 | Mean receiving yards over last 3 games | yards | NA for weeks 1-3 |
| `rec_yards_roll7` | 7 | Mean receiving yards over last 7 games | yards | NA for weeks 1-7 |
| `rec_yards_roll10` | 10 | Mean receiving yards over last 10 games | yards | NA for weeks 1-10 |

### Efficiency Features

| Column | Window | Description | Units | Season Boundary Behavior |
|--------|--------|-------------|-------|---------------------------|
| `yards_per_carry_roll5` | 5 | Ratio-of-sums: rush_yards / carries over last 5 games | yards/carry | NA for weeks 1-5 |
| `yards_per_target_roll5` | 5 | Ratio-of-sums: rec_yards / targets over last 5 games | yards/target | NA for weeks 1-5 |
| `catch_rate_roll5` | 5 | Ratio-of-sums: receptions / targets over last 5 games | proportion | NA for weeks 1-5 |

### Touchdown Features

| Column | Window | Description | Units | Season Boundary Behavior |
|--------|--------|-------------|-------|---------------------------|
| `rush_tds_roll5` | 5 | Mean rushing touchdowns over last 5 games | count | NA for weeks 1-5 |
| `rec_tds_roll5` | 5 | Mean receiving touchdowns over last 5 games | count | NA for weeks 1-5 |

## Target Columns (for Training)

| Column | Type | Description | Notes |
|--------|------|-------------|-------|
| `target_carries` | integer | Actual carries in this game | Used as training target |
| `target_rush_yards` | double | Actual rushing yards in this game | Used as training target |
| `target_rush_tds` | integer | Actual rushing touchdowns in this game | Used as training target |
| `target_targets` | integer | Actual targets in this game | Used as training target |
| `target_receptions` | integer | Actual receptions in this game | Used as training target |
| `target_rec_yards` | double | Actual receiving yards in this game | Used as training target |
| `target_rec_tds` | integer | Actual receiving touchdowns in this game | Used as training target |

## Season Boundary Rules

1. **Week 1**: All rolling features are NA (no prior games)
2. **Week 2**: Only 3-game rolling features may be non-NA (uses week 1 only)
3. **Week 3**: 3-game rolling features use weeks 1-2; 5-game+ features are NA
4. **Week N (N > window)**: Rolling features use weeks (N-window) through (N-1)

## Validation Rules

- No duplicate (player_id, season, week) rows
- No NA in player_id, season, or week
- Week 1 rolling features must be NA
- Rolling features at week t must not depend on week t or future weeks
- Rolling windows correctly truncated early-season

## Usage Notes

- Features are computed once during `scripts/refresh_weekly_cache.R`
- Simulation reads from cache only (no recomputation)
- Minimum 3 prior games required for simulation eligibility
- Regular season only (no preseason/playoff data)

