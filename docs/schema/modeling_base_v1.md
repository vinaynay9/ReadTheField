# Modeling Base Schema v1

## Overview

The `modeling_base.parquet` file is the core modeling dataset for Read the Field. It contains one row per player-game combination, with features computed strictly from information available **before** kickoff of that game. This ensures no data leakage when training models or making predictions.

### Purpose

This dataset serves as the foundation for:
- **Model Training**: Features are used to predict post-game outcomes (targets)
- **Prediction Generation**: Pre-game features enable real-time predictions before games start
- **Temporal Validation**: The leakage-safe design allows proper time-based cross-validation
- **Feature Analysis**: Understanding which pre-game factors correlate with outcomes

### Key Design Principles

1. **No Future Leakage**: All feature columns must be computed from data strictly before kickoff
2. **Rolling Windows**: Features use rolling aggregations (last 3, last 5 games) to capture recent trends
3. **Position-Specific**: Different feature sets for QB, RB, WR, TE, and K positions
4. **Context-Aware**: Includes team offensive context and opponent defensive context
5. **Temporal Ordering**: Strict ordering by (season, week, gameday) ensures correct rolling computations

---

## Schema Specification

### A. Identifiers and Metadata (Always Present)

These columns uniquely identify each row and provide basic game/player context.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `game_id` | character | id | Unique game identifier | from schedules / player stats table |
| `season` | integer | id | NFL season year | from schedules / player stats table |
| `week` | integer | id | Week number | from schedules |
| `gameday` | Date | id | Game date | from schedules |
| `player_id` | character | id | Unique player identifier | from player stats (gsis_id if available) |
| `player_name` | character | meta | Display name | from player stats / rosters |
| `position` | character | meta | Position (QB/RB/WR/TE/K) | from rosters/player stats |
| `position_group` | character | meta | One of QB, RB, WR, TE, K | map: QB→QB, RB→RB, WR→WR, TE→TE, K→K |
| `team` | character | meta | Player's team for that game | from player stats |
| `opponent` | character | meta | Opponent team | from schedules (home/away mapping) |
| `home_away` | character | meta | HOME or AWAY | from schedules mapping |
| `is_home` | integer | meta | 1 if home else 0 | as.integer(home_away == "HOME") |
| `stadium` | character | meta | Stadium name (optional) | from schedules (nullable) |
| `surface` | character | meta | Surface type (optional) | from schedules (nullable) |

**Purpose**: These columns provide the primary key (`game_id`, `player_id`) and essential context for every observation. They enable joins, filtering, and grouping operations throughout the modeling pipeline.

---

### B. Player Bio and Career Context (Pre-Game)

Physical attributes and career experience that remain relatively static but may influence performance.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `height_in` | double | feature | Player height in inches | from rosters; static by player |
| `weight_lb` | double | feature | Player weight in pounds | from rosters; static by player |
| `age` | double | feature | Age at gameday (years) | (gameday - birth_date)/365.25 |
| `experience_years` | integer | feature | NFL experience entering season | from rosters if available; else derive from first season played |

**Purpose**: Captures player physical attributes and career stage. Age and experience can correlate with performance trends (rookie adjustments, veteran consistency, age-related decline). These are static or slowly-changing features that don't require rolling windows.

---

### C. Availability and Usage Proxies (Pre-Game)

Measures of player availability and role/usage trends. Critical for identifying injury status and role changes.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `snap_share_roll3` | double | feature | Mean snap share over last 3 prior games | player weekly snaps: offense_snaps / team_offense_snaps, then rolling mean |
| `snap_share_roll5` | double | feature | Mean snap share over last 5 prior games | rolling mean |
| `snap_share_prev` | double | feature | Snap share in immediately prior game | lag(1) |
| `inactive_prev_week` | integer | feature | 1 if player had no offensive snaps in prior week while team played | infer from missing snap record for that week (careful with bye) |
| `games_missed_last4` | integer | feature | Count of prior 4 team games with zero offensive snaps | rolling sum over team games |

**Purpose**: Snap share is the cleanest proxy for:
- **Injury Status**: Sudden drops indicate injury
- **Role Changes**: Usage trends show increasing/decreasing involvement
- **Availability**: Recent inactivity flags potential DNP or limited role

**Note**: If snap data is unavailable in v1, this entire block can be omitted. However, snap share is highly predictive and should be prioritized if data sources allow.

---

### D. Game Context (Pre-Game)

Game-level contextual factors that may influence performance.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `rest_days_team` | integer | feature | Days since team's last game | gameday - lag(gameday) by team |
| `rest_days_opp` | integer | feature | Days since opponent's last game | same, but on opponent |
| `rest_diff` | integer | feature | rest_days_team - rest_days_opp | difference |
| `is_division_game` | integer | feature | 1 if teams share division (optional) | join team/division map |
| `week_in_season` | integer | feature | same as week (optional) | derived |

**Purpose**: Captures scheduling factors:
- **Rest Days**: Short weeks (Thursday games) vs. extended rest can affect performance
- **Rest Differential**: Relative rest advantage/disadvantage
- **Division Games**: Often more competitive, may have different game scripts
- **Week in Season**: Early season adjustments vs. late season playoff implications

---

### E. Team Offense Context (Pre-Game)

Aggregated team-level offensive tendencies computed from prior games only.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `team_pass_attempts_roll5` | double | feature | Mean team pass attempts over last 5 games | team game totals, rolling mean |
| `team_rush_attempts_roll5` | double | feature | Mean team rush attempts over last 5 games | rolling mean |
| `team_points_roll5` | double | feature | Mean team points scored over last 5 games | rolling mean |

**Purpose**: Captures team offensive identity and recent performance:
- **Play Calling Tendency**: Pass-heavy vs. run-heavy teams create different opportunity structures
- **Offensive Production**: Recent scoring trends indicate team form
- **Game Script Context**: High-scoring teams create more fantasy opportunities

**Computation**: These are computed at the team-game level, then joined back to player rows. Only prior games are used to avoid leakage.

---

### F. Opponent Defense Context (Pre-Game)

Opponent defensive performance metrics computed from what they "allowed" in prior games.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `opp_pass_yards_allowed_roll5` | double | feature | Mean pass yards allowed by opponent (last 5 games) | opponent defensive allowed totals, rolling mean |
| `opp_rush_yards_allowed_roll5` | double | feature | Mean rush yards allowed (last 5) | rolling mean |
| `opp_pass_tds_allowed_roll5` | double | feature | Mean pass TDs allowed (last 5) | rolling mean |
| `opp_rush_tds_allowed_roll5` | double | feature | Mean rush TDs allowed (last 5) | rolling mean |
| `opp_points_allowed_roll5` | double | feature | Mean points allowed (last 5) | rolling mean |

**Purpose**: Captures opponent defensive strength/weakness:
- **Matchup Quality**: Weak defenses create better opportunities for offensive players
- **Positional Matchups**: Pass defense vs. rush defense informs QB/RB/WR/TE projections differently
- **Touchdown Likelihood**: TD rates allowed correlate with scoring opportunities

**Computation**: For each game, compute what the opponent "allowed" by aggregating the opposing team's offensive stats. Then compute rolling means of these allowed metrics for the opponent team.

---

### G. QB Features (Pre-Game)

Position-specific features for quarterbacks. Only populated for rows where `position_group == "QB"`.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `qb_pass_att_roll3` | double | feature | Mean pass attempts last 3 games | player game stats, rolling mean |
| `qb_pass_att_roll5` | double | feature | Mean pass attempts last 5 | rolling mean |
| `qb_rush_att_roll3` | double | feature | Mean rush attempts last 3 | rolling mean |
| `qb_rush_att_roll5` | double | feature | Mean rush attempts last 5 | rolling mean |
| `qb_yards_per_att_roll5` | double | feature | Pass yards / pass att over last 5 (ratio-of-sums) | sum(pass_yards)/sum(pass_att) window |
| `qb_td_rate_roll5` | double | feature | Pass TD / pass att over last 5 (ratio-of-sums) | ratio-of-sums |
| `qb_int_rate_roll5` | double | feature | INT / pass att over last 5 (ratio-of-sums) | ratio-of-sums |
| `qb_pass_yards_roll3` | double | feature | Mean pass yards last 3 | rolling mean |
| `qb_pass_tds_roll3` | double | feature | Mean pass TD last 3 | rolling mean |
| `qb_rush_yards_roll3` | double | feature | Mean rush yards last 3 | rolling mean |
| `qb_fantasy_ppr_roll3` | double | feature | Mean PPR points last 3 (optional) | compute from prior targets only |
| `qb_fantasy_ppr_roll5` | double | feature | Mean PPR points last 5 (optional) | same |

**Purpose**: Captures QB-specific usage and efficiency:
- **Volume**: Pass/rush attempts indicate opportunity level
- **Efficiency**: Yards per attempt, TD rate, INT rate measure performance quality
- **Recent Form**: Rolling means capture recent trends vs. season-long averages
- **Dual Threat**: Rush attempts/yards for mobile QBs

**Note**: Ratio features (yards_per_att, td_rate) use ratio-of-sums to avoid small-sample bias from games with few attempts.

---

### H. RB Features (Pre-Game)

Position-specific features for running backs. Only populated for rows where `position_group == "RB"`.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `rb_carries_roll3` | double | feature | Mean carries last 3 | rolling mean |
| `rb_carries_roll5` | double | feature | Mean carries last 5 | rolling mean |
| `rb_targets_roll3` | double | feature | Mean targets last 3 | rolling mean |
| `rb_targets_roll5` | double | feature | Mean targets last 5 | rolling mean |
| `rb_yards_per_carry_roll5` | double | feature | Rush yards / carries (last 5, ratio-of-sums) | ratio-of-sums |
| `rb_yards_per_target_roll5` | double | feature | Rec yards / targets (last 5, ratio-of-sums) | ratio-of-sums |
| `rb_rush_yards_roll3` | double | feature | Mean rush yards last 3 | rolling mean |
| `rb_rec_yards_roll3` | double | feature | Mean rec yards last 3 | rolling mean |
| `rb_total_tds_roll5` | double | feature | Mean total TD last 5 | rolling mean |
| `rb_fumbles_roll5` | double | feature | Mean fumbles last 5 | rolling mean |

**Purpose**: Captures RB-specific usage and efficiency:
- **Volume**: Carries and targets indicate opportunity (rushing + receiving)
- **Efficiency**: Yards per carry and yards per target measure performance
- **Scoring**: Total TDs (rush + receiving) capture scoring opportunity
- **Ball Security**: Fumble rate (negative indicator)

**Note**: RBs are dual-threat players, so both rushing and receiving metrics are critical.

---

### I. WR Features (Pre-Game)

Position-specific features for wide receivers. Only populated for rows where `position_group == "WR"`.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `wr_targets_roll3` | double | feature | Mean targets last 3 | rolling mean |
| `wr_targets_roll5` | double | feature | Mean targets last 5 | rolling mean |
| `wr_catch_rate_roll5` | double | feature | Receptions / targets (last 5, ratio-of-sums) | ratio-of-sums |
| `wr_yards_per_target_roll5` | double | feature | Rec yards / targets (last 5, ratio-of-sums) | ratio-of-sums |
| `wr_yac_roll5` | double | feature | Mean YAC last 5 (optional) | if available in source |
| `wr_rec_yards_roll3` | double | feature | Mean rec yards last 3 | rolling mean |
| `wr_rec_tds_roll5` | double | feature | Mean rec TD last 5 | rolling mean |
| `wr_receptions_roll3` | double | feature | Mean receptions last 3 | rolling mean |

**Purpose**: Captures WR-specific usage and efficiency:
- **Volume**: Targets are the primary opportunity metric for pass-catchers
- **Efficiency**: Catch rate and yards per target measure performance
- **YAC**: Yards after catch (if available) indicates playmaking ability
- **Scoring**: Receiving TDs capture red-zone usage

**Note**: WR features are computed separately from TE features to allow independent modeling of wide receiver performance patterns.

---

### J. TE Features (Pre-Game)

Position-specific features for tight ends. Only populated for rows where `position_group == "TE"`.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `te_targets_roll3` | double | feature | Mean targets last 3 | rolling mean |
| `te_targets_roll5` | double | feature | Mean targets last 5 | rolling mean |
| `te_catch_rate_roll5` | double | feature | Receptions / targets (last 5, ratio-of-sums) | ratio-of-sums |
| `te_yards_per_target_roll5` | double | feature | Rec yards / targets (last 5, ratio-of-sums) | ratio-of-sums |
| `te_yac_roll5` | double | feature | Mean YAC last 5 (optional) | if available in source |
| `te_rec_yards_roll3` | double | feature | Mean rec yards last 3 | rolling mean |
| `te_rec_tds_roll5` | double | feature | Mean rec TD last 5 | rolling mean |
| `te_receptions_roll3` | double | feature | Mean receptions last 3 | rolling mean |

**Purpose**: Captures TE-specific usage and efficiency:
- **Volume**: Targets are the primary opportunity metric for pass-catchers
- **Efficiency**: Catch rate and yards per target measure performance
- **YAC**: Yards after catch (if available) indicates playmaking ability
- **Scoring**: Receiving TDs capture red-zone usage

**Note**: TE features are computed separately from WR features to allow independent modeling of tight end performance patterns, which may differ due to blocking responsibilities and route running roles.

---

### K. K Features (Pre-Game)

Position-specific features for kickers. Only populated for rows where `position_group == "K"`.

| Column | Type | Role | Definition | Compute |
|--------|------|------|------------|---------|
| `k_fg_att_roll5` | double | feature | Mean FG attempts last 5 | rolling mean |
| `k_fg_made_roll5` | double | feature | Mean FG made last 5 | rolling mean |
| `k_pat_att_roll5` | double | feature | Mean XP attempts last 5 | rolling mean |
| `k_pat_made_roll5` | double | feature | Mean XP made last 5 | rolling mean |
| `k_long_season_to_date` | double | feature | Longest FG made prior this game (season) | rolling max over season prior games |

**Purpose**: Captures kicker-specific usage and range:
- **Volume**: FG and XP attempts indicate opportunity (driven by team offense)
- **Accuracy**: Made/attempt ratios (can be derived from these features)
- **Range**: Longest FG made shows leg strength and coaching confidence

**Note**: Kicker performance is highly dependent on team offensive production (more scoring = more XP/FG opportunities).

---

## Targets (Post-Game Realized Stats)

These columns contain the actual outcomes from the game. They are **never** used as features for the same row, only as training targets or validation labels.

### QB Targets

| Column | Type | Role | Definition |
|--------|------|------|------------|
| `target_qb_pass_yards` | double | target | Pass yards in game |
| `target_qb_pass_tds` | double | target | Pass TD in game |
| `target_qb_rush_yards` | double | target | Rush yards in game |
| `target_qb_rush_tds` | double | target | Rush TD in game |
| `target_qb_pass_att` | double | target | Pass attempts in game |
| `target_qb_rush_att` | double | target | Rush attempts in game |
| `target_qb_completions` | double | target | Completions in game |
| `target_ppr_fantasy_off` | double | target | PPR fantasy points (offense only) |

### RB Targets

| Column | Type | Role | Definition |
|--------|------|------|------------|
| `target_rb_rush_yards` | double | target | Rush yards |
| `target_rb_rec_yards` | double | target | Receiving yards |
| `target_rb_total_tds` | double | target | Total TD (rush+rec) |
| `target_rb_carries` | double | target | Carries |
| `target_ppr_fantasy_off` | double | target | PPR points |

### WR Targets

| Column | Type | Role | Definition |
|--------|------|------|------------|
| `target_wr_receptions` | double | target | Receptions |
| `target_wr_rec_yards` | double | target | Receiving yards |
| `target_wr_rec_tds` | double | target | Receiving TD |
| `target_ppr_fantasy_off` | double | target | PPR points |

### TE Targets

| Column | Type | Role | Definition |
|--------|------|------|------------|
| `target_te_receptions` | double | target | Receptions |
| `target_te_rec_yards` | double | target | Receiving yards |
| `target_te_rec_tds` | double | target | Receiving TD |
| `target_ppr_fantasy_off` | double | target | PPR points |

### K Targets

| Column | Type | Role | Definition |
|--------|------|------|------------|
| `target_k_fg_made` | double | target | FG made |
| `target_k_fg_att` | double | target | FG attempted |
| `target_k_pat_att` | double | target | XP attempted |
| `target_k_pat_made` | double | target | XP made |

**Purpose**: Targets serve as:
- **Training Labels**: Models learn to predict these from pre-game features
- **Validation Metrics**: Compare predictions to actuals to measure model performance
- **Analysis**: Understand which features correlate with which outcomes

**PPR Fantasy Scoring**: The `target_ppr_fantasy_off` column uses standard PPR scoring:
- Receptions: 1 point each
- Receiving yards: 0.1 points per yard
- Receiving TDs: 6 points
- Rushing yards: 0.1 points per yard
- Rushing TDs: 6 points
- Passing yards: 0.04 points per yard
- Passing TDs: 4 points
- Interceptions: -2 points
- (Fumbles lost: -2 points, if tracked)

---

## Data Structure and Constraints

### Primary Key
- **Unique Identifier**: (`game_id`, `player_id`)
- **Constraint**: Exactly one row per player-game combination

### Temporal Ordering
- **Sort Key**: (`season`, `week`, `gameday`) within each player
- **Critical**: Rolling features depend on correct temporal ordering to avoid leakage

### Missing Values
- **Identifiers**: `game_id`, `player_id`, `season`, `week`, `gameday` must never be NA
- **Metadata**: `team`, `opponent`, `position_group`, `is_home` should never be NA
- **Rolling Features**: Can be NA early in a player's career (insufficient history)
- **Position Features**: NA for positions that don't match the feature group (e.g., QB features are NA for RB rows)

### Data Types
- **character**: IDs, names, categorical strings
- **integer**: Flags (0/1), counts, week numbers
- **double**: Continuous features, ratios, means
- **Date**: `gameday` column

---

## Usage in Modeling Pipeline

### Training Phase
1. **Feature Extraction**: Models use all feature columns (A-K) as inputs
2. **Target Extraction**: Models use position-appropriate target columns as outputs
3. **Temporal Splits**: Time-based cross-validation ensures no future leakage
4. **Missing Handling**: Models must handle NA values in rolling features (early career, position mismatches)

### Prediction Phase
1. **Feature Assembly**: For upcoming games, compute all pre-game features
2. **Model Inference**: Pass features to trained models to generate predictions
3. **Uncertainty Quantification**: Models produce distributions, not just point estimates
4. **Post-Game Validation**: After games, compare predictions to actual targets

### Validation Phase
1. **Leakage Checks**: Verify no features use information from the same game
2. **Temporal Validity**: Ensure rolling windows only use prior games
3. **Coverage Analysis**: Check that predicted intervals match observed outcomes
4. **Calibration**: Verify predicted probabilities align with actual frequencies

---

## Implementation Notes

### Rolling Window Computation
- **lag(1)** ensures the current game is excluded
- **roll3**: mean of lag(1), lag(2), lag(3)
- **roll5**: mean of lag(1) through lag(5)
- **ratio-of-sums**: For efficiency metrics, use `sum(numerator) / sum(denominator)` over the window, not mean of ratios

### Position Grouping
- QB: Quarterbacks only
- RB: Running backs only
- WR: Wide receivers only
- TE: Tight ends only
- K: Kickers only

### Team/Opponent Context
- Team features: computed from the player's team's prior games
- Opponent features: computed from the opponent team's prior games (what they "allowed")
- Both use rolling windows with lag to ensure pre-game only

### Snap Share (if available)
- Requires player-level and team-level snap counts
- Must handle bye weeks carefully (team didn't play vs. player didn't play)
- Create explicit zeros when team played but player had zero snaps

---

## Future Extensions (Out of Scope for v1)

- Defensive player features
- Play-by-play micro-features
- Weather/stadium-specific features (beyond basic surface type)
- Injury report integration (beyond snap share proxies)
- Advanced opponent matchups (cornerback vs. wide receiver)
- Game script features (implied by team context, but not explicit)

---

## Validation Checklist

Before using this dataset, verify:

- [ ] Uniqueness: `nrow(df) == nrow(distinct(df, game_id, player_id))`
- [ ] No future leakage: For each row, all rolling features use only games with earlier `gameday`
- [ ] Temporal ordering: Within each player, rows are sorted by `season`, `week`, `gameday`
- [ ] Missingness: Required columns (identifiers, metadata) have no NA values
- [ ] Position features: QB features are NA for non-QB rows, WR features are NA for non-WR rows, TE features are NA for non-TE rows, etc.
- [ ] Type consistency: Integer columns are integer, flags are 0/1, dates are Date type
- [ ] Target completeness: Targets are populated for all rows (may be 0, but not NA)

