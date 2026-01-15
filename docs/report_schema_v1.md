# Report Schema v1

This schema defines the API-safe simulation report returned by
`simulate_player_game_v1`.

## Top-level fields

- `metadata` (list)
- `summary` (data.frame)
- `draws` (data.frame)
- `recent_games` (data.frame; may be empty)
- `diagnostics` (list; may be empty)

## Required metadata fields

- `player_id` (string)
- `season` (integer)
- `week` (integer)
- `n_sims` (integer)
- `position` (RB/WR/TE/QB/K)
- `availability_policy` (string)
- `report_schema_version` (`v1`)
- `seed` (integer or NA)
- `error_code` (string or NA)

## Required summary stats (by position)

RB:
- `carries`, `rushing_yards`, `receptions`, `receiving_yards`,
  `total_touchdowns`, `fantasy_ppr`

WR/TE:
- `targets`, `receptions`, `receiving_yards`, `receiving_tds`,
  `total_touchdowns`, `fantasy_ppr`

QB:
- `passing_attempts`, `passing_yards`, `passing_tds`,
  `interceptions_thrown`, `qb_sacks_taken`,
  `qb_rush_attempts`, `qb_rush_yards`, `qb_rush_tds`

K:
- `fg_attempts`, `fg_made`, `pat_made`, `fantasy_ppr`

## Required draws columns

The `draws` data.frame must include the same canonical stat names as the
summary (plus `total_yards` for RB/WR/TE).
