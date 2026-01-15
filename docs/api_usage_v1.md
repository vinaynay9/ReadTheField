# API Usage v1

This document describes the public-facing simulation entry point:
`simulate_player_game_v1`.

## Entry point

```
simulate_player_game_v1(
  player_id,
  season,
  week,
  n_sims = NULL,
  availability_policy = "played_only",
  seed = NULL,
  schema_version = "v1"
)
```

### Required inputs

- `player_id`: gsis_id string (must exist in cache)
- `season`: integer season (must be available in cache)
- `week`: integer week (1–18)

### Optional inputs

- `n_sims`: integer number of simulations; defaults and caps are per-position
- `availability_policy`: `played_only`, `expected_active`, or `force_counterfactual`
- `seed`: integer; if provided, outputs are deterministic
- `schema_version`: must be `v1`

## Defaults and limits

Defaults by position:
- RB/WR/TE/QB: 1000 simulations
- K: 500 simulations

Hard caps:
- RB/WR/TE/QB: 20000 simulations
- K: 10000 simulations

Minimum:
- 100 simulations (all positions)

## Output contract

On success, returns a report matching `docs/report_schema_v1.md`.
Key guarantees:
- `metadata.report_schema_version` is `v1`
- `draws` includes canonical stat names
- `summary` includes required percentiles and stats per position
- `seed` is echoed in metadata

On error, returns a stable error object:

```
list(
  status = "error",
  error_type = "<invalid_input|data_unavailable|player_inactive|simulation_error|internal_error>",
  error_code = "<stable_code>",
  message = "<user-safe>",
  details = list(...),
  metadata = list(...)
)
```

## Speed vs accuracy

- Higher `n_sims` reduces Monte Carlo noise but increases latency.
- Use defaults for UI; use higher counts for offline analysis.
- Hard caps prevent expensive runs in production contexts.

## Determinism

- If `seed` is provided, the same inputs return identical outputs.
- If `seed` is NULL, outputs are non-deterministic by design.

## Known limitations

- Cache-only: simulations require cached data. Run `scripts/refresh_weekly_cache.R`.
- Week must be 1–18; preseason and playoffs are not supported.
- Counterfactuals depend on cache coverage and may be rejected if history is insufficient.

## Error semantics

Error types:
- `invalid_input`: malformed or unsupported inputs
- `data_unavailable`: cache missing required data or season
- `player_inactive`: player inactive or no snaps for played-only policy
- `simulation_error`: model or simulation failure
- `internal_error`: unexpected errors or missing dependencies

## Project stance

This demo prioritizes correctness, clarity, and usability over heavy
regression infrastructure.
