# ReadTheField

## Project Overview
ReadTheField simulates NFL running back (RB) stat distributions using a Monte Carlo engine. It produces probabilistic, matchup-aware projections (quantiles and outcome probabilities) instead of point forecasts. This is not a deterministic fantasy model; it is a distributional simulator with explicit uncertainty.

## High-level Architecture
- Data ingestion
  - `nflreadr` schedules and weekly player stats
  - cache-first loading with local parquet outputs
- Feature engineering layers
  - player-week identity and RB weekly stats
  - rolling player features and team/opponent context
  - prior-season aggregates and rookie/draft metadata
- Training vs simulation separation
  - global historical training data only
  - simulated player-week row never contaminates training
- Monte Carlo simulation engine
  - distributional outputs, not point estimates
- Policy layers
  - in-progress season handling (exclude from training)
  - availability policy (observed vs counterfactual rows)

## Simulation Philosophy
- Monte Carlo sampling captures variance and tail risk.
- Uncertainty intervals represent the distribution of plausible outcomes.
- Observed simulations use real player-week rows; counterfactual simulations construct a leakage-safe feature row when a player-week is missing or inactive.
- `availability_policy` controls whether missing/inactive player-weeks are allowed and how feature rows are constructed.

## Feature System (detailed)
Feature categories are explicit and time-aware. No missing values are imputed.

- Player rolling features (roll1 / roll3 / roll5)
  - Lagged usage and efficiency from prior games.
  - Included only when the rolling window exists; early weeks remain NA.
- Recent performance features
  - Cumulative career priors and decayed priors.
  - Included when derived from historical games; dropped in counterfactual modes if exposure is missing.
- Prior-season stats
  - Season-1 aggregates (carries, targets, yards, games).
  - Used as early-season signal; rookies have these set to NA.
- Draft and rookie indicators
  - `is_rookie`, `draft_round`, `draft_pick_overall`.
  - Provide explicit signal when prior-season stats are unavailable.
- Player physical attributes
  - Sourced from player metadata where available.
  - Used only as static, non-leaking signals.
- Team offensive context
  - Lagged team-level usage and passing context.
  - Joined by team and week.
- Opponent defensive rolling features
  - Defensive roll1 and roll5 features derived from opponent offense allowed.
  - Always lagged; no current-game leakage.
- Home/away indicator
  - Derived from schedule; no string parsing.
- Season phase handling
  - Early/mid/late/standard regimes select which feature sets are available.
  - Phase never changes feature computation, only model selection.

## Availability and Counterfactual Simulation
Availability controls whether a player-week row must exist and how to build a feature row when it does not.

- `played_only` (default)
  - Requires an observed player-week row with meaningful exposure.
  - Missing or inactive rows stop with a clear error.
- `expected_active`
  - If observed row is missing or inactive, builds a counterfactual row.
  - Drops exposure-dependent feature groups (player rolling + recent performance).
- `force_counterfactual`
  - Always builds a counterfactual row, even when an observed row exists.

Counterfactual rows use leakage-safe sources only:
prior-season stats, rookie/draft metadata, player attributes, team context, opponent defense, and schedule-derived game metadata.

## Data Integrity Guarantees
- No imputation: missing features remain NA.
- No leakage: rolling features exclude the current game and future weeks.
- No future data in training: in-progress seasons are excluded.
- Defensive features are derived only from opponent offensive output.

## How to Run
Refresh caches:
```
Rscript scripts/refresh_weekly_cache.R
```

## Local Demo (Frontend + API)
Run the API:
```
Rscript api/run_api.R
```

Run the frontend (static site):
```
cd frontend
python3 -m http.server 5173
```

Open the UI at `http://localhost:5173` and ensure the API is reachable at
`http://localhost:8000`.

Smoke test:
```
Rscript scripts/smoke_test_rb_simulation.R
```

Run a simulation:
```
Rscript scripts/run_rb_simulation_cli.R --player="Christian McCaffrey" --season=2024 --week=8 --n_sims=5000 --availability_policy=expected_active
```

## Repo Structure
- `R/` core simulation engine, features, models, policies, and printing
- `scripts/` cache refresh, smoke tests, and CLI entrypoint
- `data/` cached and processed artifacts (parquet)
- `docs/` schema and engineering docs
- `frontend/` static pages and assets for UI integration
- `api/` API documentation and placeholders
- `archive/` superseded scripts and historical notes
