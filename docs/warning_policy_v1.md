# Warning Policy v1

Purpose: keep known, acceptable warnings from breaking production runs while
failing fast on unexpected warnings that could signal behavioral drift.

## Allowlisted warnings (regex)

- `glm.fit: fitted probabilities numerically 0 or 1 occurred`
- `glm.fit: fitted rates numerically 0 occurred`
- `Non-finite Gaussian mean prediction under played_only; using fitted-mean fallback\.`
- `RB simulation: [0-9]+ model\(s\) using baseline`
- `nflreadr::load_players\(\) returned empty result\. Using cached player_dim\.`
- `nflreadr::load_rosters\(\) returned empty result\. Using cached player_dim\.`
- `Download failed.*using cached data`

## Enforcement rules

- Allowed warnings are annotated in logs and suppressed for deterministic runs.
- Any warning outside the allowlist is treated as an error in the production gate.
- The allowlist is intentionally small; changes require review and justification.

## Notes

- This policy applies to production gate execution and the canonical API entry
  point `simulate_player_game_v1`.
