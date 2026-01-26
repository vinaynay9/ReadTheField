# Fantasy Simulation Roadmap (v2)

This document describes a forward-looking plan for a fantasy matchup simulation
layer built on top of the existing player-level Monte Carlo engine (v1). It is
intentionally scoped as a future extension and does not modify v1 behavior.

## 1. Motivation and Use Case

Fantasy decisions benefit from distributions, not point estimates. The goal is
to estimate:
- weekly fantasy outcomes for a roster
- probability of winning, losing, or tying a matchup
- uncertainty bands around expected scores

This builds directly on the existing player-level simulation engine:
`simulate_player_game_v1` already produces draw-level outcomes per player. The
fantasy layer aggregates those draws into team-level results.

## 2. High-Level Concept

Each player’s Monte Carlo draws are mapped to fantasy points, then aggregated
across a roster to produce a distribution of team scores. This is a layer on
top of v1, not a rewrite.

Monte Carlo is appropriate because:
- fantasy outcomes are driven by non-linear scoring
- variance matters more than a single projection
- head-to-head matchups are probabilistic by nature

## 3. User Input Model

Minimum required inputs:
- rostered players (player_id list)
- opponent roster (manual entry acceptable in v2 phase 1)
- scoring rules (PPR, half-PPR, standard)

Tradeoffs:
- Manual roster entry is accurate but less convenient.
- Automated imports improve UX but risk stale or mismatched rosters.
- Scoring configuration must balance flexibility with clear defaults.

## 4. Backend Architecture (Proposed)

New components (conceptual):
- `fantasy_roster`: normalized list of players, positions, bench flags
- `fantasy_scoring`: mapping from stat draws to fantasy points
- `matchup_simulation`: orchestrator that calls v1 and aggregates results

Interaction with v1:
- `matchup_simulation` calls `simulate_player_game_v1` per player
- uses existing schema validation and error handling
- reuses draw-level stats for scoring without altering model logic

## 5. Simulation Flow (Step-by-Step)

1. Simulate each player-week with `simulate_player_game_v1`
2. Convert draw-level stats into fantasy points
3. Aggregate points per roster for each draw
4. Compute win/loss/tie probabilities
5. Summarize for display (mean, median, percentiles)

This flow keeps the fantasy layer decoupled from the core model.

## 6. Output and UX Considerations

Example outputs:
- win probability
- expected score and median score
- percentile bands (25/50/75 or 10/50/90)
- head-to-head score distribution

Frontend usage:
- show probability-first summaries
- allow users to inspect key contributors
- highlight volatility and upset risk

## 7. Key Technical Challenges

- Correlation between teammates (QB/WR stacks, game scripts)
- Opponent modeling (defense strength and tempo interactions)
- Injury and availability uncertainty
- Scoring system flexibility without branching logic
- Runtime and caching for many-player simulations

These challenges suggest phased delivery, not a single release.

## 8. Phased Implementation Plan

Phase 1: Manual input, simple scoring
- manual roster entry
- PPR and standard scoring only
- basic matchup simulation with independent draws

Phase 2: Scoring presets and UX improvements
- preset scoring templates (half-PPR, custom)
- lineup slots and bench rules
- improved error messages for missing players

Phase 3: Correlation modeling and roster imports
- optional correlation adjustments (team-level noise)
- roster imports from common platforms
- caching for repeated weekly simulations

v1 launch does not include these features by design.

## 9. Why This Was Deferred

The current release focuses on:
- player-level correctness and determinism
- clean API boundaries and schema validation
- explainable simulation outputs

The fantasy layer is a natural extension, not missing functionality. Deferring
it keeps v1 simple, reliable, and ready for public demo use.

## 10. Future Plans (Operational Enhancements)

- Postseason support: add explicit postseason schedule integration and rolling context policies
  (e.g., playoff-week baselines, safe fallbacks when postseason stats are unavailable).
- Injuries integration (optional): gated nflreadr::load_injuries() availability checks with a
  clear availability policy and calibration notes, without changing model math.
