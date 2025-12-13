# Distributions and Simulation Design (v1)

## Overview

Read the Field models player outcomes as probability distributions rather than point estimates. Each prediction represents a range of plausible outcomes, enabling uncertainty quantification and risk-aware decision-making.

All percentile outputs (p25, p50, p75) are derived via Monte Carlo simulation. The system generates thousands of simulated stat lines per player-game, then computes empirical percentiles from the resulting distribution.

**Critical constraint**: Fantasy points are always derived from simulated component statistics, never modeled directly. This ensures internal consistency between projected stat lines and fantasy totals.

---

## Core Modeling Principle

> Volume first, efficiency second, scoring last.

This ordering reflects the causal structure of NFL statistics:

1. **Volume** (attempts, targets, carries) determines opportunity
2. **Efficiency** (yards per attempt, catch rate) converts opportunity to production
3. **Scoring** (touchdowns) occurs conditional on sufficient production and red-zone opportunity

Modeling in this order avoids implausible stat lines. For example:
- A player cannot have 150 receiving yards on 2 targets
- A player cannot have 3 rushing TDs on 20 rushing yards
- A kicker cannot make 4 field goals on 2 attempts

By sampling volume first, then conditioning downstream statistics on realized volume, the simulation produces internally consistent outcomes.

---

## Distribution Choices (v1)

| Stat Type | Distribution | Rationale |
|-----------|--------------|-----------|
| Volume (attempts, targets, carries) | Negative Binomial | Overdispersed count data; Poisson acceptable as fallback if variance roughly equals mean |
| Conditional yardage | Gaussian, truncated at zero | Continuous, symmetric around expectation given volume; truncation prevents negative yards |
| Touchdowns | Poisson with log link | Rare count events; rate depends on volume and red-zone opportunity |
| Kicker makes (FG, XP) | Binomial | Binary outcomes (make/miss) with known attempt count |
| Receptions given targets | Binomial | Binary outcome per target with catch probability |

### Notes on Distribution Selection

- **Negative Binomial vs. Poisson**: Negative Binomial handles overdispersion common in NFL volume stats. If observed variance is close to mean, Poisson is acceptable.
- **Truncated Gaussian**: Yardage cannot be negative in most contexts. Truncation at zero is applied after sampling from the conditional Gaussian.
- **Poisson for TDs**: Touchdown counts are typically 0, 1, or 2 per game. Poisson with low rate parameter captures this behavior.
- **Binomial for makes**: Kicker accuracy is modeled as success probability per attempt. Given `fg_attempts`, sample `fg_made ~ Binomial(fg_attempts, p_fg)`.

---

## Position-Specific Dependency Graphs

### QB

```
pass_attempts
    |
    +---> pass_yards | pass_attempts
    +---> pass_tds | pass_attempts
    +---> interceptions | pass_attempts

rush_attempts
    |
    +---> rush_yards | rush_attempts
    +---> rush_tds | rush_attempts
```

**Sampling order**:
1. `pass_attempts` (Negative Binomial)
2. `pass_yards` (Gaussian | pass_attempts, truncated at 0)
3. `pass_tds` (Poisson | pass_attempts)
4. `interceptions` (Poisson | pass_attempts)
5. `rush_attempts` (Negative Binomial)
6. `rush_yards` (Gaussian | rush_attempts, truncated at 0)
7. `rush_tds` (Poisson | rush_attempts)

Passing and rushing paths are independent given pre-game features.

### RB

```
carries
    |
    +---> rush_yards | carries
            |
            +---> rush_tds | carries

targets
    |
    +---> receptions | targets
            |
            +---> rec_yards | receptions
                    |
                    +---> rec_tds | targets
```

**Sampling order**:
1. `carries` (Negative Binomial)
2. `rush_yards` (Gaussian | carries, truncated at 0)
3. `rush_tds` (Poisson | carries)
4. `targets` (Negative Binomial)
5. `receptions` (Binomial | targets, catch_rate)
6. `rec_yards` (Gaussian | receptions, truncated at 0)
7. `rec_tds` (Poisson | targets)

Rushing and receiving paths are independent given pre-game features.

### WR

```
targets
    |
    +---> receptions | targets
            |
            +---> rec_yards | receptions
                    |
                    +---> rec_tds | targets
```

**Sampling order**:
1. `targets` (Negative Binomial)
2. `receptions` (Binomial | targets, catch_rate)
3. `rec_yards` (Gaussian | receptions, truncated at 0)
4. `rec_tds` (Poisson | targets)

### TE

```
targets
    |
    +---> receptions | targets
            |
            +---> rec_yards | receptions
                    |
                    +---> rec_tds | targets
```

**Sampling order**:
1. `targets` (Negative Binomial)
2. `receptions` (Binomial | targets, catch_rate)
3. `rec_yards` (Gaussian | receptions, truncated at 0)
4. `rec_tds` (Poisson | targets)

TE uses identical dependency structure to WR but with separate model parameters trained only on TE data.

### K

```
fg_attempts
    |
    +---> fg_made | fg_attempts

xp_attempts
    |
    +---> xp_made | xp_attempts
```

**Sampling order**:
1. `fg_attempts` (Negative Binomial)
2. `fg_made` (Binomial | fg_attempts, p_fg)
3. `xp_attempts` (Negative Binomial)
4. `xp_made` (Binomial | xp_attempts, p_xp)

Field goal and extra point paths are independent given pre-game features.

---

## Detailed RB Simulation Example

This section walks through a single Monte Carlo draw for an RB player-game.

### Step 1: Sample Volume Statistics

**Carries**
- Model: Negative Binomial with mean and dispersion estimated from pre-game features
- Pre-game features: player rolling usage, team rushing tendency, opponent rush defense
- Draw: `carries = 18`

**Targets**
- Model: Negative Binomial with mean and dispersion estimated from pre-game features
- Pre-game features: player rolling targets, team passing tendency, opponent pass defense
- Draw: `targets = 4`

### Step 2: Sample Rushing Statistics (Conditional on Carries)

**Rush Yards**
- Model: Gaussian with mean = f(carries, yards_per_carry_roll5, opp_rush_yards_allowed_roll5)
- Conditional mean: E[rush_yards | carries=18] = 18 * 4.2 = 75.6
- Conditional SD: estimated from historical residuals
- Draw from N(75.6, 28), truncated at 0: `rush_yards = 82`

**Rush TDs**
- Model: Poisson with log-linear rate depending on carries and red-zone proxy
- Conditional rate: lambda = exp(intercept + beta_carries * 18)
- Draw from Poisson(0.45): `rush_tds = 0`

### Step 3: Sample Receiving Statistics (Conditional on Targets)

**Receptions**
- Model: Binomial with catch_rate estimated from pre-game features
- Conditional: `receptions ~ Binomial(targets=4, p=0.78)`
- Draw: `receptions = 3`

**Receiving Yards**
- Model: Gaussian with mean = f(receptions, yards_per_reception_roll5)
- Conditional mean: E[rec_yards | receptions=3] = 3 * 8.1 = 24.3
- Conditional SD: estimated from historical residuals
- Draw from N(24.3, 12), truncated at 0: `rec_yards = 28`

**Receiving TDs**
- Model: Poisson with log-linear rate depending on targets
- Conditional rate: lambda = exp(intercept + beta_targets * 4)
- Draw from Poisson(0.15): `rec_tds = 0`

### Step 4: Derive Fantasy Points

Fantasy points are computed deterministically from sampled statistics:

```
ppr_fantasy = (rush_yards * 0.1) + (rush_tds * 6)
            + (receptions * 1.0) + (rec_yards * 0.1) + (rec_tds * 6)

ppr_fantasy = (82 * 0.1) + (0 * 6) + (3 * 1.0) + (28 * 0.1) + (0 * 6)
            = 8.2 + 0 + 3.0 + 2.8 + 0
            = 14.0
```

### Step 5: Repeat and Aggregate

Repeat Steps 1-4 for N = 10,000 simulations. Collect all `ppr_fantasy` values and compute empirical percentiles.

---

## Percentile Policy

### Surfaced Percentiles (v1)

| Percentile | Interpretation |
|------------|----------------|
| p25 | Conservative/floor outcome |
| p50 | Median/expected outcome |
| p75 | Optimistic/ceiling outcome |

### Computation

Percentiles are empirical, computed directly from simulation draws:

1. Generate N simulation draws (N = 10,000)
2. Sort draws for each statistic
3. Extract values at positions corresponding to desired percentiles
4. Report as p25, p50, p75

### Rounding

- Yardage percentiles: round to nearest integer
- Count statistics (attempts, TDs, receptions): round to nearest integer
- Fantasy points: round to one decimal place

---

## Explicit Non-Goals for v1

The following approaches are intentionally excluded from v1:

| Excluded Approach | Rationale |
|-------------------|-----------|
| Direct fantasy regression | Fantasy points must be derived from component stats to ensure internal consistency |
| WR/TE pooling | WR and TE have different usage patterns; separate models required |
| Copulas | Adds complexity without clear benefit for v1 accuracy targets |
| Multivariate normals | Cross-statistic correlations are captured implicitly through conditioning on volume |
| p90/p95 outputs | Tail percentiles are noisy and may encourage over-optimization; reserved for future versions |
| Mixture models | Single-component models sufficient for v1; mixtures may be explored in v2 |
| Player-specific priors | v1 uses position-level models; hierarchical player effects deferred to v2 |
| In-game adjustments | v1 produces pre-game projections only; live updates out of scope |

---

## Summary

The v1 simulation pipeline follows a strict ordering:

1. Sample volume statistics from marginal distributions
2. Sample efficiency/yardage statistics conditional on volume
3. Sample scoring statistics conditional on volume
4. Derive fantasy points deterministically
5. Repeat for N simulations
6. Report empirical percentiles

This approach produces internally consistent stat lines, propagates uncertainty through the dependency chain, and ensures fantasy totals align with component projections.

