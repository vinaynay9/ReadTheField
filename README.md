# Read the Field (V1)

Read the Field is a probabilistic NFL player projection system focused on offensive skill positions and kickers. Rather than producing single point predictions, the system estimates distributions of possible outcomes for a given player matchup, explicitly modeling uncertainty and variance in player performance.

The project is built around the idea that NFL production is a conditional, noisy process. A player's stat line depends not only on their own historical usage and efficiency, but also on team context, opponent defensive tendencies, and game conditions. Read the Field encodes these factors as pre-game features and uses historical data to learn how player outcomes vary under similar conditions.

V1 uses publicly available NFL data to construct a leakage-safe modeling dataset at the player-game level. For each game, features are computed using only information available prior to kickoff, including recent usage trends, snap share, team offensive tendencies, opponent defensive aggregates, and basic player attributes. Post-game outcomes are stored separately and used only for training and validation.

Models are trained to predict key offensive statistics for quarterbacks, running backs, wide receivers, tight ends, and kickers. Instead of relying on a single deterministic model, Read the Field introduces stochasticity through resampling and ensemble methods. Multiple simulations are run for each matchup, producing a distribution of predicted outcomes rather than a single estimate. Outputs include expected values as well as percentile bands that describe the range of likely results.

The system is designed as an end-to-end analytics pipeline. Data ingestion, feature engineering, model training, simulation, and output generation are modular and reproducible. Model outputs are persisted and can be surfaced through a web interface that visualizes both the prediction process and the resulting distributions.

V1 intentionally prioritizes correctness, transparency, and extensibility over breadth. Defensive player predictions and play-by-play micro-analysis are explicitly out of scope at this stage, as public data for those tasks is sparse and noisy. The current design provides a stable foundation that can be extended in future versions without rewriting the core system.

**v1 intentionally excludes fumbles and other low-frequency turnover events. These may be added in future versions after validation infrastructure is complete.**

## System Architecture

The repository follows a modular pipeline architecture organized around distinct stages of the analytics workflow. Data flows from raw ingestion through feature engineering, model training, validation, and simulation. Each stage operates independently with well-defined interfaces, enabling parallel development and testing. The architecture separates concerns between data management, feature engineering, modeling, validation, and deployment components.

## Data Flow

Raw data enters the system through snapshot-based ingestion into the data/raw directory structure. Processed data transformations convert raw inputs into standardized weekly aggregations for players, defenses, and game contexts. Feature engineering modules transform processed data into modeling-ready feature matrices. Trained models consume these features to generate predictions, which flow through validation pipelines before being used in simulation workflows. Artifacts including trained models and predictions are stored separately for deployment and analysis.

## Modeling Philosophy

The modeling approach emphasizes probabilistic predictions with uncertainty quantification. Models are trained using time-based cross-validation to ensure temporal validity and prevent data leakage. Calibration and interval coverage validation ensure that predicted probabilities and confidence intervals match observed outcomes. Simulation workflows use bootstrap resampling and Monte Carlo methods to generate full probability distributions rather than point estimates, enabling decision-making under uncertainty.

## Intended Usage

The repository supports end-to-end workflows from data ingestion to prediction generation. Users execute scripts in sequence to refresh data, rebuild features, retrain models, validate performance, and run simulations. The modular design allows selective execution of pipeline stages based on specific needs. Future API and frontend components will consume model artifacts to serve predictions and visualizations without requiring direct access to the modeling codebase.


Stats Used + Returned:
All:
 - Used: Height, Weight, Injured?, Home/Away, Previous Seasons, Comparable players (by height/weight within 0.2 standard deviations), % of Plays Played
 - Returned: PPR Fantasy (offensive)

QB:
 - Used: PYD, PTD, RUYD, RTD, P Attempts, R Attempts, PPR Fantasy, COmpletions
 - Returned: PYD, PTD, RYD, RTD, P Attempts, R Attempts, Completions

RB:
 - Used: RUYD, RUTD, Receptions, RECYD, RECTD, Targets, Carries
 - Returned: RUYD, RECYD, TOTTD, Carries

WR:
 - Used: Receptions, RECYD, RECTD, targets, YAC
 - Returned: Receptions, RECYD, RECTD

TE:
 - Used: Receptions, RECYD, RECTD, targets, YAC
 - Returned: Receptions, RECYD, RECTD

OL:
 - Used: # Snaps, 
 - Returned: 

DT:
 - Used: Tackles, Sacks, FF, FR, Passes Blocked, TFL
 - Returned: Tackles, Sacks, FF, TFL

DE:
 - Used: Tackles, Sacks, FF, FR, Passes Blocked, TFL
 - Returned: Tackles, Sacks, FF, TFL

S:
 - Used: Sacks, Tackles, Interceptions, FF, FR, TFL
 - Returned: Sacks, Tackles, Interceptions, TFL

CB:
 - Used: Sacks, Tackles, Interceptions, FF, FR, TFL
 - Returned: Sacks, Tackles, Interceptions, TFL

LB:
 - Used: Sacks, Tackles, Interceptions, FF, FR, Passes Blocked, TFL
 - Returned: Sacks, Tackles, FF, TFL

K:
 - Used: Made (by distance), PAT (attempts and made), Season longest
 - Returned: FG Made, FG Attempts, # PAT

P:
 - Used: # Punts, Average punt distance, Season long
 - Returned: # Punts