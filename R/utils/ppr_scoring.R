# PPR Scoring
#
# Single source of truth for PPR fantasy point calculations.
# All fantasy point derivations must use these functions.
# Fantasy points are NEVER modeled directly - they are always derived.
#
# Scoring Rules (PPR):
#   - Passing yards: 0.04 points per yard
#   - Passing TDs: 4 points each
#   - Interceptions: -2 points each
#   - Rushing yards: 0.1 points per yard
#   - Rushing TDs: 6 points each
#   - Receptions: 1 point each
#   - Receiving yards: 0.1 points per yard
#   - Receiving TDs: 6 points each
#   - FG made: 3 points each (simplified; distance-based scoring deferred to v2)
#   - XP made: 1 point each
#
# Note: Fumbles are intentionally excluded from v1 due to low signal-to-noise ratio.
#
# Dependencies: None (base R only)

# Scoring constants (centralized for easy modification)
PPR_SCORING <- list(
  pass_yards = 0.04,
  pass_td = 4,
  interception = -2,
  rush_yards = 0.1,
  rush_td = 6,
  reception = 1,
  rec_yards = 0.1,
  rec_td = 6,
  fg_made = 3,
  xp_made = 1
)


#' Compute PPR fantasy points for RB
#'
#' Calculates PPR fantasy points from RB stat components.
#' All inputs should be numeric vectors or scalars of same length.
#'
#' @param rush_yards Numeric, rushing yards
#' @param rush_tds Numeric, rushing touchdowns
#' @param receptions Numeric, receptions (PPR: 1 point each)
#' @param rec_yards Numeric, receiving yards
#' @param rec_tds Numeric, receiving touchdowns
#' @return Numeric, PPR fantasy points
compute_ppr_rb <- function(rush_yards, rush_tds, receptions, rec_yards, rec_tds) {
  
  # Replace NA with 0 for calculation (NA stats contribute 0 points)
  rush_yards <- ifelse(is.na(rush_yards), 0, rush_yards)
  rush_tds <- ifelse(is.na(rush_tds), 0, rush_tds)
  receptions <- ifelse(is.na(receptions), 0, receptions)
  rec_yards <- ifelse(is.na(rec_yards), 0, rec_yards)
  rec_tds <- ifelse(is.na(rec_tds), 0, rec_tds)
  
  points <- (rush_yards * PPR_SCORING$rush_yards) +
            (rush_tds * PPR_SCORING$rush_td) +
            (receptions * PPR_SCORING$reception) +
            (rec_yards * PPR_SCORING$rec_yards) +
            (rec_tds * PPR_SCORING$rec_td)
  
  return(points)
}


#' Compute PPR fantasy points for QB
#'
#' Calculates PPR fantasy points from QB stat components.
#'
#' @param pass_yards Numeric, passing yards
#' @param pass_tds Numeric, passing touchdowns
#' @param interceptions Numeric, interceptions thrown
#' @param rush_yards Numeric, rushing yards
#' @param rush_tds Numeric, rushing touchdowns
#' @return Numeric, PPR fantasy points
compute_ppr_qb <- function(pass_yards, pass_tds, interceptions, 
                           rush_yards, rush_tds) {
  
  pass_yards <- ifelse(is.na(pass_yards), 0, pass_yards)
  pass_tds <- ifelse(is.na(pass_tds), 0, pass_tds)
  interceptions <- ifelse(is.na(interceptions), 0, interceptions)
  rush_yards <- ifelse(is.na(rush_yards), 0, rush_yards)
  rush_tds <- ifelse(is.na(rush_tds), 0, rush_tds)
  
  points <- (pass_yards * PPR_SCORING$pass_yards) +
            (pass_tds * PPR_SCORING$pass_td) +
            (interceptions * PPR_SCORING$interception) +
            (rush_yards * PPR_SCORING$rush_yards) +
            (rush_tds * PPR_SCORING$rush_td)
  
  return(points)
}


#' Compute PPR fantasy points for WR/TE
#'
#' Calculates PPR fantasy points from WR or TE stat components.
#' Note: WR and TE use the same scoring formula but separate models.
#'
#' @param receptions Numeric, receptions (PPR: 1 point each)
#' @param rec_yards Numeric, receiving yards
#' @param rec_tds Numeric, receiving touchdowns
#' @return Numeric, PPR fantasy points
compute_ppr_wrte <- function(receptions, rec_yards, rec_tds) {
  
  receptions <- ifelse(is.na(receptions), 0, receptions)
  rec_yards <- ifelse(is.na(rec_yards), 0, rec_yards)
  rec_tds <- ifelse(is.na(rec_tds), 0, rec_tds)
  
  points <- (receptions * PPR_SCORING$reception) +
            (rec_yards * PPR_SCORING$rec_yards) +
            (rec_tds * PPR_SCORING$rec_td)
  
  return(points)
}


#' Compute fantasy points for K
#'
#' Calculates fantasy points from kicker stat components.
#' Note: v1 uses simplified 3-point FG scoring regardless of distance.
#'
#' @param fg_made Numeric, field goals made
#' @param xp_made Numeric, extra points made
#' @return Numeric, fantasy points
compute_ppr_k <- function(fg_made, xp_made) {
  
  fg_made <- ifelse(is.na(fg_made), 0, fg_made)
  xp_made <- ifelse(is.na(xp_made), 0, xp_made)
  
  points <- (fg_made * PPR_SCORING$fg_made) +
            (xp_made * PPR_SCORING$xp_made)
  
  return(points)
}


#' Generic PPR fantasy point computation
#'
#' Dispatches to position-specific function based on position_group.
#' All inputs should be named in the stat list.
#'
#' @param position_group Character, one of "QB", "RB", "WR", "TE", "K"
#' @param stats Named list of statistics
#' @return Numeric, PPR fantasy points
compute_ppr_points <- function(position_group, stats) {
  
  if (is.null(position_group) || is.na(position_group)) {
    warning("position_group is NULL or NA, returning NA")
    return(NA_real_)
  }
  
  switch(position_group,
    "QB" = compute_ppr_qb(
      pass_yards = stats$pass_yards %||% 0,
      pass_tds = stats$pass_tds %||% 0,
      interceptions = stats$interceptions %||% 0,
      rush_yards = stats$rush_yards %||% 0,
      rush_tds = stats$rush_tds %||% 0
    ),
    "RB" = compute_ppr_rb(
      rush_yards = stats$rush_yards %||% 0,
      rush_tds = stats$rush_tds %||% 0,
      receptions = stats$receptions %||% 0,
      rec_yards = stats$rec_yards %||% 0,
      rec_tds = stats$rec_tds %||% 0
    ),
    "WR" = compute_ppr_wrte(
      receptions = stats$receptions %||% 0,
      rec_yards = stats$rec_yards %||% 0,
      rec_tds = stats$rec_tds %||% 0
    ),
    "TE" = compute_ppr_wrte(
      receptions = stats$receptions %||% 0,
      rec_yards = stats$rec_yards %||% 0,
      rec_tds = stats$rec_tds %||% 0
    ),
    "K" = compute_ppr_k(
      fg_made = stats$fg_made %||% 0,
      xp_made = stats$xp_made %||% 0
    ),
    {
      warning(paste("Unknown position_group:", position_group))
      NA_real_
    }
  )
}


# Null-coalescing operator (if not defined elsewhere)
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

