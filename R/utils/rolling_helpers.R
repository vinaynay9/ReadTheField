# Rolling Helpers
# 
# Generic lagged rolling mean and ratio-of-sums helpers for feature engineering.
# All functions operate on vectors sorted by time (ascending).
# Rolling windows are strictly lagged: the current observation is never included.
# 
# STRICT WINDOW SEMANTICS:
#   - Requires exactly N prior non-NA observations (no partial windows)
#   - Week 1 → NA (no prior games)
#   - Week 2 → NA (only 1 prior game, need N)
#   - Week N → NA (only N-1 prior games, need N)
#   - Week N+1 → first non-NA value (has exactly N prior games)
#
# Dependencies: None (base R only)

#' Compute lagged rolling mean (strict window semantics)
#'
#' Computes the mean of exactly the previous `window` observations, excluding the current row.
#' Returns NA if fewer than `window` prior non-NA observations are available.
#' Enforces strict window semantics: no partial windows allowed.
#'
#' @param x Numeric vector, sorted by time (ascending)
#' @param window Integer, exact number of prior observations required (must be >= 1)
#' @return Numeric vector of same length as x
#' @examples
#' lagged_roll_mean(c(10, 20, 30, 40, 50), window = 3)
#' # Returns: NA, NA, NA, 20, 30
#' # Week 1: NA (0 prior games, need 3)
#' # Week 2: NA (1 prior game, need 3)
#' # Week 3: NA (2 prior games, need 3)
#' # Week 4: 20 (mean of weeks 1-3: 10, 20, 30)
#' # Week 5: 30 (mean of weeks 2-4: 20, 30, 40)
lagged_roll_mean <- function(x, window) {
  
  # Defensive assertions
  if (missing(window) || is.null(window) || length(window) != 1) {
    stop("window must be a single integer >= 1")
  }
  window <- as.integer(window)
  if (window < 1) {
    stop("window must be >= 1, got ", window)
  }
  
  n <- length(x)
  if (n == 0) return(numeric(0))
  
  result <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    # Require exactly 'window' prior observations
    if (i <= window) {
      # Fewer than 'window' prior games available
      result[i] <- NA_real_
    } else {
      # Get exactly 'window' prior observations (strict window)
      start_idx <- i - window
      end_idx <- i - 1
      prior_values <- x[start_idx:end_idx]
      
      # Strict requirement: all 'window' values must be non-NA
      if (any(is.na(prior_values))) {
        result[i] <- NA_real_
      } else {
        result[i] <- mean(prior_values)
      }
    }
  }
  
  return(result)
}


#' Compute lagged rolling sum (strict window semantics)
#'
#' Computes the sum of exactly the previous `window` observations, excluding the current row.
#' Returns NA if fewer than `window` prior non-NA observations are available.
#' Enforces strict window semantics: no partial windows allowed.
#'
#' @param x Numeric vector, sorted by time (ascending)
#' @param window Integer, exact number of prior observations required (must be >= 1)
#' @return Numeric vector of same length as x
lagged_roll_sum <- function(x, window) {
  
  # Defensive assertions
  if (missing(window) || is.null(window) || length(window) != 1) {
    stop("window must be a single integer >= 1")
  }
  window <- as.integer(window)
  if (window < 1) {
    stop("window must be >= 1, got ", window)
  }
  
  n <- length(x)
  if (n == 0) return(numeric(0))
  
  result <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    # Require exactly 'window' prior observations
    if (i <= window) {
      # Fewer than 'window' prior games available
      result[i] <- NA_real_
    } else {
      # Get exactly 'window' prior observations (strict window)
      start_idx <- i - window
      end_idx <- i - 1
      prior_values <- x[start_idx:end_idx]
      
      # Strict requirement: all 'window' values must be non-NA
      if (any(is.na(prior_values))) {
        result[i] <- NA_real_
      } else {
        result[i] <- sum(prior_values)
      }
    }
  }
  
  return(result)
}


#' Compute lagged ratio-of-sums (strict window semantics)
#'
#' Computes sum(numerator) / sum(denominator) over exactly the previous `window` observations.
#' Used for efficiency metrics like yards-per-carry or yards-per-target.
#' Returns NA if fewer than `window` prior non-NA observations are available,
#' or if denominator sum is zero.
#' Enforces strict window semantics: no partial windows allowed.
#'
#' @param numerator Numeric vector (e.g., yards)
#' @param denominator Numeric vector (e.g., attempts)
#' @param window Integer, exact number of prior observations required (must be >= 1)
#' @return Numeric vector of same length as inputs
#' @examples
#' yards <- c(50, 80, 100, 60, 90)
#' carries <- c(10, 15, 20, 12, 18)
#' lagged_ratio_of_sums(yards, carries, window = 3)
#' # Week 1-3: NA (insufficient prior games)
#' # Week 4: (50+80+100)/(10+15+20) = 230/45 = 5.11
#' # Week 5: (80+100+60)/(15+20+12) = 240/47 = 5.11
lagged_ratio_of_sums <- function(numerator, denominator, window) {
  
  # Defensive assertions
  if (missing(window) || is.null(window) || length(window) != 1) {
    stop("window must be a single integer >= 1")
  }
  window <- as.integer(window)
  if (window < 1) {
    stop("window must be >= 1, got ", window)
  }
  
  n <- length(numerator)
  if (n == 0) return(numeric(0))
  if (length(denominator) != n) {
    stop("numerator and denominator must have same length")
  }
  
  result <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    # Require exactly 'window' prior observations
    if (i <= window) {
      # Fewer than 'window' prior games available
      result[i] <- NA_real_
    } else {
      # Get exactly 'window' prior observations (strict window)
      start_idx <- i - window
      end_idx <- i - 1
      
      num_values <- numerator[start_idx:end_idx]
      denom_values <- denominator[start_idx:end_idx]
      
      # Strict requirement: all 'window' values must be non-NA for both numerator and denominator
      if (any(is.na(num_values)) || any(is.na(denom_values))) {
        result[i] <- NA_real_
      } else {
        num_sum <- sum(num_values)
        denom_sum <- sum(denom_values)
        
        if (denom_sum > 0) {
          result[i] <- num_sum / denom_sum
        } else {
          result[i] <- NA_real_
        }
      }
    }
  }
  
  return(result)
}


#' Get previous value (lag 1)
#'
#' Returns the immediately prior value for each observation.
#'
#' @param x Numeric vector, sorted by time (ascending)
#' @return Numeric vector of same length as x
lagged_prev <- function(x) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  
  c(NA_real_, x[-n])
}
