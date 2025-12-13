# Rolling Helpers
# 
# Generic lagged rolling mean and ratio-of-sums helpers for feature engineering.
# All functions operate on vectors sorted by time (ascending).
# Rolling windows are strictly lagged: the current observation is never included.
#
# Dependencies: None (base R only)

#' Compute lagged rolling mean
#'
#' Computes the mean of the previous `window` observations, excluding the current row.
#' Returns NA if fewer than `min_obs` prior observations are available.
#'
#' @param x Numeric vector, sorted by time (ascending)
#' @param window Integer, number of prior observations to include
#' @param min_obs Integer, minimum observations required (default 1)
#' @return Numeric vector of same length as x
#' @examples
#' lagged_roll_mean(c(10, 20, 30, 40, 50), window = 3)
#' # Returns: NA, NA, NA, 20, 30
lagged_roll_mean <- function(x, window, min_obs = 1) {

  n <- length(x)
  if (n == 0) return(numeric(0))
  
  result <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    if (i == 1) {
      # No prior observations for first row
      result[i] <- NA_real_
    } else {
      # Get indices of prior observations (up to window)
      start_idx <- max(1, i - window)
      end_idx <- i - 1
      prior_values <- x[start_idx:end_idx]
      
      # Remove NAs and check minimum observations
      valid_values <- prior_values[!is.na(prior_values)]
      
      if (length(valid_values) >= min_obs) {
        result[i] <- mean(valid_values)
      } else {
        result[i] <- NA_real_
      }
    }
  }
  
  return(result)
}


#' Compute lagged rolling sum
#'
#' Computes the sum of the previous `window` observations, excluding the current row.
#' Returns NA if fewer than `min_obs` prior observations are available.
#'
#' @param x Numeric vector, sorted by time (ascending)
#' @param window Integer, number of prior observations to include
#' @param min_obs Integer, minimum observations required (default 1)
#' @return Numeric vector of same length as x
lagged_roll_sum <- function(x, window, min_obs = 1) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  
  result <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    if (i == 1) {
      result[i] <- NA_real_
    } else {
      start_idx <- max(1, i - window)
      end_idx <- i - 1
      prior_values <- x[start_idx:end_idx]
      valid_values <- prior_values[!is.na(prior_values)]
      
      if (length(valid_values) >= min_obs) {
        result[i] <- sum(valid_values)
      } else {
        result[i] <- NA_real_
      }
    }
  }
  
  return(result)
}


#' Compute lagged ratio-of-sums
#'
#' Computes sum(numerator) / sum(denominator) over the previous `window` observations.
#' Used for efficiency metrics like yards-per-carry or yards-per-target.
#' Returns NA if denominator sum is zero or if insufficient prior observations.
#'
#' @param numerator Numeric vector (e.g., yards)
#' @param denominator Numeric vector (e.g., attempts)
#' @param window Integer, number of prior observations to include
#' @param min_obs Integer, minimum observations required (default 1)
#' @return Numeric vector of same length as inputs
#' @examples
#' yards <- c(50, 80, 100, 60, 90)
#' carries <- c(10, 15, 20, 12, 18)
#' lagged_ratio_of_sums(yards, carries, window = 3)
lagged_ratio_of_sums <- function(numerator, denominator, window, min_obs = 1) {
  n <- length(numerator)
  if (n == 0) return(numeric(0))
  if (length(denominator) != n) {

    stop("numerator and denominator must have same length")
  }
  
  result <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    if (i == 1) {
      result[i] <- NA_real_
    } else {
      start_idx <- max(1, i - window)
      end_idx <- i - 1
      
      num_values <- numerator[start_idx:end_idx]
      denom_values <- denominator[start_idx:end_idx]
      
      # Only use rows where both are non-NA
      valid_mask <- !is.na(num_values) & !is.na(denom_values)
      
      if (sum(valid_mask) >= min_obs) {
        num_sum <- sum(num_values[valid_mask])
        denom_sum <- sum(denom_values[valid_mask])
        
        if (denom_sum > 0) {
          result[i] <- num_sum / denom_sum
        } else {
          result[i] <- NA_real_
        }
      } else {
        result[i] <- NA_real_
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

