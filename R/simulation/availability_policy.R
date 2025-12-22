# Availability Policy - Controls counterfactual simulation behavior
#
# This module defines the availability policy interface used by CLI and simulation.

validate_availability_policy <- function(policy) {
  if (missing(policy) || is.null(policy) || length(policy) == 0) {
    return("played_only")
  }
  policy <- as.character(policy)[1]
  policy <- trimws(policy)
  if (policy == "") {
    return("played_only")
  }
  allowed <- c("played_only", "expected_active", "force_counterfactual")
  if (!policy %in% allowed) {
    stop("Invalid availability_policy: ", policy,
         ". Allowed values: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  policy
}

is_counterfactual_policy <- function(policy) {
  policy <- validate_availability_policy(policy)
  policy %in% c("expected_active", "force_counterfactual")
}

describe_policy <- function(policy) {
  policy <- validate_availability_policy(policy)
  switch(
    policy,
    played_only = "Played-only (requires observed game)",
    expected_active = "Expected-active (counterfactual when inactive/missing)",
    force_counterfactual = "Force counterfactual (ignore observed game)"
  )
}

availability_note <- function(policy, reason = NULL) {
  policy <- validate_availability_policy(policy)
  label <- describe_policy(policy)
  if (is.null(reason) || is.na(reason) || !nzchar(trimws(as.character(reason)))) {
    return(paste0("Availability policy: ", label))
  }
  paste0("Availability policy: ", label, " | ", reason)
}
