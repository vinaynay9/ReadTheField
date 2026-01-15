# Warning Policy v1
#
# Defines the allowlist for expected warnings and helpers to enforce policy.

get_warning_allowlist_v1 <- function() {
  c(
    "glm.fit: fitted probabilities numerically 0 or 1 occurred",
    "glm.fit: fitted rates numerically 0 occurred",
    "Non-finite Gaussian mean prediction under played_only; using fitted-mean fallback\\.",
    "RB simulation: [0-9]+ model\\(s\\) using baseline",
    "nflreadr::load_players\\(\\) returned empty result\\. Using cached player_dim\\.",
    "nflreadr::load_rosters\\(\\) returned empty result\\. Using cached player_dim\\.",
    "Download failed.*using cached data"
  )
}

is_allowed_warning_v1 <- function(message) {
  if (is.null(message) || length(message) == 0) return(FALSE)
  message <- as.character(message)[1]
  patterns <- get_warning_allowlist_v1()
  any(vapply(patterns, function(p) grepl(p, message), logical(1)))
}

handle_warnings_v1 <- function(expr, context = "") {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (is_allowed_warning_v1(msg)) {
        label <- if (nzchar(context)) paste0(" [", context, "]") else ""
        message("Allowed warning", label, ": ", msg)
        invokeRestart("muffleWarning")
      } else {
        stop("Unexpected warning", if (nzchar(context)) paste0(" [", context, "]") else "",
             ": ", msg, call. = FALSE)
      }
    }
  )
}
