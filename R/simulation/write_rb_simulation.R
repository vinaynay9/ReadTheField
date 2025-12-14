# Write RB Simulation - Persistence Layer
#
# This function writes simulation results to a file.
# Consumes the result object from run_rb_simulation().
#
# NO computation, NO console printing
# Only file I/O
#
# Usage:
#   result <- run_rb_simulation(...)
#   write_rb_simulation(result, "rb_simulation_output.txt", overwrite = TRUE)

#' Write RB simulation results to file
#'
#' Writes a clean, single simulation report to the specified file.
#' If overwrite = TRUE, the file is cleared once at the beginning.
#' Uses print_rb_simulation() internally to format output, then redirects to file.
#'
#' @param result List returned by run_rb_simulation()
#' @param file Character, path to output file (default "rb_simulation_output.txt")
#' @param overwrite Logical, if TRUE clears file at start (default TRUE)
write_rb_simulation <- function(result, file = "rb_simulation_output.txt", overwrite = TRUE) {
  
  if (is.null(result) || is.null(result$metadata)) {
    stop("Invalid result object provided")
  }
  
  # Clear file if overwrite is TRUE
  if (overwrite && file.exists(file)) {
    file.remove(file)
  }
  
  # Open file connection for writing
  con <- file(file, open = "wt")
  
  # Use tryCatch to ensure file is closed even on error
  tryCatch({
    # Redirect output to file
    sink(file = con, split = FALSE)
    
    # Write header with timestamp
    cat("Simulation started at", as.character(Sys.time()), "\n")
    cat("Output will be saved to:", file, "\n\n")
    
    # Use print function to format output (but redirect to file)
    print_rb_simulation(result)
    
    # Close sink
    sink()
  }, finally = {
    # Ensure sink is closed
    if (sink.number() > 0) {
      sink()
    }
    # Close file connection
    close(con)
  })
  
  # Return invisibly
  invisible(NULL)
}

