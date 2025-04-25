#' Get Initial Compartment Values
#'
#' Computes integer counts for compartments based on total population size and provided proportions.
#'
#' @param N Numeric vector of total population sizes (non-negative integers). Names, if present, denote location identifiers and will be preserved across all compartments.
#' @param prop_S Numeric vector of susceptible proportions. Can be length 1 or same length as \code{N}.
#' @param prop_E Numeric vector of exposed proportions. Can be length 1 or same length as \code{N}.
#' @param prop_I Numeric vector of infected proportions. Can be length 1 or same length as \code{N}.
#' @param prop_R Numeric vector of recovered proportions. Can be length 1 or same length as \code{N}.
#' @param prop_V1 Numeric vector of single-dose vaccinated proportions. Can be length 1 or same length as \code{N}.
#' @param prop_V2 Numeric vector of two-dose vaccinated proportions. Can be length 1 or same length as \code{N}.
#'
#' @return A \code{list} with elements \code{N}, \code{S}, \code{E}, \code{I}, \code{R}, \code{V1}, \code{V2}, each an integer vector of length \code{length(N)} such that \code{N = S + E + I + R + V1 + V2}. All compartments inherit names from \code{N}, if provided.
#'
#' @details The function checks that the sum of provided proportions equals 1 for each location. If they do not, it issues a warning and rescales the proportions so that they sum to 1 before computing counts. It then rounds counts, ensures all outputs are integer type, and distributes any rounding differences to the compartment with the largest proportion so that compartments sum exactly to \code{N}.
#'
#' @examples
#' get_initial_compartment_values(
#'   N = c(location_A = 1000, location_B = 2000),
#'   prop_S = c(0.9, 0.8),
#'   prop_E = c(0.05, 0.1),
#'   prop_I = c(0.03, 0.05),
#'   prop_R = c(0.01, 0.03),
#'   prop_V1 = c(0.005, 0.01),
#'   prop_V2 = c(0.005, 0.01)
#' )
#'
#' @export
get_initial_compartment_values <- function(
          N,
          prop_S, prop_E, prop_I, prop_R, prop_V1, prop_V2
) {
     # validate and coerce N to integer
     if (!is.numeric(N) || any(N < 0) || any(N != as.integer(N))) {
          stop("N must be non-negative integers", call. = FALSE)
     }
     N <- as.integer(N)
     n_locs <- length(N)
     loc_names <- names(N)

     # validate and expand proportions
     props <- list(
          S = prop_S,
          E = prop_E,
          I = prop_I,
          R = prop_R,
          V1 = prop_V1,
          V2 = prop_V2
     )
     for (nm in names(props)) {
          val <- props[[nm]]
          if (!is.numeric(val)) {
               stop(sprintf("%s must be numeric", nm), call. = FALSE)
          }
          if (length(val) == 1) {
               props[[nm]] <- rep(val, n_locs)
          } else if (length(val) == n_locs) {
               # ok
          } else {
               stop(sprintf("%s must be length 1 or length of N", nm), call. = FALSE)
          }
     }

     # check and rescale sum of proportions to 1 per location
     total_prop <- Reduce(`+`, props)
     if (any(abs(total_prop - 1) > 1e-8)) {
          warning("Proportions do not sum to 1; rescaling to sum to 1 for each location", call. = FALSE)
          for (nm in names(props)) {
               props[[nm]] <- props[[nm]] / total_prop
          }
     }

     # compute raw counts
     comp_names <- c("S", "E", "I", "R", "V1", "V2")
     raw_counts <- matrix(0, nrow = n_locs, ncol = length(comp_names),
                          dimnames = list(loc_names, comp_names))
     for (nm in comp_names) {
          raw_counts[, nm] <- props[[nm]] * N
     }

     # round to integer and ensure integer type
     int_counts <- matrix(as.integer(round(raw_counts)),
                          nrow = n_locs, ncol = length(comp_names),
                          dimnames = list(loc_names, comp_names))

     # adjust rounding differences so each row sums to N
     diff <- N - rowSums(int_counts)
     for (i in seq_len(n_locs)) {
          if (diff[i] != 0) {
               max_nm <- comp_names[which.max(vapply(comp_names,
                                                     function(x) props[[x]][i],
                                                     numeric(1)))]
               int_counts[i, max_nm] <- int_counts[i, max_nm] + diff[i]
          }
     }

     # build output list and preserve names
     out <- list(
          N = N
     )
     for (nm in comp_names) {
          vec <- int_counts[, nm]
          names(vec) <- loc_names
          out[[nm]] <- vec
     }

     return(out)
}
