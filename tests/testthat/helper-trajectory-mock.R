# =============================================================================
# helper-trajectory-mock.R
#
# A minimal mosaic_trajectories object for testing consumers of the reduced
# trajectory artifact (currently write_trajectory_csv()).
#
# This is deliberately NOT built by running calc_model_ensemble() -- that path
# is already covered by test-trajectories.R, and a reducer run costs seconds per
# case. What the consumers need is only the artifact's SHAPE: $channels,
# $summary[[ch]]$median as an [n_loc x n_t] matrix, $location_names,
# $n_time_points and $date_start. Those five fields are the contract; this
# helper reproduces exactly them, so a change to the contract breaks these
# tests rather than silently passing.
#
# Values are a deterministic ramp (location index * 1000 + tick), so a
# transposed or interleaved write is visible in the output rather than looking
# plausible.
# =============================================================================

mock_trajectories <- function(locs = "MOZ",
                              n_t = 10L,
                              channels = c("incidence", "disease_deaths"),
                              date_start = "2018-01-01") {
     n_loc <- length(locs)
     summary <- stats::setNames(
          lapply(seq_along(channels), function(ci) {
               m <- matrix(NA_real_, nrow = n_loc, ncol = n_t)
               for (li in seq_len(n_loc)) {
                    m[li, ] <- li * 1000 + seq_len(n_t) + (ci - 1) * 0.5
               }
               list(median = m)
          }),
          channels
     )

     structure(
          list(
               schema         = "mosaic_trajectories",
               channels       = channels,
               location_names = locs,
               n_locations    = n_loc,
               n_time_points  = as.integer(n_t),
               date_start     = date_start,
               date_stop      = as.character(
                    as.Date(date_start) + (n_t - 1L)
               ),
               summary        = summary
          ),
          class = "mosaic_trajectories"
     )
}
