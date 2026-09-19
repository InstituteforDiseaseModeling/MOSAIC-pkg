# Build a held-out config: zero the observation weights after T_cut so the
# likelihood cannot see them, while the simulation still runs the full window.
# Uses existing machinery (config$reported_*_weight -> weights_obs_* in
# calc_model_likelihood); no package change required.
make_holdout_config <- function(config, t_cut) {
     d <- seq(as.Date(config$date_start), by = "day",
              length.out = ncol(config$reported_cases))
     oos <- which(d > as.Date(t_cut))
     if (!length(oos)) stop("t_cut leaves no out-of-sample window")
     for (f in c("reported_cases_weight", "reported_deaths_weight")) {
          if (is.null(config[[f]])) stop("config lacks ", f)
          config[[f]][, oos] <- 0
     }
     attr(config, "t_cut") <- as.Date(t_cut)
     attr(config, "oos_idx") <- oos
     config
}

# Dates for a run, and the horizon bucket (months past t_cut) of each day.
run_dates <- function(config) {
     seq(as.Date(config$date_start), by = "day",
         length.out = ncol(config$reported_cases))
}
horizon_bucket <- function(dates, t_cut) {
     hm <- as.numeric(difftime(dates, as.Date(t_cut), units = "days")) / 30.44
     ifelse(hm <= 0, NA_character_,
     ifelse(hm <= 1, "h1",
     ifelse(hm <= 2, "h2",
     ifelse(hm <= 3, "h3",
     ifelse(hm <= 6, "h4-6", "h>6")))))
}
