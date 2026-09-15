#' Normalise and validate a config for the R transmission engine
#'
#' Python's \code{params.py} is 1,009 lines, and it is tempting to call it
#' "just coercion" and drop it -- but that is only safe if the engine's sole
#' input is \code{make_simulation_config()} output, and it is not: the engine also
#' accepts raw lists and file paths. Scalar-to-matrix broadcasting, dimension
#' checking and non-finite rejection all live there today and have to live
#' somewhere afterwards. This is that somewhere.
#'
#' Orientation is the single most dangerous thing here. Configs carry
#' time-varying fields as \code{[npatches][nticks]} (location-major, matching
#' the JSON on disk), while the engine's internal state is
#' \code{[nticks + 1, npatches]} (time-major, matching Python's frames). This
#' function transposes once, at the boundary, and every dimension is asserted
#' rather than assumed -- a silently transposed input is the likeliest way to
#' get plausible-looking wrong answers out of the whole port.
#'
#' @param config Config list, or a path to a \code{.json} / \code{.json.gz}
#'   file.
#' @param components Pipeline subset that will be run; determines which
#'   compartments \code{Census} sums and which parameters are required.
#' @return A list of validated engine parameters, including the original
#'   (normalised) config under \code{$config}.
#' @keywords internal
sim_params <- function(config, components = SIM_PIPELINE) {

     config <- .sim_load_config(config)

     par <- list()
     par$config <- config
     par$seed <- config$seed

     # -- shape ----------------------------------------------------------------
     if (is.null(config$location_name)) {
          stop("Config is missing `location_name`; the engine cannot infer npatches.",
               call. = FALSE)
     }
     par$npatches <- length(config$location_name)

     par$nticks <- .sim_nticks(config)

     # Which compartments exist depends on the pipeline subset in play, and
     # `Census` must sum exactly these. Mirrors the Python components' lazy
     # `hasattr` checks, but resolved up front so it is inspectable.
     par$compartments <- .sim_compartments(components)

     # -- required scalars and vectors -----------------------------------------
     par$check_invariants <- isTRUE(config$check_invariants %||% TRUE)

     # -- time-varying matrices ------------------------------------------------
     # Broadcast then transpose to [nticks, npatches].
     for (nm in c("b_jt", "d_jt")) {
          par[[nm]] <- .sim_time_matrix(config[[nm]], nm,
                                        par$nticks, par$npatches)
     }
     if (any(c("EnvToHuman", "Environmental") %in% components)) {
          par$psi_jt <- .sim_time_matrix(config$psi_jt, "psi_jt",
                                         par$nticks, par$npatches)
          if (any(par$psi_jt < 0 | par$psi_jt > 1)) {
               stop("`psi_jt` must lie in [0, 1]; the beta-CDF decay map and the ",
                    "psi-normalisation both assume it.", call. = FALSE)
          }
     }

     # PERF and parity: the Python engine caches
     # `-expm1(-d_jt)` once at check() time (susceptible.py:100-103) as a
     # float32 matrix, and seven draw sites read it. Precompute the same thing
     # in double here -- see the float32 discussion in migrate-laser-r.md
     # section 4.5; Tier B pins the difference rather than assuming it away.
     par$non_disease_death_prob_jt <- -expm1(-par$d_jt)

     # -- initial conditions ---------------------------------------------------
     for (nm in intersect(names(.SIM_INITIAL_FIELDS), par$compartments)) {
          field <- .SIM_INITIAL_FIELDS[[nm]]
          if (!is.null(config[[field]])) {
               par[[field]] <- .sim_patch_vector(config[[field]], field,
                                                  par$npatches, integral = TRUE)
          }
     }
     if (any(c("Isym", "Iasym") %in% par$compartments)) {
          par$I_j_initial <- .sim_patch_vector(config$I_j_initial, "I_j_initial",
                                               par$npatches, integral = TRUE)
          # Scalar in the engine (`params.py` scalars table), not per-patch.
          # Loading it as a patch vector would accept a 40-vector that
          # `np.float32()` rejects outright. float32 because it multiplies into
          # `round(sigma * progressing)` -- see .sim_f32().
          par$sigma <- .sim_f32(
               .sim_scalar(config$sigma, "sigma", lower = 0, upper = 1))
     }

     # -- parameters feeding the deterministic precomputation ------------------
     par$components <- components

     # `DerivedValues` reads `tau_i`, `pi_ij` and `beta_jt_human` -- the three
     # things this block and sim_precompute() build for `HumanToHuman`. The
     # Python component does not build them itself either; it relies on
     # HumanToHuman having run first and its `check()` fails outright if it has
     # not. Requiring the same inputs for either component is the same
     # constraint without the ordering dependency.
     if (any(c("HumanToHuman", "DerivedValues") %in% components)) {
          for (nm in c("latitude", "longitude", "beta_j0_hum",
                       "a_1_j", "b_1_j", "a_2_j", "b_2_j")) {
               par[[nm]] <- .sim_patch_vector(config[[nm]], nm, par$npatches)
          }
          par$p <- .sim_scalar(config$p, "p", positive = TRUE)
          par$mobility_omega <- .sim_scalar(config$mobility_omega, "mobility_omega")
          par$mobility_gamma <- .sim_scalar(config$mobility_gamma, "mobility_gamma")

          # The gravity model's populations are the INITIAL compartment sums,
          # read straight off the config rather than from the running N: the
          # Python engine builds pi_ij once in HumanToHuman.__init__ from
          # S + E + I + R + V1 + V2 at t = 0 and never rebuilds it. Note it
          # sums the raw `I_j_initial`, NOT the sigma-split Isym/Iasym, so the
          # total is the same either way -- but reading the config field keeps
          # it independent of which components happen to be in the pipeline.
          par$N_j_gravity <- Reduce(`+`, lapply(
               c("S_j_initial", "E_j_initial", "I_j_initial", "R_j_initial",
                 "V1_j_initial", "V2_j_initial"),
               function(nm) .sim_patch_vector(config[[nm]] %||% 0, nm,
                                              par$npatches, integral = TRUE)))
     }

     if ("EnvToHuman" %in% components) {
          par$beta_j0_env <- .sim_patch_vector(config$beta_j0_env,
                                               "beta_j0_env", par$npatches)
     }

     # -- dynamics parameters, by component ------------------------------------
     # Each component caches its constant hazard's `-expm1(-rate)` once in
     # `check()` (e.g. `self._waning_prob`, `self._gamma_1_prob`). The `*_prob`
     # fields below are those caches, named to match so a reader can line them
     # up against the Python source. The engine computes them in float32; these
     # are double -- Tier B pins the difference rather than assuming it away.

     if ("Recovered" %in% components) {
          par$epsilon     <- .sim_scalar(config$epsilon, "epsilon", lower = 0)
          par$waning_prob <- -expm1(-par$epsilon)
     }

     if ("Infectious" %in% components) {
          par$iota    <- .sim_scalar(config$iota,    "iota",    lower = 0)
          par$gamma_1 <- .sim_scalar(config$gamma_1, "gamma_1", lower = 0)
          par$gamma_2 <- .sim_scalar(config$gamma_2, "gamma_2", lower = 0)
          par$iota_prob    <- -expm1(-par$iota)
          par$gamma_1_prob <- -expm1(-par$gamma_1)
          par$gamma_2_prob <- -expm1(-par$gamma_2)

          par$rho        <- .sim_scalar(config$rho,        "rho",        lower = 0, upper = 1)
          par$rho_deaths <- .sim_scalar(config$rho_deaths, "rho_deaths", lower = 0, upper = 1)

          # chi divides the reported-case draw, so zero is a division by zero
          # rather than a merely odd value.
          # float32: both divide a draw inside a `round()` that lands in the
          # integer reported_cases channel.
          par$chi_endemic  <- .sim_f32(.sim_scalar(config$chi_endemic,
                                                       "chi_endemic",  positive = TRUE))
          par$chi_epidemic <- .sim_f32(.sim_scalar(config$chi_epidemic,
                                                       "chi_epidemic", positive = TRUE))

          par$delta_reporting_cases  <- .sim_lag(config$delta_reporting_cases,
                                                 "delta_reporting_cases")
          par$delta_reporting_deaths <- .sim_lag(config$delta_reporting_deaths,
                                                 "delta_reporting_deaths")

          for (nm in c("mu_j_baseline", "mu_j_slope", "mu_j_epidemic_factor")) {
               par[[nm]] <- .sim_patch_vector(config[[nm]], nm, par$npatches)
          }
          # mu_j_slope is the one mu term the engine puts no sign constraint on.
          if (any(par$mu_j_baseline < 0)) {
               stop("`mu_j_baseline` is negative at patch(es) ",
                    .sim_fmt(which(par$mu_j_baseline < 0)), ".", call. = FALSE)
          }
          if (any(par$mu_j_epidemic_factor < 0)) {
               stop("`mu_j_epidemic_factor` is negative at patch(es) ",
                    .sim_fmt(which(par$mu_j_epidemic_factor < 0)), ".", call. = FALSE)
          }

          # Scalar OR per-patch in the engine; broadcasting covers both.
          # float32: it sits on the right of two comparisons whose outcomes are
          # discrete (the epidemic flag, and the endemic/epidemic chi choice).
          par$epidemic_threshold <- .sim_f32(.sim_patch_vector(
               config$epidemic_threshold, "epidemic_threshold", par$npatches,
               lower = 0))
     }

     if ("Vaccinated" %in% components) {
          par$omega_1 <- .sim_scalar(config$omega_1, "omega_1", lower = 0)
          par$omega_2 <- .sim_scalar(config$omega_2, "omega_2", lower = 0)
          par$omega_1_prob <- -expm1(-par$omega_1)
          par$omega_2_prob <- -expm1(-par$omega_2)
          # float32: each scales a dose count inside a `round()`.
          par$phi_1 <- .sim_f32(.sim_scalar(config$phi_1, "phi_1", lower = 0, upper = 1))
          par$phi_2 <- .sim_f32(.sim_scalar(config$phi_2, "phi_2", lower = 0, upper = 1))

          for (nm in c("nu_1_jt", "nu_2_jt")) {
               # float32: `round(nu_*_jt[tick])` is the delivered dose count.
               par[[nm]] <- .sim_f32(.sim_time_matrix(config[[nm]], nm,
                                                          par$nticks, par$npatches))
               dim(par[[nm]]) <- c(par$nticks, par$npatches)
               if (any(par[[nm]] < 0)) {
                    stop(sprintf("`%s` has negative dose counts.", nm), call. = FALSE)
               }
          }

          # Dose-one donor compartments, in the order the engine iterates them:
          # the per-compartment pro-rata split rounds independently, so the
          # ORDER is observable in the results and is not ours to normalise.
          src <- config$nu_jt_sources %||% c("S", "E", "Isym", "Iasym", "R")
          src <- as.character(src)
          unknown <- setdiff(src, c("S", "E", "Isym", "Iasym", "R", "V1", "V2"))
          if (length(unknown)) {
               stop("`nu_jt_sources` names unknown compartment(s): ",
                    paste(unknown, collapse = ", "), ".", call. = FALSE)
          }
          if (anyDuplicated(src)) {
               stop("`nu_jt_sources` contains duplicates; each donor compartment ",
                    "may appear once.", call. = FALSE)
          }
          # The engine filters sources to those actually present via `hasattr`.
          par$nu_jt_sources <- intersect(src, par$compartments)
     }

     if (any(c("HumanToHuman", "EnvToHuman", "DerivedValues") %in% components)) {
          par$tau_i <- .sim_patch_vector(config$tau_i, "tau_i", par$npatches,
                                         lower = 0, upper = 1)
          # `1 - tau_i` is a float32 array in the engine and both transmission
          # components use it as `round(local_frac * S_next)` -- an integer
          # binomial `n`. Reproduce the stored precision; see .sim_f32().
          par$local_frac <- .sim_f32(1 - par$tau_i)
     }

     if ("HumanToHuman" %in% components) {
          # alpha_1 is scalar-or-per-patch in the engine and must be in (0, 1];
          # alpha_2 is a plain scalar.
          par$alpha_1 <- .sim_patch_vector(config$alpha_1, "alpha_1",
                                           par$npatches, upper = 1)
          if (any(par$alpha_1 <= 0)) {
               stop("`alpha_1` must be in (0, 1]; got a value <= 0 at patch(es) ",
                    .sim_fmt(which(par$alpha_1 <= 0)), ".", call. = FALSE)
          }
          par$alpha_2 <- .sim_scalar(config$alpha_2, "alpha_2", positive = TRUE)
     }

     if (any(c("EnvToHuman", "Environmental") %in% components)) {
          par$theta_j <- .sim_patch_vector(config$theta_j, "theta_j",
                                           par$npatches, lower = 0, upper = 1)
     }

     if ("EnvToHuman" %in% components) {
          # kappa is the half-saturation constant in W / (kappa + W); it is added
          # to W, so kappa = 0 with W = 0 is 0/0.
          par$kappa <- .sim_scalar(config$kappa, "kappa", positive = TRUE)
     }

     if ("Environmental" %in% components) {
          par$zeta_1 <- .sim_scalar(config$zeta_1, "zeta_1", lower = 0)
          par$zeta_2 <- .sim_scalar(config$zeta_2, "zeta_2", lower = 0)
     }

     if ("Environmental" %in% components) {
          par$decay_days_short <- .sim_scalar(config$decay_days_short,
                                              "decay_days_short", positive = TRUE)
          par$decay_days_long  <- .sim_scalar(config$decay_days_long,
                                              "decay_days_long", positive = TRUE)
          par$decay_shape_1 <- .sim_scalar(config$decay_shape_1,
                                           "decay_shape_1", positive = TRUE)
          par$decay_shape_2 <- .sim_scalar(config$decay_shape_2,
                                           "decay_shape_2", positive = TRUE)
     }

     sim_precompute(par)
}

# Validate a scalar parameter. Length > 1 is an error rather than a silent
# take-the-first, which is how a per-patch vector supplied for a scalar slot
# would otherwise go unnoticed.
# Round a double to float32 precision, via a round-trip through a 4-byte IEEE
# single. R has no float32 type, so this is the only faithful way to reproduce a
# value the Python engine *stores* as float32.
#
# Used sparingly and deliberately. The rule (see migrate-laser-r.md section 4.5):
#
#   * Where the oracle's single precision only reaches a FLOAT output channel,
#     the R engine stays in double -- it is the more accurate of the two, and
#     Tier B pins the difference with a tolerance. `pi_ij` is the worked example.
#   * Where it reaches an INTEGER -- a draw argument, a `round()` that lands in a
#     compartment, or a comparison that yields a flag -- reproduce it. This is
#     not an accuracy question: an integer that differs by one decorrelates the
#     draw sequence and makes every later assertion in the replay meaningless.
#
# The float32 fields that reach an integer, and where:
#
#   local_frac (1 - tau_i)  round(local_frac * S_next)   HumanToHuman, EnvToHuman
#   sigma                   round(sigma * progressing)   Infectious, and the t=0 split
#   phi_1                   round(phi_1 * comp_doses)    Vaccinated, dose one
#   phi_2                   round(phi_2 * doses2)        Vaccinated, dose two
#   nu_1_jt, nu_2_jt        round(nu_*_jt[tick])         Vaccinated, both schedules
#   chi_endemic/_epidemic   round(drawn / chi_eff)       Infectious, reported cases
#   epidemic_threshold      Isym > threshold * N         Infectious, epidemic flag
#                           frac < threshold             Infectious, chi selection
#
# Everything else the engine stores as float32 -- `d_jt`, `b_jt`, `mu_j_*`,
# `alpha_*`, `theta_j`, `kappa`, `zeta_*`, `beta_*`, `psi_jt` -- reaches only a
# float: a hazard compared under the combined tolerance, or a float output
# channel. Those stay in double, which is the more accurate of the two.
.sim_f32 <- function(x) {
     readBin(writeBin(as.double(x), raw(), size = 4L), what = "double",
             size = 4L, n = length(x), endian = .Platform$endian)
}

.sim_scalar <- function(x, name, positive = FALSE,
                        lower = NULL, upper = NULL) {
     if (is.null(x)) {
          stop(sprintf("Config is missing required scalar `%s`.", name), call. = FALSE)
     }
     if (length(x) != 1L) {
          stop(sprintf("`%s` must be a single value; got length %d.", name, length(x)),
               call. = FALSE)
     }
     x <- as.numeric(x)
     .sim_reject_nonfinite(x, name)
     if (positive && x <= 0) {
          stop(sprintf("`%s` must be positive; got %s.", name, format(x)), call. = FALSE)
     }
     if (!is.null(lower) && x < lower) {
          stop(sprintf("`%s` must be >= %s; got %s.", name, format(lower), format(x)),
               call. = FALSE)
     }
     if (!is.null(upper) && x > upper) {
          stop(sprintf("`%s` must be <= %s; got %s.", name, format(upper), format(x)),
               call. = FALSE)
     }
     x
}

# A reporting delay. The engine coerces these with `np.int32(...)` and then
# compares `tick - delay >= 0`, so a non-integral value would be silently
# truncated toward zero and quietly shift every reported series by a day.
# Reject it instead.
.sim_lag <- function(x, name) {
     x <- .sim_scalar(x, name, lower = 0)
     if (x != trunc(x)) {
          stop(sprintf("`%s` must be a whole number of days; got %s.",
                       name, format(x)), call. = FALSE)
     }
     as.integer(x)
}

# Compartments seeded directly from a single config field. `Isym` / `Iasym`
# are deliberately absent: they are not seeded one-to-one but split out of the
# shared `I_j_initial` by sigma (infectious.py:83-84), which
# `sim_seed_state()` handles explicitly.
.SIM_INITIAL_FIELDS <- c(
     S = "S_j_initial", E = "E_j_initial", R = "R_j_initial",
     V1 = "V1_j_initial", V2 = "V2_j_initial"
)

# component -> compartments it brings into existence
.SIM_COMPONENT_COMPARTMENTS <- list(
     Susceptible = "S", Exposed = "E", Infectious = c("Isym", "Iasym"),
     Recovered = "R", Vaccinated = c("V1", "V2")
)

.sim_compartments <- function(components) {
     ordered <- c("S", "E", "Isym", "Iasym", "R", "V1", "V2")
     present <- unlist(.SIM_COMPONENT_COMPARTMENTS[
          intersect(names(.SIM_COMPONENT_COMPARTMENTS), components)],
          use.names = FALSE)
     ordered[ordered %in% present]
}

.sim_load_config <- function(config) {
     if (is.character(config)) {
          if (length(config) != 1L) {
               stop("A config path must be a single string.", call. = FALSE)
          }
          if (!file.exists(config)) {
               stop(sprintf("Config file not found: %s", config), call. = FALSE)
          }
          if (!grepl("\\.json(\\.gz)?$", config)) {
               stop(sprintf(paste0("Unsupported config format: %s. The engine ",
                                   "reads .json and .json.gz only."), config),
                    call. = FALSE)
          }
          # Cached on path + size + mtime: a loop over the same config path
          # would otherwise re-parse 5.76 MB on every simulation. Equivalent to
          # `fromJSON(config, simplifyVector = TRUE)` -- verified identical.
          return(.mosaic_read_json_cached(config))
     }
     if (!is.list(config)) {
          stop(sprintf("`config` must be a list or a path to a .json file, not %s.",
                       class(config)[1]), call. = FALSE)
     }
     config
}

.sim_nticks <- function(config) {
     if (is.null(config$date_start) || is.null(config$date_stop)) {
          stop("Config must carry `date_start` and `date_stop`.", call. = FALSE)
     }
     # as.Date() *errors* on an unrecognised string rather than returning NA,
     # so an is.na() guard here would be dead code -- the parse has to be
     # wrapped to produce a message that names the offending field.
     parse_date <- function(value, field) {
          out <- tryCatch(as.Date(value), error = function(e) NA)
          if (length(out) != 1L || is.na(out)) {
               stop(sprintf("Unparseable `%s`: %s. Expected YYYY-MM-DD.",
                            field, paste(format(value), collapse = ", ")),
                    call. = FALSE)
          }
          out
     }
     start <- parse_date(config$date_start, "date_start")
     stop_ <- parse_date(config$date_stop, "date_stop")
     if (stop_ < start) {
          stop(sprintf("date_stop (%s) precedes date_start (%s).", stop_, start),
               call. = FALSE)
     }
     # `date_stop` is inclusive: nticks = (stop - start).days + 1, matching
     # params.py.
     as.integer(as.numeric(stop_ - start)) + 1L
}

# Normalise a time-varying field to [nticks, npatches], broadcasting a scalar
# or a per-patch vector, and rejecting anything whose dimensions do not line up.
.sim_time_matrix <- function(x, name, nticks, npatches) {

     if (is.null(x)) {
          stop(sprintf("Config is missing required time-varying field `%s`.", name),
               call. = FALSE)
     }

     if (is.data.frame(x)) x <- as.matrix(x)

     if (!is.array(x) && length(x) == 1L) {
          out <- matrix(as.numeric(x), nrow = nticks, ncol = npatches)
     } else if (!is.array(x) && length(x) == npatches) {
          # per-patch constant over time
          out <- matrix(as.numeric(x), nrow = nticks, ncol = npatches, byrow = TRUE)
     } else if (is.matrix(x)) {
          d <- dim(x)
          if (identical(d, c(npatches, nticks))) {
               # the on-disk [npatches, nticks] orientation: transpose once, here
               out <- t(matrix(as.numeric(x), nrow = npatches, ncol = nticks))
          } else if (identical(d, c(nticks, npatches))) {
               out <- matrix(as.numeric(x), nrow = nticks, ncol = npatches)
          } else {
               stop(sprintf(paste0("`%s` has dimensions %d x %d, which matches ",
                                   "neither [npatches, nticks] = %d x %d nor ",
                                   "[nticks, npatches] = %d x %d."),
                            name, d[1], d[2], npatches, nticks, nticks, npatches),
                    call. = FALSE)
          }
     } else {
          stop(sprintf(paste0("`%s` must be a scalar, a length-%d per-patch ",
                              "vector, or a matrix; got %s of length %d."),
                       name, npatches, class(x)[1], length(x)), call. = FALSE)
     }

     .sim_reject_nonfinite(out, name)
     out
}

.sim_patch_vector <- function(x, name, npatches, integral = FALSE,
                              lower = NULL, upper = NULL) {

     if (length(x) == 1L) x <- rep(x, npatches)
     if (length(x) != npatches) {
          stop(sprintf("`%s` has length %d; expected %d (npatches) or 1.",
                       name, length(x), npatches), call. = FALSE)
     }
     x <- as.numeric(x)
     .sim_reject_nonfinite(x, name)

     if (!is.null(lower) && any(x < lower)) {
          stop(sprintf("`%s` is below %s at patch(es) %s.", name, lower,
                       .sim_fmt(which(x < lower))), call. = FALSE)
     }
     if (!is.null(upper) && any(x > upper)) {
          stop(sprintf("`%s` exceeds %s at patch(es) %s.", name, upper,
                       .sim_fmt(which(x > upper))), call. = FALSE)
     }
     if (integral) {
          if (any(x < 0)) {
               stop(sprintf("`%s` is negative at patch(es) %s; compartment counts must be >= 0.",
                            name, .sim_fmt(which(x < 0))), call. = FALSE)
          }
          if (any(x != trunc(x))) {
               stop(sprintf("`%s` is non-integral at patch(es) %s; compartment counts must be whole.",
                            name, .sim_fmt(which(x != trunc(x)))), call. = FALSE)
          }
          return(as.integer(x))
     }
     x
}

# NA / NaN / Inf must be rejected at the boundary, not carried in. R's
# `rbinom(n, size, prob)` returns NA for a non-finite `prob` without warning,
# so a single bad config value would otherwise propagate silently through a
# whole run and surface only as an NA likelihood.
.sim_reject_nonfinite <- function(x, name) {
     bad <- which(!is.finite(x))
     if (length(bad)) {
          stop(sprintf("`%s` contains %d non-finite value(s) (NA/NaN/Inf) at index %s.",
                       name, length(bad), .sim_fmt(bad)), call. = FALSE)
     }
     invisible(TRUE)
}

#' Seed t=0 state from the validated parameters
#'
#' Mirrors the Python components' \code{__init__} methods, each of which writes
#' its own compartment's \code{[0]} row from the matching \code{*_j_initial}
#' parameter.
#'
#' @param state State list from \code{sim_alloc_state()}.
#' @param par Parameters from \code{sim_params()}.
#' @return The state list with row 1 seeded.
#' @keywords internal
sim_seed_state <- function(state, par) {

     for (nm in intersect(names(.SIM_INITIAL_FIELDS), par$compartments)) {
          field <- .SIM_INITIAL_FIELDS[[nm]]
          if (!is.null(par[[field]])) state[[nm]][[1L]] <- par[[field]]
     }

     # `I_j_initial` splits by sigma, and the split is observable: Isym takes
     # `round(sigma * I)` and Iasym takes the remainder, so the two always sum
     # back to I exactly (infectious.py:83-84). Note `as.integer(round(x))`,
     # never `as.integer(x)` -- NumPy's np.round is round-half-to-even and
     # agrees with R's round(), but as.integer() truncates.
     if (any(c("Isym", "Iasym") %in% par$compartments)) {
          isym <- as.integer(round(par$sigma * par$I_j_initial))
          if ("Isym" %in% par$compartments)  state$Isym[[1L]]  <- isym
          if ("Iasym" %in% par$compartments) state$Iasym[[1L]] <- par$I_j_initial - isym
     }

     state
}
