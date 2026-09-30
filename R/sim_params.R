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
#' @param mode \code{"rng"} (production) or \code{"replay"} (the laser-cholera
#'   parity harness); decides which mortality inputs are read.
#' @return A list of validated engine parameters, including the config as
#'   supplied (a file path is parsed, a list is kept unchanged) under
#'   \code{$config}.
#' @keywords internal
sim_params <- function(config, components = SIM_PIPELINE, mode = c("rng", "replay")) {

     mode <- match.arg(mode)
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
     # Every compartment in play needs its initial field. An absent (or
     # misspelled) field is refused, never read as zero: a missing S_j_initial
     # otherwise runs a complete, silent, all-zero epidemic that calibration
     # would only ever see as a bad fit. The oracle asserts the same fields
     # (params.py); .sim_patch_vector() refuses a NULL by name.
     for (nm in intersect(names(.SIM_INITIAL_FIELDS), par$compartments)) {
          field <- .SIM_INITIAL_FIELDS[[nm]]
          par[[field]] <- .sim_patch_vector(config[[field]], field,
                                             par$npatches, integral = TRUE)
     }
     if (any(c("Isym", "Iasym") %in% par$compartments)) {
          par$I_j_initial <- .sim_patch_vector(config$I_j_initial, "I_j_initial",
                                               par$npatches, integral = TRUE)
          # Scalar in the engine (`params.py` scalars table), not per-patch.
          # Loading it as a patch vector would accept a 40-vector that
          # `np.float32()` rejects outright. float32 because in replay it
          # multiplies into `round(sigma * progressing)` and the t=0
          # `round(sigma * I_j_initial)`, both integers -- see .sim_f32(). In rng
          # mode it is a binomial probability, where the rounding is harmless.
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
          # All six fields are required here too, whatever the pipeline subset:
          # a missing one would otherwise shrink the gravity populations
          # silently.
          par$N_j_gravity <- Reduce(`+`, lapply(
               c("S_j_initial", "E_j_initial", "I_j_initial", "R_j_initial",
                 "V1_j_initial", "V2_j_initial"),
               function(nm) .sim_patch_vector(config[[nm]], nm,
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

          # Scalar OR per-patch in the engine; broadcasting covers both.
          # float32: it sits on the right of a comparison whose outcome is
          # discrete (the endemic/epidemic chi choice, and in replay mode the
          # mortality epidemic flag).
          par$epidemic_threshold <- .sim_f32(.sim_patch_vector(
               config$epidemic_threshold, "epidemic_threshold", par$npatches,
               lower = 0))

          if (identical(mode, "replay")) {
               # Replay reproduces laser-cholera 0.16.1 draw for draw, and the
               # oracle's mortality is a daily hazard on the symptomatic stock:
               # mu_j_baseline x (1 + mu_j_epidemic_factor x flag), reported
               # delta_reporting_deaths days after the death. The fixtures carry
               # exactly those inputs, so replay reads them and nothing else.
               par$delta_reporting_deaths <- .sim_lag(config$delta_reporting_deaths,
                                                      "delta_reporting_deaths")
               for (nm in c("mu_j_baseline", "mu_j_epidemic_factor")) {
                    par[[nm]] <- .sim_patch_vector(config[[nm]], nm, par$npatches)
               }
               if (any(par$mu_j_baseline < 0)) {
                    stop("`mu_j_baseline` is negative at patch(es) ",
                         .sim_fmt(which(par$mu_j_baseline < 0)), ".", call. = FALSE)
               }
               if (any(par$mu_j_epidemic_factor < 0)) {
                    stop("`mu_j_epidemic_factor` is negative at patch(es) ",
                         .sim_fmt(which(par$mu_j_epidemic_factor < 0)), ".", call. = FALSE)
               }
          } else {
               # Production mortality (v0.96.0): the reported CFR `mu_jt`, one
               # value per location and day, is converted to the probability that
               # a new symptomatic onset is fatal at that tick. The conversion is
               # exact: E[reported deaths] / E[reported cases] = mu_jt on
               # epidemic-PPV ticks, whatever gamma_1 or the lags are.
               par$mu_jt      <- .sim_mu_jt(config, par)
               par$p_fatal_jt <- .sim_p_fatal(par)
          }
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
          # V1/V2 are not valid donors: the Vaccinated phase reads its donor pool
          # from row tick + 1, which holds no V1/V2 until the phase itself writes
          # them at the end, so they would count as zero and silently deliver no
          # doses. make_simulation_config() accepts the same five.
          unknown <- setdiff(src, c("S", "E", "Isym", "Iasym", "R"))
          if (length(unknown)) {
               stop("`nu_jt_sources` names compartment(s) that cannot donate dose-one ",
                    "vaccinees: ", paste(unknown, collapse = ", "),
                    ". Valid sources: S, E, Isym, Iasym, R.", call. = FALSE)
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
          # alpha_1 is scalar-or-per-patch in the engine and must be in (0, 1]
          # -- 0 would collapse the I-dependence to a constant.
          par$alpha_1 <- .sim_patch_vector(config$alpha_1, "alpha_1",
                                           par$npatches, upper = 1)
          if (any(par$alpha_1 <= 0)) {
               stop("`alpha_1` must be in (0, 1]; got a value <= 0 at patch(es) ",
                    .sim_fmt(which(par$alpha_1 <= 0)), ".", call. = FALSE)
          }
          # alpha_2 is a plain scalar in [0, 1]. Unlike alpha_1, ZERO IS VALID:
          # N^0 = 1 removes the population-size normalisation entirely, which is
          # density-dependent transmission. Both the config validator
          # (make_simulation_config: "alpha_2 in [0, 1]") and the model spec
          # ("determines density (0) vs frequency (1) dependence") call it legal,
          # so `positive = TRUE` here rejected a documented configuration. It
          # also left alpha_2 with no upper bound, accepting values > 1 that the
          # config validator forbids. Bound it on both sides instead.
          par$alpha_2 <- .sim_scalar(config$alpha_2, "alpha_2",
                                     lower = 0, upper = 1)
     }

     if (any(c("EnvToHuman", "Environmental") %in% components)) {
          par$theta_j <- .sim_patch_vector(config$theta_j, "theta_j",
                                           par$npatches, lower = 0, upper = 1)
     }

     if ("EnvToHuman" %in% components) {
          # kappa is the half-saturation constant in dose / (kappa + dose), where
          # dose is the per-capita W / N in rng mode (v0.89.0) and the raw W in
          # replay; it is added to the dose, so kappa = 0 with W = 0 is 0/0.
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
#   sigma                   round(sigma * progressing)   Infectious, and the t=0 split (replay;
#                                                        a binomial probability in rng mode)
#   phi_1                   round(phi_1 * comp_doses)    Vaccinated, dose one
#   phi_2                   round(phi_2 * doses2)        Vaccinated, dose two
#   nu_1_jt, nu_2_jt        round(nu_*_jt[tick])         Vaccinated, both schedules
#   chi_endemic/_epidemic   round(drawn / chi_eff)       Infectious, reported cases
#   epidemic_threshold      Isym > threshold * N         Infectious, epidemic flag (replay)
#                           frac < threshold             Infectious, chi selection
#
# Everything else the engine stores as float32 -- `d_jt`, `b_jt`, `mu_jt`, `mu_j_*` (replay),
# `alpha_*`, `theta_j`, `kappa`, `zeta_*`, `beta_*`, `psi_jt` -- reaches only a
# float: a hazard compared under the combined tolerance, or a float output
# channel. Those stay in double, which is the more accurate of the two.
.sim_f32 <- function(x) {
     readBin(writeBin(as.double(x), raw(), size = 4L), what = "double",
             size = 4L, n = length(x), endian = .Platform$endian)
}

# Validate a scalar parameter. Length > 1 is an error rather than a silent
# take-the-first, which is how a per-patch vector supplied for a scalar slot
# would otherwise go unnoticed.
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

# Fields that mark a config written for a pre-v0.96.0 mortality model: the daily
# hazard on the symptomatic stock (mu_j_baseline, mu_j_epidemic_factor and the B2
# target CFR_target they were derived from) and the older single-rate model (mu_j).
# Not the same list as .MOSAIC_REMOVED_MORTALITY_PARAMS (sample_parameters.R), the
# priors the sampler skips: mu_j_slope and delta_reporting_deaths do not mark a
# config as legacy, because make_simulation_config() accepts and drops them.
.MOSAIC_LEGACY_MORTALITY_FIELDS <- c("mu_j_baseline", "mu_j_epidemic_factor", "CFR_target", "mu_j")

# The reported CFR as a [nticks, npatches] matrix (row i is tick i - 1). The ONE
# resolver of config$mu_jt: the engine (.sim_mu_jt) and the integrated deaths
# likelihood (.mosaic_config_mu_jt) both call it, so they cannot disagree.
#
# A legacy config (any of .MOSAIC_LEGACY_MORTALITY_FIELDS) often carries a `mu_jt`
# matrix that no engine ever read, so that matrix is never trusted. Its
# calibrated per-location `CFR_target` -- the reported CFR that model targeted --
# becomes a constant `mu_jt` instead, and the user is told once per session. A
# legacy config without `CFR_target` has nothing to convert and is refused.
.mosaic_mu_jt_matrix <- function(config, nticks, npatches) {
     legacy <- .MOSAIC_LEGACY_MORTALITY_FIELDS[
          vapply(.MOSAIC_LEGACY_MORTALITY_FIELDS, function(f) !is.null(config[[f]]), logical(1))]
     if (length(legacy)) {
          if (is.null(config$CFR_target)) {
               stop("This config predates the v0.96.0 mortality model: it carries ",
                    paste0("`", legacy, "`", collapse = ", "), " but no `CFR_target` to ",
                    "convert. Rebuild it from a current config_default (see make_mu_jt()).",
                    call. = FALSE)
          }
          .mosaic_warn_once("legacy_mortality_config", paste0(
               "Config predates the v0.96.0 mortality model (it carries ",
               paste0("`", legacy, "`", collapse = ", "), "). Its per-location ",
               "`CFR_target` is used as a constant `mu_jt`; its other mortality fields, ",
               "`delta_reporting_deaths` and any legacy `mu_jt` are ignored. Rebuild the ",
               "config to use the time-varying WHO-annual `mu_jt`."))
          mu <- config$CFR_target
     } else {
          mu <- config$mu_jt
          if (is.null(mu)) {
               stop("Config is missing `mu_jt`, the reported case fatality ratio by location ",
                    "and day (see make_mu_jt()).", call. = FALSE)
          }
          # A one-location [1 x nticks] matrix can come back from JSON as a plain vector.
          if (npatches == 1L && !is.array(mu) && length(mu) == nticks && nticks > 1L)
               mu <- matrix(as.numeric(mu), nrow = 1L)
     }
     m <- .sim_time_matrix(mu, "mu_jt", nticks, npatches)
     bad <- m < 0 | m >= 1
     if (any(bad)) {
          w <- which(bad, arr.ind = TRUE)[1L, ]
          stop(sprintf("`mu_jt` must lie in [0, 1); patch %d at tick %d is %s.",
                       w[2], w[1] - 1L, format(m[w[1], w[2]])), call. = FALSE)
     }
     m
}

.sim_mu_jt <- function(config, par) .mosaic_mu_jt_matrix(config, par$nticks, par$npatches)

# Per-onset probability of a fatal outcome, [nticks, npatches]:
#   p = mu_jt * rho / (rho_deaths * chi_epidemic).
# Substituting into the observation model gives E[reported deaths] /
# E[reported cases] = mu_jt on epidemic-PPV ticks exactly. A probability of 1 or
# more means the requested reported CFR cannot be produced by these reporting
# parameters, so it is an error, never a clamp.
#
# Indexing. Row r of `p` (tick r - 1) is read during tick r - 1 and
# applied to the onsets that tick produces. Those onsets are written to state
# row r + 1, which is results column r of the TRIM_FIRST `new_symptomatic`
# channel: TRIM_FIRST columns hold the state at the END of a tick, so column r
# is day r's onsets. By results column, then, onsets in column c are fated at
# mu_jt[, c] -- no offset -- which is the alignment the integrated deaths
# likelihood (.mosaic_deaths_exposure()) and the post-hoc redraw
# (.mosaic_posthoc_deaths()) assume; test-review-engine-p-fatal-alignment.R
# pins it for the engine and the likelihood's exposure. "Recorded one row later" describes the state layout,
# not a one-day lag.
.sim_p_fatal <- function(par) {
     if (par$rho_deaths <= 0) {
          if (any(par$mu_jt > 0)) {
               stop("`rho_deaths` is 0, so no death is ever reported, yet `mu_jt` asks for ",
                    "a positive reported CFR.", call. = FALSE)
          }
          return(par$mu_jt)
     }
     p <- par$mu_jt * (par$rho / (par$rho_deaths * par$chi_epidemic))
     if (any(p >= 1)) {
          w <- which(p >= 1, arr.ind = TRUE)[1L, ]
          stop(sprintf(paste0("mu_jt * rho / (rho_deaths * chi_epidemic) reaches %s at patch %d, ",
                              "tick %d: no per-onset fatality probability produces that reported CFR ",
                              "(mu_jt %s, rho %s, rho_deaths %s, chi_epidemic %s)."),
                       format(p[w[1], w[2]]), w[2], w[1] - 1L, format(par$mu_jt[w[1], w[2]]),
                       format(par$rho), format(par$rho_deaths), format(par$chi_epidemic)),
               call. = FALSE)
     }
     p
}

# One warning per R session per key. PSOCK workers are separate sessions, so each
# warns at most once.
.mosaic_once <- new.env(parent = emptyenv())
.mosaic_warn_once <- function(key, message) {
     if (isTRUE(.mosaic_once[[key]])) return(invisible(FALSE))
     assign(key, TRUE, envir = .mosaic_once)
     warning(message, call. = FALSE)
     invisible(TRUE)
}

.sim_patch_vector <- function(x, name, npatches, integral = FALSE,
                              lower = NULL, upper = NULL) {

     if (is.null(x)) {
          stop(sprintf("Config is missing `%s`.", name), call. = FALSE)
     }
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
#' @param state State environment from \code{sim_alloc_state()}.
#' @param par Parameters from \code{sim_params()}.
#' @param ctl Draw controller from \code{sim_draws()}; in \code{"rng"} mode the
#'   symptomatic split of \code{I_j_initial} is drawn, otherwise (\code{NULL} or
#'   \code{"replay"}) it is the oracle's deterministic \code{round()}.
#' @return The state environment with row 1 seeded.
#' @keywords internal
sim_seed_state <- function(state, par, ctl = NULL) {

     r1 <- state$rows[[1L]]

     for (nm in intersect(names(.SIM_INITIAL_FIELDS), par$compartments)) {
          r1[[nm]] <- par[[.SIM_INITIAL_FIELDS[[nm]]]]
     }

     # `I_j_initial` splits by sigma, and the two halves always sum back to I
     # exactly. MODE-DEPENDENT, for the same reason as the per-tick split in
     # sim_phase_infectious() (v0.89.0): the oracle seeds Isym with the
     # deterministic `round(sigma * I)` (infectious.py:83-84), which is wrong in
     # the mean at small counts -- at sigma = 0.25 every patch seeded with 1 or 2
     # infections starts with no symptomatic at all. Production ("rng") draws
     # Binom(I, sigma) at the rng-only site `infectious/sigma_split_t0`; replay
     # keeps the oracle's form, because drawing here would consume a variate the
     # fixture never recorded. Note `as.integer(round(x))`, never
     # `as.integer(x)` -- np.round is round-half-to-even and agrees with R's
     # round(), but as.integer() truncates.
     if (any(c("Isym", "Iasym") %in% par$compartments)) {
          isym <- if (!is.null(ctl) && isTRUE(ctl$mode == "rng")) {
               .sim_binom(ctl, "infectious/sigma_split_t0", par$I_j_initial, par$sigma)
          } else {
               as.integer(round(par$sigma * par$I_j_initial))
          }
          if ("Isym" %in% par$compartments)  r1$Isym  <- isym
          if ("Iasym" %in% par$compartments) r1$Iasym <- par$I_j_initial - isym
     }

     state
}
