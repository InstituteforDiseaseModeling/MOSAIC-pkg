#' Normalise and validate a config for the R transmission engine
#'
#' Python's \code{params.py} is 1,009 lines, and it is tempting to call it
#' "just coercion" and drop it -- but that is only safe if the engine's sole
#' input is \code{make_LASER_config()} output, and it is not: the engine also
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
laser_params <- function(config, components = LASER_PIPELINE) {

     config <- .laser_load_config(config)

     par <- list()
     par$config <- config
     par$seed <- config$seed

     # -- shape ----------------------------------------------------------------
     if (is.null(config$location_name)) {
          stop("Config is missing `location_name`; the engine cannot infer npatches.",
               call. = FALSE)
     }
     par$npatches <- length(config$location_name)

     par$nticks <- .laser_nticks(config)

     # Which compartments exist depends on the pipeline subset in play, and
     # `Census` must sum exactly these. Mirrors the Python components' lazy
     # `hasattr` checks, but resolved up front so it is inspectable.
     par$compartments <- .laser_compartments(components)

     # -- required scalars and vectors -----------------------------------------
     par$check_invariants <- isTRUE(config$check_invariants %||% TRUE)

     # -- time-varying matrices ------------------------------------------------
     # Broadcast then transpose to [nticks, npatches].
     for (nm in c("b_jt", "d_jt")) {
          par[[nm]] <- .laser_time_matrix(config[[nm]], nm,
                                          par$nticks, par$npatches)
     }
     if (any(c("EnvToHuman", "Environmental") %in% components)) {
          par$psi_jt <- .laser_time_matrix(config$psi_jt, "psi_jt",
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
     for (nm in intersect(names(.LASER_INITIAL_FIELDS), par$compartments)) {
          field <- .LASER_INITIAL_FIELDS[[nm]]
          if (!is.null(config[[field]])) {
               par[[field]] <- .laser_patch_vector(config[[field]], field,
                                                  par$npatches, integral = TRUE)
          }
     }
     if (any(c("Isym", "Iasym") %in% par$compartments)) {
          par$I_j_initial <- .laser_patch_vector(config$I_j_initial, "I_j_initial",
                                                 par$npatches, integral = TRUE)
          par$sigma <- .laser_patch_vector(config$sigma, "sigma", par$npatches,
                                           lower = 0, upper = 1)
     }

     # -- parameters feeding the deterministic precomputation ------------------
     par$components <- components

     if ("HumanToHuman" %in% components) {
          for (nm in c("latitude", "longitude", "beta_j0_hum",
                       "a_1_j", "b_1_j", "a_2_j", "b_2_j")) {
               par[[nm]] <- .laser_patch_vector(config[[nm]], nm, par$npatches)
          }
          par$p <- .laser_scalar(config$p, "p", positive = TRUE)
          par$mobility_omega <- .laser_scalar(config$mobility_omega, "mobility_omega")
          par$mobility_gamma <- .laser_scalar(config$mobility_gamma, "mobility_gamma")

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
               function(nm) .laser_patch_vector(config[[nm]] %||% 0, nm,
                                                par$npatches, integral = TRUE)))
     }

     if ("EnvToHuman" %in% components) {
          par$beta_j0_env <- .laser_patch_vector(config$beta_j0_env,
                                                 "beta_j0_env", par$npatches)
     }

     if ("Environmental" %in% components) {
          par$decay_days_short <- .laser_scalar(config$decay_days_short,
                                                "decay_days_short", positive = TRUE)
          par$decay_days_long  <- .laser_scalar(config$decay_days_long,
                                                "decay_days_long", positive = TRUE)
          par$decay_shape_1 <- .laser_scalar(config$decay_shape_1,
                                             "decay_shape_1", positive = TRUE)
          par$decay_shape_2 <- .laser_scalar(config$decay_shape_2,
                                             "decay_shape_2", positive = TRUE)
     }

     laser_precompute(par)
}

# Validate a scalar parameter. Length > 1 is an error rather than a silent
# take-the-first, which is how a per-patch vector supplied for a scalar slot
# would otherwise go unnoticed.
.laser_scalar <- function(x, name, positive = FALSE) {
     if (is.null(x)) {
          stop(sprintf("Config is missing required scalar `%s`.", name), call. = FALSE)
     }
     if (length(x) != 1L) {
          stop(sprintf("`%s` must be a single value; got length %d.", name, length(x)),
               call. = FALSE)
     }
     x <- as.numeric(x)
     .laser_reject_nonfinite(x, name)
     if (positive && x <= 0) {
          stop(sprintf("`%s` must be positive; got %s.", name, format(x)), call. = FALSE)
     }
     x
}

# Compartments seeded directly from a single config field. `Isym` / `Iasym`
# are deliberately absent: they are not seeded one-to-one but split out of the
# shared `I_j_initial` by sigma (infectious.py:83-84), which
# `laser_seed_state()` handles explicitly.
.LASER_INITIAL_FIELDS <- c(
     S = "S_j_initial", E = "E_j_initial", R = "R_j_initial",
     V1 = "V1_j_initial", V2 = "V2_j_initial"
)

# component -> compartments it brings into existence
.LASER_COMPONENT_COMPARTMENTS <- list(
     Susceptible = "S", Exposed = "E", Infectious = c("Isym", "Iasym"),
     Recovered = "R", Vaccinated = c("V1", "V2")
)

.laser_compartments <- function(components) {
     ordered <- c("S", "E", "Isym", "Iasym", "R", "V1", "V2")
     present <- unlist(.LASER_COMPONENT_COMPARTMENTS[
          intersect(names(.LASER_COMPONENT_COMPARTMENTS), components)],
          use.names = FALSE)
     ordered[ordered %in% present]
}

.laser_load_config <- function(config) {
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
          return(jsonlite::fromJSON(config, simplifyVector = TRUE))
     }
     if (!is.list(config)) {
          stop(sprintf("`config` must be a list or a path to a .json file, not %s.",
                       class(config)[1]), call. = FALSE)
     }
     config
}

.laser_nticks <- function(config) {
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
.laser_time_matrix <- function(x, name, nticks, npatches) {

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

     .laser_reject_nonfinite(out, name)
     out
}

.laser_patch_vector <- function(x, name, npatches, integral = FALSE,
                                lower = NULL, upper = NULL) {

     if (length(x) == 1L) x <- rep(x, npatches)
     if (length(x) != npatches) {
          stop(sprintf("`%s` has length %d; expected %d (npatches) or 1.",
                       name, length(x), npatches), call. = FALSE)
     }
     x <- as.numeric(x)
     .laser_reject_nonfinite(x, name)

     if (!is.null(lower) && any(x < lower)) {
          stop(sprintf("`%s` is below %s at patch(es) %s.", name, lower,
                       .laser_fmt(which(x < lower))), call. = FALSE)
     }
     if (!is.null(upper) && any(x > upper)) {
          stop(sprintf("`%s` exceeds %s at patch(es) %s.", name, upper,
                       .laser_fmt(which(x > upper))), call. = FALSE)
     }
     if (integral) {
          if (any(x < 0)) {
               stop(sprintf("`%s` is negative at patch(es) %s; compartment counts must be >= 0.",
                            name, .laser_fmt(which(x < 0))), call. = FALSE)
          }
          if (any(x != trunc(x))) {
               stop(sprintf("`%s` is non-integral at patch(es) %s; compartment counts must be whole.",
                            name, .laser_fmt(which(x != trunc(x)))), call. = FALSE)
          }
          return(as.integer(x))
     }
     x
}

# NA / NaN / Inf must be rejected at the boundary, not carried in. R's
# `rbinom(n, size, prob)` returns NA for a non-finite `prob` without warning,
# so a single bad config value would otherwise propagate silently through a
# whole run and surface only as an NA likelihood.
.laser_reject_nonfinite <- function(x, name) {
     bad <- which(!is.finite(x))
     if (length(bad)) {
          stop(sprintf("`%s` contains %d non-finite value(s) (NA/NaN/Inf) at index %s.",
                       name, length(bad), .laser_fmt(bad)), call. = FALSE)
     }
     invisible(TRUE)
}

#' Seed t=0 state from the validated parameters
#'
#' Mirrors the Python components' \code{__init__} methods, each of which writes
#' its own compartment's \code{[0]} row from the matching \code{*_j_initial}
#' parameter.
#'
#' @param state State list from \code{laser_alloc_state()}.
#' @param par Parameters from \code{laser_params()}.
#' @return The state list with row 1 seeded.
#' @keywords internal
laser_seed_state <- function(state, par) {

     for (nm in intersect(names(.LASER_INITIAL_FIELDS), par$compartments)) {
          field <- .LASER_INITIAL_FIELDS[[nm]]
          if (!is.null(par[[field]])) state[[nm]][1L, ] <- par[[field]]
     }

     # `I_j_initial` splits by sigma, and the split is observable: Isym takes
     # `round(sigma * I)` and Iasym takes the remainder, so the two always sum
     # back to I exactly (infectious.py:83-84). Note `as.integer(round(x))`,
     # never `as.integer(x)` -- NumPy's np.round is round-half-to-even and
     # agrees with R's round(), but as.integer() truncates.
     if (any(c("Isym", "Iasym") %in% par$compartments)) {
          isym <- as.integer(round(par$sigma * par$I_j_initial))
          if ("Isym" %in% par$compartments)  state$Isym[1L, ]  <- isym
          if ("Iasym" %in% par$compartments) state$Iasym[1L, ] <- par$I_j_initial - isym
     }

     state
}
