# Parameter names <-> locations.
#
# A location-specific parameter is stored as "<base>_<location>" (convert_config_to_matrix(),
# samples.parquet), where <location> is an element of the config's location_name. Location names
# are not restricted to ISO codes: any names that pass .mosaic_check_location_names() work (e.g.
# "MOZ_Cabo_Delgado", "MOZ_Zambézia", "KAA", "Lusaka"). Code that splits a parameter name into
# base and location uses these helpers; code that already knows a parameter's location (e.g. from
# a posterior_quantiles.csv row) strips the exact suffix with .mosaic_strip_location().


# Columns of samples.parquet that are not parameters (bookkeeping, weights and flags, and the
# retired delta_reporting_deaths of older runs). They are never split, and no location name may make
# one of them look like "<base>_<location>".
.MOSAIC_SAMPLES_META_COLS <- c(
     "sim", "iter", "seed_sim", "seed_iter", "likelihood",
     "is_valid", "is_finite", "is_retained", "is_outlier", "is_best_model",
     "is_best_subset", "is_best_subset_opt",
     "weight_all", "weight_retained", "weight_best", "weight_best_opt",
     "delta_reporting_deaths"
)


# Names that are never split: the config parameters convert_config_to_matrix() keeps (a global one
# appears under its own name) and the samples bookkeeping columns.
.mosaic_unsplit_names <- function() unique(c(.MOSAIC_CONFIG_PARAMS_KEEP, .MOSAIC_SAMPLES_META_COLS))


# Parameters that are always stored per location (with a location suffix even for one location):
# the location parameters of convert_config_to_matrix() and the location-scale parameters of
# estimated_parameters. Used to find the locations in a set of names that comes without them.
.mosaic_location_param_bases <- function() {
     est <- get("estimated_parameters", envir = asNamespace("MOSAIC"))
     unique(c(.MOSAIC_CONFIG_LOCATION_PARAMS, est$parameter_name[est$scale == "location"]))
}


# The locations in a set of parameter names: the suffixes after "<base>_" for the parameters that
# are always per location (longest base first), excluding plain indices ("_1": a vector that is not
# per location).
.mosaic_infer_locations <- function(param_names) {
     param_names <- as.character(param_names)
     bases <- .mosaic_location_param_bases()
     bases <- bases[order(-nchar(bases))]
     open <- !(param_names %in% .mosaic_unsplit_names())
     locs <- character(0)
     for (b in bases) {
          pre <- paste0(b, "_")
          hit <- open & startsWith(param_names, pre)
          if (!any(hit)) next
          rest <- substring(param_names[hit], nchar(pre) + 1L)
          locs <- c(locs, rest[nzchar(rest) & !grepl("^[0-9]+$", rest)])
          open[hit] <- FALSE
     }
     unique(locs)
}


# Split parameter names into base parameter and location: "<base>_<location>" for the longest
# location it ends with (so "beta_j0_tot_MOZ_Cabo_Delgado" is beta_j0_tot at "MOZ_Cabo_Delgado"),
# the name itself and NA otherwise. Config parameters and samples bookkeeping columns are never
# split. `location_names` should be the config's; without it the locations are inferred from the
# names (.mosaic_infer_locations()).
#
# Returns a data frame with one row per name: parameter, base and location.
.mosaic_split_param_names <- function(param_names, location_names = NULL) {
     param_names <- as.character(param_names)
     if (is.null(location_names)) location_names <- .mosaic_infer_locations(param_names)
     locs <- unique(as.character(location_names))
     locs <- locs[order(-nchar(locs))]
     base <- param_names
     location <- rep(NA_character_, length(param_names))
     open <- !(param_names %in% .mosaic_unsplit_names())
     len <- nchar(param_names)
     for (L in locs) {
          suf <- paste0("_", L)
          hit <- open & len > nchar(suf) & endsWith(param_names, suf)
          if (!any(hit)) next
          base[hit] <- substr(param_names[hit], 1L, len[hit] - nchar(suf))
          location[hit] <- L
          open[hit] <- FALSE
     }
     data.frame(parameter = param_names, base = base, location = location, stringsAsFactors = FALSE)
}


# Base parameter of each name (the name itself when it has no location).
.mosaic_param_base <- function(param_names, location_names = NULL) {
     .mosaic_split_param_names(param_names, location_names)$base
}


# Strip a known location from parameter names: "<base>_<location>" -> "<base>", other names
# unchanged. For code that already knows the location (no pattern matching, so "." or other
# characters in a location name are taken literally).
.mosaic_strip_location <- function(param_names, location) {
     param_names <- as.character(param_names)
     location <- rep_len(as.character(location), length(param_names))
     suf <- paste0("_", location)
     hit <- !is.na(location) & nchar(param_names) > nchar(suf) & endsWith(param_names, suf)
     param_names[hit] <- substr(param_names[hit], 1L, nchar(param_names[hit]) - nchar(suf[hit]))
     param_names
}


# "beta_j0_tot (MOZ_Cabo_Delgado)" for a location-specific name, the name itself otherwise; for
# axis labels.
.mosaic_param_label <- function(param_names, location_names = NULL) {
     sp <- .mosaic_split_param_names(param_names, location_names)
     ifelse(is.na(sp$location), sp$parameter, paste0(sp$base, " (", sp$location, ")"))
}


# Location names a calibration can use. Each name must start with a letter (any script) and hold
# only letters, digits, "_", "." and "-", so it is safe in parameter names, file names, JSON keys
# and data-frame columns; names must be unique, also ignoring case (per-location figure files would
# overwrite each other on a case-insensitive file system). They need not be ISO codes. Finally the
# set must split unambiguously: every "<parameter>_<location>" must split back to that parameter and
# location, with the names given and with them inferred. A pair like "X" and "tot_X" fails that
# ("beta_j0_tot_X" would read as beta_j0 at "tot_X"). Config parameters and samples bookkeeping
# columns are never split (.mosaic_unsplit_names()), so a location such as "best" cannot turn
# "weight_best" into a parameter. Stops naming the problem.
.mosaic_check_location_names <- function(location_names) {
     if (!is.character(location_names) || !length(location_names)) {
          stop("config$location_name must be a non-empty character vector.", call. = FALSE)
     }
     bad <- is.na(location_names) | !grepl("^\\p{L}[\\p{L}\\p{N}_.-]*$", location_names, perl = TRUE)
     if (any(bad)) {
          stop("config$location_name holds names a calibration cannot use: ",
               paste0("'", location_names[bad], "'", collapse = ", "),
               ". Use names that start with a letter and contain only letters, digits, '_', '.' or '-'.",
               call. = FALSE)
     }
     dup <- unique(location_names[duplicated(tolower(location_names))])
     if (length(dup)) {
          stop("config$location_name has duplicates (ignoring case): ", paste(dup, collapse = ", "),
               call. = FALSE)
     }
     clash <- intersect(location_names, .mosaic_unsplit_names())
     if (length(clash)) {
          stop("config$location_name uses parameter or samples column names as locations: ",
               paste(clash, collapse = ", "), call. = FALSE)
     }
     bases <- .mosaic_location_param_bases()
     probe <- paste0(rep(bases, each = length(location_names)), "_", location_names)
     want_base <- rep(bases, each = length(location_names))
     want_loc <- rep(location_names, times = length(bases))
     for (given in list(location_names, NULL)) {
          sp <- .mosaic_split_param_names(probe, given)
          wrong <- which(is.na(sp$location) | sp$base != want_base | sp$location != want_loc)
          if (length(wrong)) {
               stop("config$location_name holds names that make parameter names ambiguous: ",
                    sprintf("'%s' would read as %s at '%s'", probe[wrong[1]], sp$base[wrong[1]],
                            sp$location[wrong[1]]),
                    ". Rename the location(s) so that no name is the tail of a parameter name plus another.",
                    call. = FALSE)
          }
     }
     invisible(location_names)
}
