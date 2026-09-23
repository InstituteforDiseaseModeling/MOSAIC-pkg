#' Per-country overland departure rates from the E3 border-throughput evidence
#'
#' Weekly outbound departure probability per person, assembled in the
#' MOSAIC-OCV E3 investigation from border-throughput counts (Goma ~50k/day,
#' Busia ~23k/day, Ressano Garcia ~7.1k/day), IOM DTM flow-monitoring, and the
#' StatsSA P0351 overland share. Values are the four regional
#' \code{params_<region>.json} margins.
#'
#' \strong{These are evidence-anchored approximations, not measurements.} Every
#' underlying count is a floor (recorded crossings only, movements not persons),
#' so the band is deliberately wide and is carried into the prior as such --
#' see \code{\link{est_overland_tau_prior}}.
#'
#' Where two regions disagreed the \strong{larger} value is kept (UGA: central
#' 0.003 vs eastern 0.002 -> 0.003), on the grounds that each regional estimate
#' counted only that region's corridors and so understates total outbound flow.
#'
#' \strong{Ethiopia is revised up from E3's 3e-4 to 1e-3/week.} E3's value
#' implies ~5,400 outbound crossings/day for 127M people -- fewer than Rwanda
#' (13.8M) and fewer than a single Mozambican border post -- and the Horn had
#' the thinnest border-throughput evidence of the four regions. E3's own
#' border-district decomposition (\code{tau_national ~ f_border x tau_border},
#' calibrated off MOZ) gives ~6.7e-4. 1e-3 keeps Ethiopia comfortably the
#' lowest evidenced value, preserving the ordering, without asserting
#' something arithmetically implausible. \strong{Any conclusion about
#' Ethiopian cross-border spillover must be re-run across this band}: the
#' earlier \dQuote{no resolvable spillover} finding was an arithmetic
#' consequence of the least-evidenced number in the panel.
#'
#' @format Named numeric vector, weekly departure probability per person.
#' @source MOSAIC-OCV \code{output/E3_mobility_route2b_*/params_*.json};
#'   method in \code{notes/E3-mobility-west-refit.md} section 3.
#' @export
MOSAIC_E3_TAU_WEEKLY <- c(
     # west
     BEN = 0.0030, BFA = 0.0020, CIV = 0.0020, CMR = 0.0025, GHA = 0.0020,
     LBR = 0.0010, NER = 0.0030, NGA = 0.0040, TCD = 0.0020, TGO = 0.0030,
     # central
     AGO = 0.0015, BDI = 0.0030, COD = 0.0050, COG = 0.0020, KEN = 0.0020,
     RWA = 0.0030, SSD = 0.0025, TZA = 0.0020, UGA = 0.0030, ZMB = 0.0025,
     # southern
     MOZ = 0.0020, MWI = 0.0025, ZAF = 0.0030, ZWE = 0.0030,
     # eastern
     ETH = 0.0010, SOM = 0.0020
)


#' Build a per-country overland departure-rate prior for MOSAIC calibration
#'
#' Turns the E3 evidence band into a per-country Beta prior on the DAILY
#' departure probability \code{tau_i}, suitable for use as a calibration prior
#' in place of the air-derived one.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}.
#' @param iso_codes ISO3 codes. Defaults to \code{MOSAIC::iso_codes_mosaic}.
#' @param in_set_adjust Scale each country's tau by the share of its outbound
#'   migrant stock whose destination is inside the patch set. \code{TRUE} by
#'   default. Without it a country whose largest corridors leave the patch set
#'   (Ethiopia -> Djibouti/Sudan, South Sudan -> Sudan, South Africa ->
#'   Lesotho) has that flow silently re-routed onto its in-set neighbours by
#'   the row normalisation of \code{pi_ij}.
#' @param write Write \code{param_tau_departure_overland.csv}? \code{TRUE} by
#'   default. Internal callers passing a narrowed \code{iso_codes} must pass
#'   \code{FALSE}, or they truncate the model-input file to that subset.
#' @param tau_weekly Named weekly departure probabilities. Defaults to
#'   \code{\link{MOSAIC_E3_TAU_WEEKLY}}.
#' @param tau_weekly_default Weekly value for countries the E3 work never
#'   covered. Default \code{NULL} = the median of \code{tau_weekly}.
#' @param span Target 95\% interval width, as a MULTIPLICATIVE factor, for
#'   countries with direct E3 evidence. Default \code{10} -- one order of
#'   magnitude, which is what the underlying border-throughput evidence
#'   actually supports (every count is a floor).
#' @param span_default Target 95\% span for countries with no country-specific
#'   evidence. Default \code{30}.
#' @param family Prior family. \code{"lognormal"} (default) or \code{"beta"}.
#'   Lognormal is strongly preferred: a Beta wide enough to express an
#'   order-of-magnitude floor is J-shaped, putting the prior mode at zero
#'   departure rate.
#' @param verbose Print a summary.
#'
#' @return Invisibly, a \code{data.frame}: \code{iso_code},
#'   \code{tau_weekly}, \code{tau_daily}, \code{sd}, \code{shape1},
#'   \code{shape2}, \code{ci_lo}, \code{ci_hi}, \code{evidence}
#'   (\code{"E3"} or \code{"default"}). Written to
#'   \code{model/input/param_tau_departure_overland.csv}.
#'
#' @section Choosing the width:
#' The E3 band spans roughly 1e-3 to 5e-3 per week, a factor of 5. Treating
#' that as a 95% interval on a log scale gives
#' \code{sdlog = log(5) / (2 * 1.96) = 0.41}, hence \code{CV ~ 0.43}, rounded
#' to \strong{0.45}; unevidenced countries get \strong{0.75}.
#'
#' \strong{Realised intervals, measured, not derived:} CV 0.45 gives a 95%
#' span of \strong{6.4x} and CV 0.75 gives \strong{29.3x} -- not the "factor
#' of 10" an earlier version of this note claimed. The lognormal-to-Beta
#' transfer moves the \emph{moment}, not the \emph{interval}, and the two
#' diverge above CV ~ 0.6. A true factor-10 interval needs CV = 0.55.
#'
#' \strong{This prior is TIGHTER than the one it replaces.} The production air
#' prior in \code{priors_default} is not the raw fit in
#' \code{param_tau_departure.csv}: \code{data-raw/make_priors_default.R}
#' applies \code{tau_uncertainty_factor = 0.001}, giving a median CV of
#' \strong{1.39} with 31 of 40 countries having \code{shape1 < 1} (mode at
#' zero). So adopting this prior is a ~12x re-centring combined with a ~3x
#' tightening, on a quantity whose sources are explicitly floors. Since
#' \code{tau_i} is weakly identified and MOSAIC uses the prior as its
#' proposal, the centre largely determines the posterior -- widening to
#' CV >= 1 would require switching to a lognormal, because a Beta with
#' CV >= 1 is J-shaped (mode at zero).
#'
#' @section Relationship to the air prior:
#' Air-derived \code{tau_i} has median ~3e-5/day; these overland values are
#' ~4e-5 to 7e-4/day, median 3.6e-4 -- a \strong{~12x lift}.
#'
#' The supporting anchor is StatsSA Tourism 2024 (Report 03-51-02):
#' \strong{68-91% of South Africa's ~8.9M annual cross-border arrivals are
#' overland}, so an air-only fit captures ~9-32% of flow, i.e. road:air
#' \strong{~2x to ~10x} for a formal, highly air-connected economy. Informal
#' crossings, which StatsSA cannot see, push this higher for porous or
#' landlocked countries. (Earlier notes in this repo cite "~1:90 air:road" and
#' a "~100x undercount": those restate the 91% overland SHARE as a ratio and
#' are not supported by the source. A related 7x inflation appears where E3
#' compared a weekly overland rate against MOSAIC's daily air rate.)
#'
#' These REPLACE the air prior for an overland-aware calibration. Note the
#' conceptually cleaner object is additive, \code{tau_air + tau_overland},
#' since the air residual carries the long-range, non-adjacent links that are
#' the only route by which a distant country can be seeded.
#'
#' @seealso \code{\link{rake_mobility_od_to_tau}}, \code{\link{est_mobility}}
#' @importFrom utils write.csv
#' @importFrom glue glue
#' @export
est_overland_tau_prior <- function(PATHS,
                                   iso_codes          = NULL,
                                   write              = TRUE,
                                   in_set_adjust      = TRUE,
                                   tau_weekly         = MOSAIC_E3_TAU_WEEKLY,
                                   tau_weekly_default = NULL,
                                   span               = 10,
                                   span_default       = 30,
                                   family             = c("lognormal", "beta"),
                                   verbose            = TRUE) {

     family <- match.arg(family)
     if (is.null(iso_codes)) iso_codes <- iso_codes_mosaic
     iso_codes <- sort(unique(toupper(iso_codes)))
     if (is.null(tau_weekly_default)) tau_weekly_default <- stats::median(tau_weekly)

     have <- iso_codes %in% names(tau_weekly)
     tw   <- ifelse(have, tau_weekly[iso_codes], tau_weekly_default)
     spn  <- ifelse(have, span, span_default)

     # In-set fraction. tau counts ALL outbound crossings, but pi_ij
     # row-normalises over MOSAIC patches only, so flow to an out-of-set
     # neighbour is mis-routed onto in-set ones. Binding for ETH (Djibouti,
     # Sudan), SSD (Sudan), ZAF (Lesotho), TCD/NER/ERI/MRT. Derived from the
     # DESA bilateral stock -- the share of each origin's total outbound stock
     # whose destination is in the patch set -- not asserted.
     f_in <- rep(1, length(iso_codes))
     if (isTRUE(in_set_adjust)) {
          fi <- .tau_in_set_fraction(PATHS, iso_codes)
          if (!is.null(fi)) {
               f_in <- fi
               if (verbose) {
                    message(glue::glue(
                         "  in-set fraction: median {round(stats::median(f_in), 2)}, ",
                         "lowest {paste(utils::head(iso_codes[order(f_in)], 3), collapse = '/')}"))
               }
          } else if (verbose) {
               message("  in-set fraction: DESA unavailable; tau left unadjusted")
          }
     }

     tw   <- tw * f_in
     td   <- tw / 7                                   # weekly -> DAILY (MOSAIC convention)
     # LOGNORMAL, not Beta. A Beta wide enough to express an order-of-magnitude
     # floor is J-shaped: shape1 = (1-mu)/CV^2 - mu, so CV >= 1 puts the mode
     # at ZERO departure. Lognormal keeps the mode at the anchor, is the
     # natural family for a small positive rate, and lets the 95% interval be
     # specified directly as a span rather than backed out of a moment.
     sdlog <- log(spn) / (2 * stats::qnorm(0.975))
     meanlog <- log(td)
     ci_lo <- stats::qlnorm(0.025, meanlog, sdlog)
     ci_hi <- stats::qlnorm(0.975, meanlog, sdlog)
     sdv   <- td * sqrt(exp(sdlog^2) - 1)

     if (identical(family, "beta")) {
          bp <- .beta_from_mean_sd(td, sdv)
          shape1 <- bp$shape1; shape2 <- bp$shape2
          ci_lo <- stats::qbeta(0.025, shape1, shape2)
          ci_hi <- stats::qbeta(0.975, shape1, shape2)
     } else {
          shape1 <- NA_real_; shape2 <- NA_real_
     }

     out <- data.frame(
          iso_code   = iso_codes,
          tau_weekly = tw,
          tau_daily  = td,
          distribution = family,
          meanlog    = if (identical(family, "lognormal")) meanlog else NA_real_,
          sdlog      = if (identical(family, "lognormal")) sdlog else NA_real_,
          sd         = sdv,
          shape1     = shape1,
          shape2     = shape2,
          ci_lo      = ci_lo,
          ci_hi      = ci_hi,
          in_set_frac = round(f_in, 3),
          evidence   = ifelse(have, "E3", "default"),
          stringsAsFactors = FALSE, row.names = NULL
     )

     f <- file.path(PATHS$MODEL_INPUT, "param_tau_departure_overland.csv")
     # write = FALSE for internal callers: rake_mobility_od_to_tau() narrows
     # iso_codes first, so an unconditional write would silently TRUNCATE the
     # model-input file to whatever subset was being raked.
     if (write) utils::write.csv(out, f, row.names = FALSE)

     if (verbose) {
          message(glue::glue(
               "Overland tau prior: {sum(have)} countries from E3 evidence, ",
               "{sum(!have)} at the default ({signif(tau_weekly_default, 3)}/wk)"))
          message(glue::glue(
               "  daily tau: median {signif(stats::median(td), 3)}, ",
               "range {signif(min(td), 3)}-{signif(max(td), 3)}"))
          message("  -> ", f)
     }
     invisible(out)
}


#' Beta shape parameters from a mean and sd, clamped to a valid region
#' @keywords internal
#' @noRd
.beta_from_mean_sd <- function(mu, sd) {
     v  <- sd^2
     cv <- sd / mu
     # The real cliff is shape1 = (1-mu)/CV^2, so the prior turns J-shaped
     # (mode at 0, unbounded density) at CV >= 1 -- INDEPENDENT of mu. The old
     # variance clamp only fired at CV > 52.9 for these means: dead code
     # sitting exactly where a guard appeared to be. Warn at the cliff that
     # actually exists.
     if (any(cv >= 1, na.rm = TRUE)) {
          warning("CV >= 1 requested for ", sum(cv >= 1, na.rm = TRUE),
                  " value(s): the Beta becomes J-shaped (mode at zero). ",
                  "Use a lognormal if you need this much spread.", call. = FALSE)
     }
     vmax <- mu * (1 - mu) * 0.999
     if (any(v > vmax, na.rm = TRUE)) {
          warning("Requested variance exceeds the Beta bound mu(1-mu); clamped.",
                  call. = FALSE)
     }
     v <- pmin(v, vmax)
     k <- mu * (1 - mu) / v - 1
     list(shape1 = mu * k, shape2 = (1 - mu) * k)
}


#' Rake the fused OD structure to per-country outbound departure margins
#'
#' Iterative proportional fitting (\code{mipfp::Ipfp}) of the unit-free fused
#' structure onto real daily person-flow margins, giving an OD matrix that
#' carries both the fused structure AND a defensible amplitude.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}.
#' @param iso_codes ISO3 codes. Defaults to \code{MOSAIC::iso_codes_mosaic}.
#' @param tau_daily Named daily departure probabilities. \code{NULL} (default)
#'   takes them from \code{\link{est_overland_tau_prior}}.
#' @param N Named population vector. \code{NULL} (default) reads the
#'   demographics used elsewhere in \code{\link{est_mobility}}.
#' @param pop_year Population vintage for the row margin. Default
#'   \code{2017}, which is what \code{\link{est_mobility}} divides by
#'   (\dQuote{to match OAG data}). \strong{These must agree}: raking against
#'   a different year makes the recovered \code{tau_i} equal the target times
#'   \code{N(pop_year)/N(2017)}.
#' @param verbose Print progress.
#'
#' @return Invisibly, the raked OD matrix (daily person-flows, origin x
#'   destination). Written to
#'   \code{processed/mobility/M_fused_raked.csv}.
#'
#' @section Row margins only:
#' Raking is to the OUTBOUND (row) margin \code{tau_daily * N} only. E3 tested
#' raking to both a row and a population column margin and rejected it: on a
#' 10x10 it over-constrains and distorts the row shape (Nigeria's structure
#' flattened). Row-only raking hits the tau target to machine precision and
#' leaves the fused row structure untouched.
#'
#' @seealso \code{\link{est_overland_tau_prior}}, \code{\link{est_mobility}}
#' @importFrom utils read.csv write.csv
#' @importFrom glue glue
#' @export
rake_mobility_od_to_tau <- function(PATHS,
                                    iso_codes = NULL,
                                    tau_daily = NULL,
                                    N         = NULL,
                                    pop_year  = 2017L,
                                    verbose   = TRUE) {

     if (!requireNamespace("mipfp", quietly = TRUE)) {
          stop("Package 'mipfp' is required for the IPF rake.\n",
               "  install.packages('mipfp')", call. = FALSE)
     }
     if (is.null(iso_codes)) {
          iso_codes <- get("iso_codes_mosaic", envir = asNamespace("MOSAIC"))
     }
     iso_codes <- sort(unique(toupper(iso_codes)))

     f_struct <- file.path(PATHS$DATA_PROCESSED, "mobility", "M_structure_fused.csv")
     if (!file.exists(f_struct)) {
          stop("Fused structure not found: ", f_struct,
               "\n  Build it with process_mobility_od_data(PATHS).", call. = FALSE)
     }
     M <- as.matrix(utils::read.csv(f_struct, row.names = 1, check.names = FALSE))
     colnames(M) <- gsub("^X", "", colnames(M))
     iso_codes <- intersect(iso_codes, rownames(M))
     M <- M[iso_codes, iso_codes, drop = FALSE]

     if (is.null(tau_daily)) {
          tp <- est_overland_tau_prior(PATHS, iso_codes = iso_codes,
                                       write = FALSE, verbose = FALSE)
          tau_daily <- stats::setNames(tp$tau_daily, tp$iso_code)
     }
     if (is.null(N)) {
          d <- utils::read.csv(file.path(PATHS$DATA_DEMOGRAPHICS,
                                         "demographics_africa_2000_2023.csv"),
                               stringsAsFactors = FALSE)
          # MUST match the vintage est_mobility() divides by. It reads 2017
          # ("to match OAG data"); raking against a later year silently
          # inflates the recovered tau by N(year)/N(2017) -- measured 5.6% to
          # 23.0% across the 40, worst for the fastest-growing countries.
          if (!any(d$year == pop_year)) {
               stop("pop_year ", pop_year, " not present in demographics.", call. = FALSE)
          }
          d <- d[d$year == pop_year & d$iso_code %in% iso_codes, ]
          N <- stats::setNames(as.numeric(d$population), d$iso_code)
     }
     N         <- N[iso_codes]
     tau_daily <- tau_daily[iso_codes]
     if (anyNA(N) || anyNA(tau_daily)) {
          stop("Missing population or tau for: ",
               paste(iso_codes[is.na(N) | is.na(tau_daily)], collapse = ", "), call. = FALSE)
     }

     target_row <- as.numeric(tau_daily) * as.numeric(N)   # daily departures
     seed <- M
     seed[seed <= 0] <- 1e-12                              # Ipfp needs a positive seed

     if (verbose) {
          message(glue::glue("IPF rake: {length(iso_codes)} countries, ",
                             "target daily departures {round(sum(target_row))} total"))
     }
     fit <- mipfp::Ipfp(seed = seed, target.list = list(1), target.data = list(target_row),
                        print = FALSE)
     R <- fit$x.hat
     dimnames(R) <- list(iso_codes, iso_codes)

     err <- max(abs(rowSums(R) - target_row) / pmax(target_row, 1e-12))
     if (verbose) message(glue::glue("  max relative row-margin error: {signif(err, 3)}"))
     if (err > 1e-6) {
          warning("IPF did not converge to the tau margins (max rel err ", signif(err, 3), ").",
                  call. = FALSE)
     }
     # structure must be untouched by a row-only rake
     s0 <- .od_row_normalise(M); s1 <- .od_row_normalise(R)
     dstr <- max(abs(s0 - s1), na.rm = TRUE)
     if (verbose) message(glue::glue("  max change in row structure: {signif(dstr, 3)} (want ~0)"))

     f_out <- file.path(PATHS$DATA_PROCESSED, "mobility", "M_fused_raked.csv")
     utils::write.csv(as.data.frame(R), f_out)
     if (verbose) message("  -> ", f_out)
     invisible(R)
}


#' Share of an origin's OVERLAND-plausible flow that lands inside the patch set
#'
#' Denominator is flow to \strong{land-contiguous} countries only, not total
#' outbound stock. Using the total is wrong for an overland tau: it counts
#' long-haul labour migration that never crosses a land border. Ethiopia is
#' the clearest case -- its outbound stock is dominated by Gulf migration, so
#' a total-stock denominator returned an in-set fraction of 0.15 and cut its
#' tau below the value it was raised from, for a reason that has nothing to do
#' with border crossing.
#'
#' Contiguity is computed over every country shapefile on disk (~55), so
#' out-of-set land neighbours -- Djibouti and Sudan for Ethiopia, Sudan for
#' South Sudan, Lesotho for South Africa -- are correctly in the denominator
#' and absent from the numerator.
#'
#' @keywords internal
#' @noRd
.tau_in_set_fraction <- function(PATHS, iso_codes) {

     snap <- .mobility_od_newest_snapshot(PATHS)
     if (is.na(snap)) return(NULL)
     f <- list.files(snap, pattern = "undesa.*\\.xlsx$", full.names = TRUE)
     if (!length(f)) return(NULL)

     # --- land neighbours over ALL available shapefiles, not just the set ---
     shp <- list.files(PATHS$DATA_SHAPEFILES, pattern = "_ADM0\\.shp$", full.names = TRUE)
     all_iso <- sub("_ADM0\\.shp$", "", basename(shp))
     if (length(shp) < 2L) return(NULL)
     nb <- tryCatch({
          # st_union per file: several ADM0 files are multi-feature, so
          # concatenating raw geometries gives more rows than countries and
          # the distance matrix silently misaligns (observed: 108 geometries
          # for 55 countries -> the whole adjustment became a no-op).
          g <- do.call(c, lapply(shp, function(x)
               sf::st_union(suppressMessages(sf::st_geometry(sf::st_read(x, quiet = TRUE))))))
          stopifnot(length(g) == length(all_iso))
          dm <- matrix(as.numeric(sf::st_distance(g)), nrow = length(all_iso),
                       dimnames = list(all_iso, all_iso))
          dm < 10000
     }, error = function(e) NULL)
     if (is.null(nb)) return(NULL)
     diag(nb) <- FALSE

     d <- tryCatch(suppressWarnings(
               readxl::read_excel(f[1], sheet = "Table 1", skip = 10)),
          error = function(e) NULL)
     if (is.null(d)) return(NULL)
     nm <- names(d)
     cd <- grep("^Location code of destination$", nm)
     co <- grep("^Location code of origin$", nm)
     cy <- grep("^2024", nm)[1]
     if (!length(cd) || !length(co) || is.na(cy)) return(NULL)

     o <- countrycode::countrycode(suppressWarnings(as.integer(unlist(d[[co]]))),
                                   "un", "iso3c", warn = FALSE)
     e <- countrycode::countrycode(suppressWarnings(as.integer(unlist(d[[cd]]))),
                                   "un", "iso3c", warn = FALSE)
     v <- suppressWarnings(as.numeric(unlist(d[[cy]])))

     base <- !is.na(o) & !is.na(e) & o != e & !is.na(v) & v > 0 &
             o %in% iso_codes & o %in% all_iso & e %in% all_iso
     if (!any(base)) return(NULL)
     # keep only origin-destination pairs that share a land border.
     # Index on the FILTERED subset: mapply over the full vectors hits pairs
     # whose codes are absent from the shapefile set and errors out.
     adj <- base
     adj[base] <- nb[cbind(o[base], e[base])]
     adj[is.na(adj)] <- FALSE
     if (!any(adj)) return(NULL)

     tot <- tapply(v[adj], o[adj], sum)
     sel <- adj & e %in% iso_codes
     ins <- tapply(v[sel], o[sel], sum)

     fr <- rep(NA_real_, length(iso_codes)); names(fr) <- iso_codes
     fr[names(tot)] <- 0
     fr[names(ins)] <- as.numeric(ins) / as.numeric(tot[names(ins)])
     # no contiguous DESA record -> assume fully in-set rather than zeroing
     fr[is.na(fr)] <- 1
     pmin(pmax(fr, 0.05), 1)
}
