#' Pre-fit and freeze the environmental-suitability (psi) cache for rolling CV
#'
#' Fits the environmental-suitability (psi) LSTM once per rolling-origin cutoff
#' \code{T} (with \code{fit_date_stop = T}) and freezes each fit's daily psi
#' prediction into a per-cutoff CSV under \code{dir_cache}. The frozen cache is
#' consumed by \code{\link{run_rolling_cv}} via its \code{psi_cache} argument, so
#' the (non-deterministic, expensive) psi fit is run exactly once per cutoff and
#' the calibration backtest reuses the identical frozen psi across reruns.
#'
#' @details
#' \strong{Leakage discipline.} Each cutoff's fit uses \code{fit_date_stop = T}
#' (no observation after \code{T} informs the psi representation); the prediction
#' window (\code{pred_date_start}/\code{pred_date_stop}) is harness-owned and held
#' fixed across cutoffs so every frozen CSV spans the same grid.
#'
#' \strong{Leak-free v7.4 hazard panel (opt-in).} When the RAW
#' \code{est_suitability_spec} requests \code{feature_set = "v7.4"}, the four
#' imputed hazard channels (flood/cyclone/drought probabilities) would LEAK if
#' their GAMs were fit once on the full series: a fold's covariates would depend
#' on outcomes in that fold's OOS future. To prevent this, for each cutoff
#' \code{T} this function rebuilds a \emph{leak-free} panel via
#' \code{\link{compile_suitability_data}(gam_train_stop = T, include_flood_prob
#' = TRUE)} -- every hazard GAM is fit only on rows with \code{date <= T} and
#' then predicts every row -- writes it to \code{dir_cache/panel_v74_<T>.csv},
#' and fits psi via \code{est_suitability(source_csv = <panel>, feature_set =
#' "v7.4", fit_date_stop = T)}. The panel is compiled ONCE per cutoff and reused
#' across the whole \eqn{\ge}10-seed psi ensemble (the 3 GAMs are not refit per
#' seed). When v7.4 is not requested the DEFAULT path is byte-unchanged: psi is
#' fit from the canonical panel with no \code{source_csv} and no
#' \code{gam_train_stop} (v1/v7.3 behaviour).
#'
#' \strong{Panel-window alignment (v7.4).} The leak-free panel's compile/GAM-fit
#' window is deliberately aligned to the psi-LSTM \emph{training} window
#' \eqn{[\code{fit_date_start}, T]}, NOT an arbitrary-wide (e.g. 2000->2027)
#' span. Imputed hazard-probability magnitudes dilute when the compile window is
#' wider than the LSTM fit window (more all-zero rows shrink the same event
#' labels), so the hazard probabilities the LSTM ingests must be computed over
#' the same rows the LSTM trains on. The compile \code{date_start} is therefore
#' resolved from the spec's LSTM \code{fit_date_start}
#' (\code{est_suitability_spec$arch_control$fit_date_start}, default
#' \code{"2015-01-01"}) and \code{date_stop} from the cutoff \code{T}. This
#' resolved window is folded into the cache spec-hash (below) so a panel built
#' over a different window cannot be silently reused.
#'
#' \strong{Atomic cache (concurrency-safe).} \code{est_suitability()} writes a
#' single global \code{MODEL_INPUT/pred_psi_suitability_day.csv}; this function
#' copies that file into \code{dir_cache/psi_<T>.csv} via a tempfile +
#' \code{file.rename} so a partially-written cache file can never be observed.
#'
#' \strong{Cache key (spec hash).} For each cutoff a \code{spec_hash} is computed
#' over \code{list(fit_date_stop = T, est_suitability_spec)} where the resolved
#' spec includes \code{arch_control$n_seeds}. The number of pooled seeds is part
#' of the key: refitting with a different seed count invalidates the cache. When
#' the spec requests \code{feature_set = "v7.4"}, a \code{v74_panel} block
#' (\code{leakfree = TRUE}, resolved \code{compile_date_start}, and
#' \code{gam_train_stop = T}) is also folded into the hash, so a frozen psi
#' built from a v7.3 panel -- or from a v7.4 panel compiled over a different
#' window -- cannot be silently reused. \code{run_rolling_cv(psi_cache=)}
#' recomputes the same hash from its own \code{est_suitability_spec} and
#' \strong{hard-errors} on a mismatch.
#'
#' \strong{Idempotent / resume.} A cutoff is skipped when its \code{psi_<T>.csv}
#' exists and the manifest already records a matching \code{spec_hash} for that
#' cutoff; rerunning therefore only fits the missing/changed cutoffs.
#'
#' @param PATHS Path list from \code{\link{get_paths}}.
#' @param cutoffs Date or character vector of rolling-origin cutoffs.
#' @param est_suitability_spec Named list of \emph{modeling} arguments forwarded
#'   to \code{\link{est_suitability}} (e.g. \code{architecture}, \code{feature_set},
#'   \code{response_var}, \code{bias_correct}, and the lstm_v2 \code{arch_control}
#'   list, which carries \code{n_seeds}/\code{parallel_seeds}). Date keys are
#'   ignored (harness-owned) with a warning.
#' @param pred_date_start,pred_date_stop Prediction window (Date/character) used
#'   for every cutoff fit. Held fixed so all frozen CSVs share one date grid.
#' @param dir_cache Directory to hold the frozen \code{psi_<T>.csv} files and the
#'   \code{psi_manifest.json} (created if needed).
#' @param verbose Logical (default TRUE).
#'
#' @return Invisibly, the manifest list (also written to
#'   \code{dir_cache/psi_manifest.json}). Side effects: one \code{psi_<T>.csv}
#'   per fitted cutoff plus the manifest under \code{dir_cache}.
#'
#' @seealso \code{\link{run_rolling_cv}}, \code{\link{est_suitability}}
#' @export
prefit_rolling_cv_psi <- function(PATHS,
                                  cutoffs,
                                  est_suitability_spec = list(),
                                  pred_date_start,
                                  pred_date_stop,
                                  dir_cache,
                                  verbose = TRUE) {

     if (missing(PATHS) || is.null(PATHS$MODEL_INPUT))
          stop("PATHS (with $MODEL_INPUT) is required.")
     if (missing(dir_cache) || is.null(dir_cache) || !nzchar(dir_cache))
          stop("dir_cache is required.")
     if (missing(pred_date_start) || missing(pred_date_stop))
          stop("pred_date_start and pred_date_stop are required.")
     cutoffs <- sort(unique(as.Date(cutoffs)))
     if (length(cutoffs) == 0L) stop("No cutoffs supplied.")

     # Strip harness-owned date keys from the user spec (same contract as
     # run_rolling_cv); what remains is the modeling spec that the hash keys on.
     spec <- .rcv_strip_date_keys(est_suitability_spec)

     # Leak-free v7.4 hazard-panel mode. Guard on the RAW requested spec (NOT a
     # default-merged value) per CLAUDE.md lesson #13: v7.4 is active only when
     # the caller explicitly asked for feature_set == "v7.4". Everything else --
     # including the default (no feature_set) and v1/v7.3 -- keeps the canonical
     # panel + no gam_train_stop (byte-unchanged legacy path).
     v74 <- .rcv_psi_v74_request(spec)
     if (v74$active && verbose)
          message(sprintf(
               "prefit_rolling_cv_psi: v7.4 leak-free hazard panels ON (compile window %s -> <cutoff>, gam_train_stop = cutoff)",
               v74$compile_date_start))

     pred_start <- as.Date(pred_date_start)
     pred_stop  <- as.Date(pred_date_stop)

     dir.create(dir_cache, recursive = TRUE, showWarnings = FALSE)
     manifest_path <- file.path(dir_cache, "psi_manifest.json")

     # Resume: read any prior manifest so we can skip already-frozen cutoffs whose
     # spec_hash matches what we would compute now.
     prior <- .rcv_psi_read_manifest(manifest_path)

     mosaic_ver <- as.character(utils::packageVersion("MOSAIC"))
     laser_ver  <- .rcv_laser_version()
     n_seeds    <- .rcv_spec_n_seeds(spec)
     par_seeds  <- .rcv_spec_parallel_seeds(spec)

     entries <- if (length(prior$cutoffs)) prior$cutoffs else list()
     # Index prior entries by cutoff string for quick lookup.
     prior_by_cut <- stats::setNames(
          lapply(entries, identity),
          vapply(entries, function(e) as.character(e$cutoff), character(1)))

     for (k in seq_along(cutoffs)) {
          T_k     <- cutoffs[k]
          T_chr   <- as.character(T_k)
          csv_dst <- file.path(dir_cache, sprintf("psi_%s.csv", T_chr))
          spec_hash <- .rcv_psi_spec_hash(T_k, spec)

          prev <- prior_by_cut[[T_chr]]
          if (!is.null(prev) && file.exists(csv_dst) &&
              identical(prev$spec_hash, spec_hash)) {
               if (verbose)
                    message(sprintf("[%d/%d] %s  cache hit (spec_hash match) -> skip fit",
                                    k, length(cutoffs), T_chr))
               # keep the (validated) prior entry as-is
               prior_by_cut[[T_chr]] <- prev
               next
          }

          if (verbose)
               message(sprintf("[%d/%d] %s  fitting psi (fit_date_stop=%s)%s",
                               k, length(cutoffs), T_chr, T_chr,
                               if (v74$active) "  [v7.4 leak-free panel]" else ""))

          owned <- list(
               PATHS           = PATHS,
               fit_date_stop   = T_k,
               pred_date_start = pred_start,
               pred_date_stop  = pred_stop)

          # v7.4: compile the leak-free hazard panel ONCE for this cutoff
          # (3 GAMs fit on date <= T, predict all rows) over the LSTM training
          # window, then point est_suitability at it. Reused across every seed
          # of the psi ensemble. Non-v7.4 leaves owned untouched -> canonical
          # panel, no source_csv (byte-unchanged legacy path).
          if (v74$active) {
               panel_csv <- file.path(dir_cache, sprintf("panel_v74_%s.csv", T_chr))
               .rcv_build_leakfree_panel_v74(
                    PATHS          = PATHS,
                    cutoff         = T_k,
                    compile_date_start = v74$compile_date_start,
                    out_csv        = panel_csv,
                    verbose        = verbose)
               owned$source_csv <- panel_csv
          }

          es_args <- .rcv_merge_est_args(spec, owned)
          do.call(MOSAIC::est_suitability, es_args)

          psi_src <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_day.csv")
          if (!file.exists(psi_src))
               stop("est_suitability() did not write ", psi_src,
                    " for cutoff ", T_chr)

          # Atomic freeze: copy to a tempfile in dir_cache then rename into place.
          tmp <- tempfile(pattern = sprintf("psi_%s_", T_chr),
                          tmpdir = dir_cache, fileext = ".csv.tmp")
          ok  <- file.copy(psi_src, tmp, overwrite = TRUE)
          if (!ok) {
               if (file.exists(tmp)) unlink(tmp)
               stop("failed to stage frozen psi copy for cutoff ", T_chr)
          }
          if (!file.rename(tmp, csv_dst)) {
               if (file.exists(tmp)) unlink(tmp)
               stop("failed to atomically place frozen psi for cutoff ", T_chr)
          }

          entry <- list(
               cutoff         = T_chr,
               csv            = basename(csv_dst),
               sha256         = .rcv_file_hash(csv_dst),
               spec_hash      = spec_hash,
               n_seeds        = n_seeds,
               parallel_seeds = par_seeds,
               mosaic_version = mosaic_ver,
               laser_version  = laser_ver)
          if (v74$active) {
               entry$hazard_panel        <- "v7.4_leakfree"
               entry$panel_csv           <- sprintf("panel_v74_%s.csv", T_chr)
               entry$compile_date_start  <- v74$compile_date_start
               entry$gam_train_stop      <- T_chr
          }
          prior_by_cut[[T_chr]] <- entry

          # Persist after every cutoff so a long interrupted run resumes cleanly.
          .rcv_psi_write_manifest(manifest_path,
                                  prior_by_cut[as.character(cutoffs)],
                                  spec = spec, pred_start = pred_start,
                                  pred_stop = pred_stop, mosaic_ver = mosaic_ver,
                                  laser_ver = laser_ver)
     }

     manifest <- .rcv_psi_write_manifest(
          manifest_path, prior_by_cut[as.character(cutoffs)],
          spec = spec, pred_start = pred_start, pred_stop = pred_stop,
          mosaic_ver = mosaic_ver, laser_ver = laser_ver)

     if (verbose)
          message(sprintf("Done: %d cutoff(s) frozen. Cache: %s",
                          length(cutoffs), dir_cache))
     invisible(manifest)
}


# ============================ internal helpers ============================

#' Strip harness-owned date keys from an est_suitability_spec (with warning)
#' @keywords internal
#' @noRd
.rcv_strip_date_keys <- function(spec) {
     date_keys <- c("fit_date_start", "fit_date_stop", "pred_date_start", "pred_date_stop")
     bad <- intersect(names(spec), date_keys)
     if (length(bad)) {
          warning("est_suitability_spec date args ignored (harness-owned): ",
                  paste(bad, collapse = ", "), call. = FALSE)
          spec <- spec[setdiff(names(spec), date_keys)]
     }
     spec
}

#' Resolve whether an est_suitability_spec requests the leak-free v7.4 hazard
#' panel, and the compile window aligned to the psi-LSTM training window.
#'
#' \strong{RAW-spec guard (CLAUDE.md lesson #13).} v7.4 is active ONLY when the
#' caller explicitly set \code{feature_set == "v7.4"} in the spec. The default
#' (no \code{feature_set}) and any other value (\code{"v1"}/\code{"v7.3"}/...)
#' return \code{active = FALSE}, so the byte-unchanged legacy path is used. The
#' guard reads the spec directly; it never keys off a default-merged value.
#'
#' \strong{Window alignment.} \code{compile_date_start} is resolved from the
#' spec's LSTM training start, \code{arch_control$fit_date_start} (the same key
#' \code{est_suitability()} reads, default \code{"2015-01-01"} from the B4
#' fixture). This aligns the hazard-GAM compile window to the LSTM fit window so
#' the imputed hazard-probability magnitudes are not diluted by out-of-window
#' zero rows. The compile \code{date_stop} is the cutoff itself (set by the
#' caller). ml can vary the window by setting
#' \code{est_suitability_spec$arch_control$fit_date_start}.
#' @keywords internal
#' @noRd
.rcv_psi_v74_request <- function(spec) {
     fs     <- spec$feature_set
     active <- is.character(fs) && length(fs) == 1L && identical(fs, "v7.4")
     # Resolve the compile start from the SAME key est_suitability() reads for
     # the LSTM training start. Kept in sync with .est_suitability_lstm_v2()'s
     # `ac$fit_date_start %||% "2015-01-01"` default.
     start <- spec$arch_control$fit_date_start %||% "2015-01-01"
     list(active = isTRUE(active),
          compile_date_start = as.character(as.Date(start)))
}

#' Build a leak-free v7.4 hazard panel for ONE cutoff.
#'
#' Regenerates the suitability panel via \code{compile_suitability_data()} with
#' \code{gam_train_stop = cutoff} so all three hazard GAMs (flood/cyclone/
#' drought) are fit only on rows with \code{date <= cutoff} and predict every
#' row (leak-free). The compile window is \code{[compile_date_start, cutoff]},
#' aligned to the psi-LSTM training window (see \code{\link{prefit_rolling_cv_psi}}
#' panel-window alignment).
#'
#' \strong{Non-destructive isolation.} \code{compile_suitability_data()} writes
#' its output to the CANONICAL
#' \code{PATHS$DATA_CHOLERA_WEEKLY/cholera_country_weekly_suitability_data.csv}.
#' To avoid clobbering that committed artefact (the default psi path reads it),
#' this helper redirects only \code{PATHS$DATA_CHOLERA_WEEKLY} to a per-cutoff
#' scratch dir into which the one input file compile reads from that dir
#' (\code{cholera_surveillance_weekly_combined.csv}) is symlinked/copied. All
#' other read paths (\code{DATA_CLIMATE}, \code{DATA_ENSO}, ...) are untouched.
#' The produced panel is then atomically moved into place at \code{out_csv}.
#' @keywords internal
#' @noRd
.rcv_build_leakfree_panel_v74 <- function(PATHS, cutoff, compile_date_start,
                                          out_csv, verbose = TRUE) {
     if (length(PATHS$DATA_CHOLERA_WEEKLY) != 1L || !nzchar(PATHS$DATA_CHOLERA_WEEKLY))
          stop(".rcv_build_leakfree_panel_v74: PATHS$DATA_CHOLERA_WEEKLY must be a single path.")
     cutoff <- as.Date(cutoff)

     dir_cache <- dirname(out_csv)
     scratch   <- file.path(dir_cache, sprintf(".compile_v74_%s", as.character(cutoff)))
     if (dir.exists(scratch)) unlink(scratch, recursive = TRUE, force = TRUE)
     dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
     on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

     # Stage the one input file compile reads from DATA_CHOLERA_WEEKLY into the
     # scratch dir so the redirected PATHS still finds its cases input.
     cases_in <- file.path(PATHS$DATA_CHOLERA_WEEKLY,
                           "cholera_surveillance_weekly_combined.csv")
     if (!file.exists(cases_in))
          stop(".rcv_build_leakfree_panel_v74: missing cases input ", cases_in)
     cases_stage <- file.path(scratch, "cholera_surveillance_weekly_combined.csv")
     if (!file.copy(cases_in, cases_stage, overwrite = TRUE))
          stop(".rcv_build_leakfree_panel_v74: failed to stage cases input into ", scratch)

     PATHS_c <- PATHS
     PATHS_c$DATA_CHOLERA_WEEKLY <- scratch

     if (verbose)
          message(sprintf(
               "  building leak-free v7.4 panel: compile %s -> %s, gam_train_stop = %s",
               compile_date_start, as.character(cutoff), as.character(cutoff)))

     # Compile settings mirror the canonical panel build (model/LAUNCH.R) so the
     # leak-free panel is column-identical to the canonical one EXCEPT for the
     # two intentional divergences that make it leak-free AND window-aligned:
     #
     #   date_start     -> compile_date_start  (LSTM-window alignment, not 2000)
     #   gam_train_stop -> cutoff              (leak-free hazard GAMs)
     #
     # WINDOW ALIGNMENT (the hard requirement): the hazard-prob MAGNITUDES the
     # LSTM ingests dilute when the GAM-fit window is wider than the LSTM train
     # window (same event labels, more all-zero rows). gam_train_stop = cutoff
     # already caps the GAM-fit UPPER bound at T; date_start = compile_date_start
     # caps its LOWER bound at the LSTM train start, so the GAM fits over exactly
     # [compile_date_start, T] -- the same rows the LSTM trains on.
     #
     # date_stop stays NULL (auto = full availability): the panel MUST still span
     # rows > T so est_suitability() can PREDICT the OOS forecast window. Those
     # future rows carry leak-free hazard probs (GAM fit <= T predicts every row)
     # -- truncating the panel at T would break the prediction window, not just
     # the fit window.
     compile_suitability_data(
          PATHS              = PATHS_c,   # write redirected to scratch (non-destructive)
          cutoff             = NULL,
          use_epidemic_peaks = TRUE,
          date_start         = compile_date_start,
          date_stop          = NULL,
          forecast_mode      = TRUE,
          forecast_horizon   = 9,
          include_lags       = TRUE,
          include_flood_prob = TRUE,
          gam_train_stop     = cutoff)

     produced <- file.path(scratch, "cholera_country_weekly_suitability_data.csv")
     if (!file.exists(produced))
          stop(".rcv_build_leakfree_panel_v74: compile_suitability_data() did not produce ",
               produced)

     # Atomic move into place (tempfile in the destination dir + rename).
     tmp <- tempfile(pattern = "panel_v74_", tmpdir = dir_cache, fileext = ".csv.tmp")
     if (!file.copy(produced, tmp, overwrite = TRUE)) {
          if (file.exists(tmp)) unlink(tmp)
          stop(".rcv_build_leakfree_panel_v74: failed to stage panel copy for ", out_csv)
     }
     if (!file.rename(tmp, out_csv)) {
          if (file.exists(tmp)) unlink(tmp)
          stop(".rcv_build_leakfree_panel_v74: failed to atomically place ", out_csv)
     }
     invisible(out_csv)
}

#' Number of pooled psi seeds implied by an est_suitability_spec.
#' Mirrors est_suitability()/arch_control defaults; n_seeds MUST be in the hash.
#' @keywords internal
#' @noRd
.rcv_spec_n_seeds <- function(spec) {
     n <- spec$arch_control$n_seeds
     if (is.null(n)) return(NA_integer_)
     as.integer(n)
}

#' Parallel-seed worker count implied by an est_suitability_spec (provenance).
#' @keywords internal
#' @noRd
.rcv_spec_parallel_seeds <- function(spec) {
     p <- spec$arch_control$parallel_seeds
     if (is.null(p)) return(NA_integer_)
     as.integer(p)
}

#' Deterministic content hash of a config object (no external dep required).
#'
#' Deterministic, dependency-free content hash: serialize the object to a
#' version-pinned byte stream (stable for a fixed R serialization version) then
#' fold into a fixed-width hex digest. \code{n_seeds} enters via the serialized
#' spec (\code{arch_control$n_seeds}), so a change in pooled-seed count
#' invalidates the key. Dependency-free on purpose (no undeclared \code{digest}).
#' @keywords internal
#' @noRd
.rcv_obj_hash <- function(obj) {
     raw <- serialize(obj, connection = NULL, version = 2L)
     .rcv_bytes_hash(raw)
}

#' spec_hash over (fit_date_stop, modeling spec) for one cutoff.
#'
#' When the spec requests the v7.4 leak-free hazard panel, a normalized
#' \code{v74_panel} block (leakfree marker + resolved compile window +
#' gam_train_stop = cutoff) is folded into the hashed key. This makes the cache
#' key sensitive to the panel provenance: a v7.3/default panel, or a v7.4 panel
#' compiled over a different \code{compile_date_start}, hashes differently and
#' cannot be silently reused. Symmetric across prefit and
#' \code{run_rolling_cv(psi_cache=)} because both call this helper on the same
#' (date-stripped) spec.
#' @keywords internal
#' @noRd
.rcv_psi_spec_hash <- function(cutoff, spec) {
     # Drop execution-only knobs that do not change the cached psi CONTENT
     # (parallel_seeds only sets how many seeds fit concurrently). n_seeds stays
     # in the hash (it changes the pooled psi). This keeps the cache valid when
     # the prefit and read phases run with different parallel_seeds (e.g. prefit
     # serial on the whole box, calibrate cells that don't refit at all).
     if (!is.null(spec$arch_control)) spec$arch_control$parallel_seeds <- NULL
     key <- list(fit_date_stop = as.character(as.Date(cutoff)),
                 est_suitability_spec = spec)
     v74 <- .rcv_psi_v74_request(spec)
     if (v74$active)
          key$v74_panel <- list(leakfree = TRUE,
                                compile_date_start = v74$compile_date_start,
                                gam_train_stop     = as.character(as.Date(cutoff)))
     .rcv_obj_hash(key)
}

#' Deterministic byte-fold fingerprint of a file's bytes (dependency-free).
#' @keywords internal
#' @noRd
.rcv_file_hash <- function(path) {
     raw <- readBin(path, what = "raw", n = file.info(path)$size)
     .rcv_bytes_hash(raw)
}

#' Deterministic dependency-free hash of a raw vector (64-hex-char digest).
#' Not cryptographic; only a stable content fingerprint for the no-digest path.
#' @keywords internal
#' @noRd
.rcv_bytes_hash <- function(raw) {
     if (length(raw) == 0L) raw <- as.raw(0L)
     n  <- length(raw)
     iv <- as.integer(raw)                      # 0..255
     # 8 independent rolling 32-bit FNV-1a-style accumulators over strided bytes,
     # concatenated to a 64-hex-char fingerprint (stable across platforms). All
     # arithmetic stays in double precision mod 2^32 (FNV words exceed .Machine
     # integer range), so format each word's unsigned 32-bit value directly.
     hex_words <- character(8)
     for (lane in seq_len(8)) {
          h <- 2166136261                        # FNV offset basis (as double)
          idx <- if (lane <= n) seq.int(lane, n, by = 8L) else integer(0)
          for (b in iv[idx]) {
               # XOR the low byte without leaving double range: x XOR b == x - (x mod 256) + ((x mod 256) XOR b)
               lo <- h %% 256
               h  <- h - lo + bitwXor(as.integer(lo), b)
               h  <- (h * 16777619) %% 4294967296 # FNV prime, mod 2^32
          }
          hi <- as.integer((h %/% 65536) %% 65536)   # top 16 bits
          lo <- as.integer(h %% 65536)                # bottom 16 bits
          hex_words[lane] <- sprintf("%04x%04x", hi, lo)
     }
     paste(hex_words, collapse = "")
}

#' laser-cholera engine version string (best-effort; never errors).
#' @keywords internal
#' @noRd
.rcv_laser_version <- function() {
     v <- tryCatch({
          lc <- reticulate::import("laser.cholera", delay_load = FALSE)
          as.character(lc$`__version__`)
     }, error = function(e) NA_character_)
     if (length(v) != 1L || is.na(v)) NA_character_ else v
}

#' Read a prior psi_manifest.json (returns list(cutoffs=<list>) or empty).
#' @keywords internal
#' @noRd
.rcv_psi_read_manifest <- function(path) {
     if (!file.exists(path)) return(list(cutoffs = list()))
     man <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
                     error = function(e) NULL)
     if (is.null(man) || is.null(man$cutoffs)) return(list(cutoffs = list()))
     # jsonlite list-of-objects -> list of named lists with scalar fields unboxed
     cuts <- lapply(man$cutoffs, function(e) lapply(e, function(x) {
          if (is.list(x) && length(x) == 1L) x[[1]] else x
     }))
     list(cutoffs = cuts)
}

#' Write psi_manifest.json from the per-cutoff entry list.
#' @keywords internal
#' @noRd
.rcv_psi_write_manifest <- function(path, entry_list, spec, pred_start, pred_stop,
                                    mosaic_ver, laser_ver) {
     entry_list <- Filter(Negate(is.null), entry_list)
     manifest <- list(
          experiment      = "rolling_cv_psi_cache",
          created         = as.character(Sys.time()),
          mosaic_version  = mosaic_ver,
          laser_version   = laser_ver,
          pred_date_start = as.character(pred_start),
          pred_date_stop  = as.character(pred_stop),
          est_suitability_spec = spec,
          cutoffs         = unname(entry_list))
     .rcv_write_json(manifest, path)
     manifest
}
