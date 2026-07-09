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
#' \strong{Atomic cache (concurrency-safe).} \code{est_suitability()} writes a
#' single global \code{MODEL_INPUT/pred_psi_suitability_day.csv}; this function
#' copies that file into \code{dir_cache/psi_<T>.csv} via a tempfile +
#' \code{file.rename} so a partially-written cache file can never be observed.
#'
#' \strong{Cache key (spec hash).} For each cutoff a \code{spec_hash} is computed
#' over \code{list(fit_date_stop = T, est_suitability_spec)} where the resolved
#' spec includes \code{arch_control$n_seeds}. The number of pooled seeds is part
#' of the key: refitting with a different seed count invalidates the cache.
#' \code{run_rolling_cv(psi_cache=)} recomputes the same hash from its own
#' \code{est_suitability_spec} and \strong{hard-errors} on a mismatch.
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
               message(sprintf("[%d/%d] %s  fitting psi (fit_date_stop=%s)",
                               k, length(cutoffs), T_chr, T_chr))

          es_args <- .rcv_merge_est_args(spec, list(
               PATHS           = PATHS,
               fit_date_stop   = T_k,
               pred_date_start = pred_start,
               pred_date_stop  = pred_stop))
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

          prior_by_cut[[T_chr]] <- list(
               cutoff         = T_chr,
               csv            = basename(csv_dst),
               sha256         = .rcv_file_hash(csv_dst),
               spec_hash      = spec_hash,
               n_seeds        = n_seeds,
               parallel_seeds = par_seeds,
               mosaic_version = mosaic_ver,
               laser_version  = laser_ver)

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
