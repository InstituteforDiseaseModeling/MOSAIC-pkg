#!/usr/bin/env Rscript
# =============================================================================
# build_caches.R -- repair the psi_evolve caches into usable run_rolling_cv()
# psi caches for the 3-arm downstream A/B screen.
#
# ARMS
#   prod   <- psi_cache_P000E   LSTM trunk          (the comparator)
#   nd     <- psi_cache_NDe     DLinear trunk       (the treatment)
#   nd_rep <- psi_cache_NDeR    DLinear, seed_base=1001  (the NOISE FLOOR)
#
# nd/nd_rep are an existing same-spec disjoint-seed replicate pair, so the
# downstream DLinear replicate floor costs zero psi compute. The effect
# (prod vs nd) is only believable if it exceeds the floor (nd vs nd_rep).
# A same-arm CALIBRATION replicate is impossible -- the pipeline is
# deterministic given (config, priors, control) -- so this psi-seed replicate
# is the only noise floor obtainable at all.
#
# WHY THIS SCRIPT EXISTS. claude/psi_evolve/run_arm.R shards cutoffs across 10
# processes; each calls prefit_rolling_cv_psi(), which rewrites the WHOLE
# psi_manifest.json with only its own shard. Last writer wins, so every cache
# on disk records 1 of its 10 cutoffs. .rcv_psi_cache_lookup() resolves cutoffs
# from the MANIFEST (R/run_rolling_cv.R:580-587), so 9 of 10 cutoffs hard-abort.
# The psi_<T>.csv files themselves are complete and correct.
#
# The psi_evolve caches are READ-ONLY. This writes new dirs under ~/psi_ab/
# containing SYMLINKS to the original CSVs plus a correct manifest. Nothing
# under ~/psi_evolve is modified.
# =============================================================================
suppressMessages(library(MOSAIC))

SRC  <- path.expand("~/psi_evolve")
DEST <- path.expand("~/psi_ab")

# Integer literals and KEY ORDER are both load-bearing: the spec hash is byte
# identity over serialize(), so 10L != 10 and a reordered list hashes
# differently. These were read back out of the original manifests. Do not
# reformat, reorder, or "tidy" them.
ARMS <- list(
     prod = list(
          src  = "psi_cache_P000E",
          spec = list(feature_set = "v7.3",
                      arch_control = list(n_seeds = 10L, parallel_seeds = 1L, lead = 0L,
                                          country_static = "off", country_balance = FALSE,
                                          epoch_select_seeds = 2L)),
          sentinel = list(cutoff = "2024-10-01",
                          hash = "f489b128d447e6c6e3ddbbb84bc763a8065dd9eff42fcdf8baca1390b5984be0")),
     nd = list(
          src  = "psi_cache_NDe",
          spec = list(feature_set = "v7.3",
                      arch_control = list(n_seeds = 10L, parallel_seeds = 1L, lead = 0L,
                                          country_static = "off", country_balance = FALSE,
                                          trunk = "dlinear", dlinear_pad = "edge",
                                          epoch_select_seeds = 2L)),
          sentinel = list(cutoff = "2026-04-15",
                          hash = "d5a46b1854d96f50ea4a4fa02e272c4034d9f9f8bf4f2e384c1c0d38a789b9b8")),
     nd_rep = list(
          src  = "psi_cache_NDeR",
          spec = list(feature_set = "v7.3",
                      arch_control = list(n_seeds = 10L, parallel_seeds = 1L, lead = 0L,
                                          seed_base = 1001L,
                                          country_static = "off", country_balance = FALSE,
                                          trunk = "dlinear", dlinear_pad = "edge",
                                          epoch_select_seeds = 2L)),
          sentinel = list(cutoff = "2025-07-01",
                          hash = "f2ed9500a736dec8a812481841e3d8e06f66bda093bbb134d7b137c4bc65d530")),

     # ---- LSTM follow-up screen (2026-09-28) ---------------------------------
     # p000  = main-production proxy: LSTM, no D9b/N8, OLD epoch rule (0.91.1)
     # p000r = p000's disjoint-seed replicate -> the LSTM noise floor the first
     #         screen lacked (its nd/nd_rep floor measured DLinear noise only)
     # n9    = the branch's shipped defaults: D9b (country_static) + N8
     #         (country_balance) + epoch fix (0.91.13)
     p000 = list(
          src  = "psi_cache_P000",
          spec = list(feature_set = "v7.3",
                      arch_control = list(n_seeds = 10L, parallel_seeds = 1L, lead = 0L)),
          sentinel = list(cutoff = "2025-10-01",
                          hash = "b94ebbf8eea93ab2038f463870ea4de82a971d58005f5ac046da29587112b8f2")),
     p000r = list(
          src  = "psi_cache_P000R",
          spec = list(feature_set = "v7.3",
                      arch_control = list(n_seeds = 10L, parallel_seeds = 1L, lead = 0L,
                                          seed_base = 1001L)),
          sentinel = list(cutoff = "2025-10-01",
                          hash = "8935461424c737f87452b63050eb2af8df6b9100267e7c18006ae398e2f0ddb8")),
     n9 = list(
          src  = "psi_cache_N9",
          spec = list(feature_set = "v7.3",
                      arch_control = list(n_seeds = 10L, parallel_seeds = 1L, lead = 0L,
                                          country_static = "frozen", country_balance = TRUE,
                                          epoch_select_seeds = 2L)),
          sentinel = list(cutoff = "2026-04-15",
                          hash = "d403af8842e73f107d252610349906685a12eb48ec1877c001884f709399dde8"))
)

dir.create(DEST, showWarnings = FALSE, recursive = TRUE)
specs_out <- list()

for (arm in names(ARMS)) {
     A       <- ARMS[[arm]]
     src_dir <- file.path(SRC, A$src)
     out_dir <- file.path(DEST, paste0("cache_", arm))
     dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

     csvs <- sort(list.files(src_dir, pattern = "^psi_\\d{4}-\\d{2}-\\d{2}\\.csv$"))
     stopifnot(length(csvs) > 0L)
     cutoffs <- sub("^psi_(.*)\\.csv$", "\\1", csvs)

     spec_hashed <- MOSAIC:::.rcv_strip_date_keys(A$spec)

     entries <- lapply(seq_along(cutoffs), function(i) {
          T_chr <- cutoffs[i]
          src   <- file.path(src_dir, csvs[i])
          dst   <- file.path(out_dir, csvs[i])
          if (!file.exists(dst)) file.symlink(src, dst)
          list(cutoff         = T_chr,
               csv            = csvs[i],
               spec_hash      = MOSAIC:::.rcv_psi_spec_hash(T_chr, spec_hashed),
               n_seeds        = 10L,
               parallel_seeds = 1L,
               mosaic_version = "0.91.13",
               source         = src)
     })

     # Hard-assert the sentinel BEFORE writing anything a run could consume. If a
     # recompute stops matching the hash the original wave recorded, the hash
     # contract has drifted and every cache built here is invalid.
     got <- entries[[match(A$sentinel$cutoff, cutoffs)]]$spec_hash
     if (!identical(got, A$sentinel$hash))
          stop(sprintf("arm %s: spec_hash for %s recomputed as %s but the original manifest recorded %s. Hash contract changed -- do NOT use these caches.",
                       arm, A$sentinel$cutoff, got, A$sentinel$hash))

     jsonlite::write_json(
          list(experiment      = "rolling_cv_psi_cache",
               created         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
               mosaic_version  = as.character(utils::packageVersion("MOSAIC")),
               pred_date_start = "2018-01-01",
               pred_date_stop  = "2027-02-04",
               rebuilt_from    = src_dir,
               rebuilt_why     = "original manifest was shard-raced to 1 of 10 cutoffs",
               est_suitability_spec = A$spec,
               cutoffs         = entries),
          file.path(out_dir, "psi_manifest.json"),
          auto_unbox = TRUE, pretty = TRUE, digits = NA)

     specs_out[[arm]] <- A$spec
     cat(sprintf("%-7s <- %-18s : %2d cutoffs, sentinel %s OK\n",
                 arm, A$src, length(entries), A$sentinel$cutoff))
}

# The run script must pass a spec that serializes byte-identically to the one
# hashed here. Ship the R object itself rather than retyping the literal.
saveRDS(specs_out, file.path(DEST, "arm_specs.rds"))

# ---- Verify by driving the package's OWN lookup over every cutoff ----------
cat("\nVerifying with .rcv_psi_cache_lookup() (round-tripped through the rds):\n")
specs_rt <- readRDS(file.path(DEST, "arm_specs.rds"))
for (arm in names(ARMS)) {
     out_dir <- file.path(DEST, paste0("cache_", arm))
     man     <- MOSAIC:::.rcv_psi_read_manifest(file.path(out_dir, "psi_manifest.json"))
     keys    <- vapply(man$cutoffs, function(e) as.character(e$cutoff), character(1))
     ok <- vapply(keys, function(T_chr)
          file.exists(MOSAIC:::.rcv_psi_cache_lookup(out_dir, man, T_chr, specs_rt[[arm]])),
          logical(1))
     cat(sprintf("  %-7s : %2d/%2d cutoffs resolve and exist\n", arm, sum(ok), length(ok)))
     stopifnot(all(ok))
}
cat(sprintf("\nAll %d caches usable.\n", length(ARMS)))
