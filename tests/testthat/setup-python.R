# =============================================================================
# setup-python.R -- one-time, per-process test environment setup.
#
# testthat auto-sources every setup-*.R ONCE per worker process (serial run:
# once; parallel run: once per PSOCK worker) BEFORE any test file.
#
# It used to do two things. The second -- a one-time probe of the Python
# interpreter and the two laser-cholera submodules, cached in options() for
# helper-skips.R to read -- went with the Python engine in v0.67.0. Its only
# consumer was skip_if_no_python_likelihood(), which gated the R-vs-Python
# likelihood parity tests; those tests and that helper are gone, so the probe
# was caching three flags nothing read, at the cost of a reticulate interpreter
# init plus two module imports (~6 s) in EVERY test process. The surviving
# Python probe (tensorflow) was already lazy, and stays that way in
# helper-skips.R.
#
# What remains is thread pinning, which must happen before anything -- BLAS,
# Arrow, or a TensorFlow import from a suitability test -- can start a thread
# pool. This is the CLAUDE.md BLAS deadlock landmine: parallel test workers must
# not oversubscribe CPU. .mosaic_set_blas_threads() delegates to
# .mosaic_set_all_thread_env(1L), which sets all six CLAUDE.md thread vars plus
# ARROW_NUM_THREADS, all to "1".
# =============================================================================

suppressWarnings(try(MOSAIC:::.mosaic_set_blas_threads(1L), silent = TRUE))
