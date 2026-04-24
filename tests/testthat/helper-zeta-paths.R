# Shared helper used by the zeta prior test files.
# Returns a minimal PATHS-like list whose MODEL_INPUT and DOCS_FIGURES
# directories live inside a per-test tempdir so est_zeta_*_prior() writes
# never pollute the working tree.
.mk_test_paths <- function() {
     tmp <- withr::local_tempdir(.local_envir = parent.frame())
     list(
          ROOT         = tmp,
          MODEL_INPUT  = file.path(tmp, "model", "input"),
          DOCS_FIGURES = file.path(tmp, "figures")
     )
}
