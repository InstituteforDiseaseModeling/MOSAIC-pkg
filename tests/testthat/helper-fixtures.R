# =============================================================================
# helper-fixtures.R -- memoized expensive fixtures shared across tests.
#
# sample_parameters() is the dominant pure-R cost in the suite (it runs the
# full prior-sampling pipeline). Many tests call it ONLY to obtain a valid
# sampled config whose STRUCTURE / derivation-identities they then assert --
# they do not depend on the specific seed. .cached_sampled_config(seed)
# memoizes per-seed within a process so those tests share one draw instead of
# paying the cost N times.
#
# IMPORTANT: tests that genuinely exercise the sampler's RNG (determinism for a
# fixed seed, seed->different-draw behavior, distributional loops over many
# seeds) MUST keep calling MOSAIC::sample_parameters() directly -- the cache
# would mask exactly the behavior they verify.
# =============================================================================

.cached_sampled_config <- local({
  .cache <- list()
  function(seed = 123L, ...) {
    key <- paste0("seed=", seed, "|", paste(names(list(...)), unlist(list(...)),
                                            sep = "=", collapse = "|"))
    if (is.null(.cache[[key]])) {
      .cache[[key]] <<- MOSAIC::sample_parameters(seed = seed, verbose = FALSE, ...)
    }
    .cache[[key]]
  }
})

# Relative-error expectation. expect_equal(x, y, tolerance = t) switches to an
# ABSOLUTE difference when mean(abs(y)) <= t, so a CFR near 0.03 checked with
# tolerance = 0.1 passes for anything within +/-0.1. Use this for small
# quantities such as rates and CFRs.
expect_rel_equal <- function(object, expected, rel) {
  err <- max(abs(object / expected - 1))
  testthat::expect_lt(err, rel, label = sprintf("max relative error %.4g", err))
}

# Hold a null graphics device for the calling test. The documentation-figure
# functions print() each plot to the current device before saving it, which
# under Rscript would otherwise open the default device and leave Rplots.pdf in
# the test directory. Devices opened during the test are closed afterwards.
local_null_device <- function(env = parent.frame()) {
  before <- grDevices::dev.list()
  grDevices::pdf(NULL)
  withr::defer({
    for (d in rev(setdiff(grDevices::dev.list(), before))) grDevices::dev.off(d)
  }, envir = env)
  invisible(NULL)
}

# Every text label in a rendered ggplot (including plots composed with cowplot,
# whose panels are embedded grobs), for asserting on titles and legend keys.
plot_text_labels <- function(p) {
  walk <- function(g) {
    out <- character(0)
    if (!is.null(g$label) && (is.character(g$label) || is.expression(g$label))) {
      out <- as.character(g$label)
    }
    kids <- c(if (inherits(g, "gTree")) as.list(g$children), if (!is.null(g$grobs)) g$grobs)
    for (k in kids) if (inherits(k, "grob") || inherits(k, "gtable")) out <- c(out, walk(k))
    out
  }
  walk(ggplot2::ggplotGrob(p))
}
