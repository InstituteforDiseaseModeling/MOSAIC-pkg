# Tests for the epidemic_peaks field shipped in config_default and the
# matching validation block in make_simulation_config(). Supports the Python
# likelihood port (laser-cholera#47) by guaranteeing the field is present
# in the shipped config so worker-side scoring has the peaks it needs.

# Minimal call shim that injects a custom epidemic_peaks into config_default
# and routes through make_simulation_config so validation runs. Strips tracking
# fields (zeta_ratio, decay_days_spread) that live on config_default but are
# not accepted by make_simulation_config's signature.
.make_minimal_then_call <- function(epidemic_peaks) {
  args <- MOSAIC::config_default
  args$metadata <- NULL
  args$zeta_ratio <- NULL
  args$decay_days_spread <- NULL
  args$reported_cases_weight <- NULL
  args$reported_deaths_weight <- NULL
  args$reported_tier <- NULL
  args$output_file_path <- NULL
  args$epidemic_peaks <- epidemic_peaks
  do.call(MOSAIC::make_simulation_config, args)
}

test_that("config_default ships epidemic_peaks as a 2-col character data.frame", {
  ep <- MOSAIC::config_default$epidemic_peaks
  expect_s3_class(ep, "data.frame")
  expect_named(ep, c("iso_code", "peak_date"))
  expect_type(ep$iso_code, "character")
  expect_type(ep$peak_date, "character")
  expect_gt(nrow(ep), 0)

  # peak_date values parse as ISO yyyy-mm-dd
  parsed <- suppressWarnings(as.Date(ep$peak_date))
  expect_false(any(is.na(parsed)))
})

test_that("config_default metadata version is bumped to 3.2+", {
  v <- MOSAIC::config_default$metadata$version
  expect_true(utils::compareVersion(v, "3.2") >= 0,
              info = sprintf("got version %s", v))
})

test_that("make_simulation_config validation: malformed epidemic_peaks errors", {
  # Missing required columns
  bad <- data.frame(iso_code = "MOZ", wrong_col = "2024-01-01",
                    stringsAsFactors = FALSE)
  expect_error(
    .make_minimal_then_call(epidemic_peaks = bad),
    "epidemic_peaks is missing required column"
  )

  # Not a data.frame
  expect_error(
    .make_minimal_then_call(epidemic_peaks = list(iso_code = "MOZ",
                                                  peak_date = "2024-01-01")),
    "epidemic_peaks must be a data\\.frame"
  )

  # Unparseable date
  bad_dates <- data.frame(iso_code = "MOZ", peak_date = "not-a-date",
                          stringsAsFactors = FALSE)
  expect_error(
    .make_minimal_then_call(epidemic_peaks = bad_dates),
    "unparseable date"
  )
})

test_that("make_simulation_config validation: unknown iso_code is a hard error (v0.32.0+)", {
  unknown <- data.frame(iso_code = "ZZZ", peak_date = "2024-01-01",
                        stringsAsFactors = FALSE)
  # v0.32.0 promoted the prior warning to a hard error: laser-cholera v0.13+
  # asserts every iso_code in epidemic_peaks appears in location_name, so
  # MOSAIC fails fast at config construction instead of letting the worker
  # crash. See R/make_simulation_config.R::~918 and NEWS v0.32.0.
  expect_error(
    .make_minimal_then_call(epidemic_peaks = unknown),
    "epidemic_peaks contains iso_code\\(s\\) not in location_name: ZZZ"
  )
})


test_that("the shipped config_default and priors_default are one build", {
  # A window move needs two priors passes (est_initial_V1_V2() divides by the
  # installed config's populations), so a config built against the wrong pass of
  # the priors, or priors built for another window, must not ship.
  cfg <- MOSAIC::config_default
  pri <- MOSAIC::priors_default
  expect_identical(as.character(pri$metadata$build_date_start), as.character(cfg$date_start))

  # The config's initial conditions are the priors' Beta means, normalised so each
  # location's six proportions sum to 1: within a few percent of the means for the
  # large compartments (v7.0: 0.9966-1.0165). The first pass of the 2023 -> 2018
  # move left V1 x1.11-1.19 off; E and I are tiny counts and round too coarsely.
  bmean <- function(par, iso) {
    p <- pri$parameters_location[[par]]$location[[iso]]$parameters
    p$shape1 / (p$shape1 + p$shape2)
  }
  for (comp in c("R", "V1", "V2", "S")) {
    par <- paste0("prop_", comp, "_initial")
    r <- cfg[[par]] / vapply(cfg$location_name, function(iso) bmean(par, iso), numeric(1))
    expect_true(all(r > 0.98 & r < 1.03),
                label = sprintf("%s / prior mean within [0.98, 1.03] (%.4f-%.4f)", par, min(r), max(r)))
  }
  props <- cfg$prop_S_initial + cfg$prop_E_initial + cfg$prop_I_initial +
    cfg$prop_R_initial + cfg$prop_V1_initial + cfg$prop_V2_initial
  expect_true(all(abs(props - 1) < 1e-9))

  # Doses are whole and non-negative, and no request delivered from 2023 has a
  # second round (the ICG suspended two-dose outbreak response in October 2022).
  nu <- cfg$nu_1_jt + cfg$nu_2_jt
  expect_true(all(is.finite(nu)) && all(cfg$nu_1_jt >= 0) && all(cfg$nu_2_jt >= 0) && all(nu == round(nu)))
  dates <- as.Date(cfg$date_start) + seq_len(ncol(cfg$nu_2_jt)) - 1L
  expect_true(all(cfg$nu_2_jt[, dates >= as.Date("2023-01-01")] == 0))
})
