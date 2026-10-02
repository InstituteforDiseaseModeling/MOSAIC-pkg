# Regression tests for the multi-source fit target + per-observation confidence
# weight matrices added to config_default in v4.1 (multi-source integration).

test_that("config_default carries aligned reported_cases/deaths weight matrices", {
  cfg <- MOSAIC::config_default
  rc  <- cfg$reported_cases;  rd  <- cfg$reported_deaths
  rcw <- cfg$reported_cases_weight;  rdw <- cfg$reported_deaths_weight

  expect_true(is.matrix(rcw)); expect_true(is.matrix(rdw))
  expect_identical(dim(rcw), dim(rc))
  expect_identical(dim(rdw), dim(rd))
})

test_that("weight present iff value present, and weights lie in [0,1]", {
  cfg <- MOSAIC::config_default
  for (ch in c("cases", "deaths")) {
    v <- cfg[[paste0("reported_", ch)]]
    w <- cfg[[paste0("reported_", ch, "_weight")]]
    # finite weight exactly where the observation is non-NA
    expect_true(all(is.finite(w[!is.na(v)])),
                info = paste(ch, "non-NA cells must carry a finite weight"))
    expect_true(all(is.na(w[is.na(v)])),
                info = paste(ch, "NA cells must carry NA weight"))
    fin <- w[is.finite(w)]
    expect_true(all(fin >= 0 & fin <= 1),
                info = paste(ch, "weights must be in [0,1]"))
  }
})

test_that("fit matrices cover all 40 modelled locations in canonical order", {
  cfg <- MOSAIC::config_default
  expect_identical(cfg$location_name, MOSAIC::iso_codes_mosaic)
  expect_equal(nrow(cfg$reported_cases), length(MOSAIC::iso_codes_mosaic))
  # Multi-source target: every modelled country should carry case data EXCEPT a
  # documented set with no in-window surveillance signal. ERI has no 2023+
  # surveillance under current sources (its pre-rebuild content was 365 all-zero
  # documented_zero placeholder cells); it must remain in the canonical 40-location
  # order for the metapop structure, and an all-NA fit row is handled safely by the
  # min_obs_for_likelihood gate (0 contribution). Any OTHER country going empty is
  # an unexpected coverage regression and still fails here.
  no_surveillance_ok <- "ERI"
  empty <- cfg$location_name[rowSums(!is.na(cfg$reported_cases)) == 0L]
  expect_true(all(empty %in% no_surveillance_ok),
              info = paste0("unexpected country(ies) with zero non-NA case cells: ",
                            paste(setdiff(empty, no_surveillance_ok), collapse = ", "),
                            " (only ", paste(no_surveillance_ok, collapse = ", "),
                            " is a documented no-surveillance location)"))
})

test_that("weight matrices survive the JSON round-trip as aligned matrices", {
  cfg <- MOSAIC::config_default
  fp  <- system.file("extdata", "config_default.json", package = "MOSAIC")
  skip_if(fp == "", "config_default.json not installed")
  js <- MOSAIC::read_json_to_list(fp)
  expect_true(is.matrix(js$reported_cases_weight))
  expect_identical(dim(js$reported_cases_weight), dim(cfg$reported_cases))
  # positional values match (dimnames are dropped on round-trip by design)
  expect_equal(unname(js$reported_cases_weight),  unname(cfg$reported_cases_weight))
  expect_equal(unname(js$reported_deaths_weight), unname(cfg$reported_deaths_weight))
})

test_that("get_location_config keeps weight rows aligned with reported_cases on subset", {
  cfg <- MOSAIC::config_default
  one <- MOSAIC::get_location_config(iso = cfg$location_name[1], config = cfg)
  expect_equal(nrow(one$reported_cases), 1L)
  expect_equal(nrow(one$reported_cases_weight),  nrow(one$reported_cases))
  expect_equal(nrow(one$reported_deaths_weight), nrow(one$reported_deaths))
  expect_identical(dim(one$reported_cases_weight), dim(one$reported_cases))
})

# reported_tier (v0.101.0; release red team CD-4) has the same role and build path
# as the weight matrices, so it gets the same contract: an integer matrix
# aligned with reported_cases (rows in location_name order), present exactly
# where a case or death count is (data-raw/make_config_default.R), coded 1
# observed / 2 reconstructed / 3 imputed. An NA on an observed cell would drop
# the week from the dispersion fit while the likelihood still scores it.
check_reported_tier <- function(cfg) {
  tier <- cfg$reported_tier
  expect_true(is.matrix(tier) && is.integer(tier))
  expect_identical(dim(tier), dim(cfg$reported_cases))
  present <- !is.na(cfg$reported_cases) | !is.na(cfg$reported_deaths)
  expect_identical(unname(!is.na(tier)), unname(present))
  expect_true(all(tier[!is.na(tier)] %in% 1:3))
}

check_reported_tier_json <- function(cfg, js) {
  expect_true(is.matrix(js$reported_tier))
  expect_identical(dim(js$reported_tier), dim(cfg$reported_tier))
  # Positional values match (dimnames are dropped on the round trip by design).
  expect_equal(unname(js$reported_tier), unname(cfg$reported_tier))
}

test_that("reported_tier is an aligned integer matrix, coded 1-3, present exactly where a count is", {
  cfg <- MOSAIC::config_default
  skip_if(is.null(cfg$reported_tier), "config_default carries no reported_tier")
  check_reported_tier(cfg)
  one <- MOSAIC::get_location_config(iso = cfg$location_name[1], config = cfg)
  expect_identical(as.integer(one$reported_tier), as.integer(cfg$reported_tier[1, ]))
})

test_that("reported_tier survives the JSON round trip", {
  cfg <- MOSAIC::config_default
  skip_if(is.null(cfg$reported_tier), "config_default carries no reported_tier")
  fp <- system.file("extdata", "config_default.json", package = "MOSAIC")
  skip_if(fp == "", "config_default.json not installed")
  check_reported_tier_json(cfg, MOSAIC::read_json_to_list(fp))
})
