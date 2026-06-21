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
