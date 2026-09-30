library(testthat)
library(MOSAIC)

# -----------------------------------------------------------------------------
# Helper: write a minimal synthetic CSV in the GTFCC schema to a temp dir,
# construct a fake PATHS that points at it, and return both.
# -----------------------------------------------------------------------------
make_fake_gtfcc <- function(rows) {
  root <- tempfile("fake_root_")
  csv_dir <- file.path(root, "ees-cholera-mapping", "data", "cholera",
                        "epicentre", "gtfcc")
  dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

  default_cols <- c("country", "raw_tooltip", "raw_content", "req_id", "req_year",
                    "event_date", "event_type", "round_id", "doses", "vaccine",
                    "duration_days", "via_tool", "by_org")
  df <- rows
  for (col in default_cols) if (!col %in% names(df)) df[[col]] <- NA
  df <- df[, default_cols, drop = FALSE]

  csv_path <- file.path(csv_dir, "cholera_vacc_requests.csv")
  write.csv(df, csv_path, row.names = FALSE, na = "")
  list(PATHS = list(ROOT = root), csv_path = csv_path)
}

# Fake minimal config: a single country with a defined population.
make_fake_config <- function(iso, N, date_start) {
  list(location_name = iso, N_j_initial = N, date_start = as.character(date_start))
}

# -----------------------------------------------------------------------------
# 1. Waning: one R1 dose decays per exp(-omega_1 * (t0 - d - lag))
# -----------------------------------------------------------------------------
test_that("V1 waning decays exponentially with omega_1 and t_lag", {
  N <- 1e7
  doses <- 1e6
  t0 <- as.Date("2025-01-01")
  d  <- as.Date("2024-01-01")      # exactly 1 year before t0
  omega_1 <- 1 / 1000               # half-life ~ 693 days
  t_lag <- 14

  rows <- data.frame(
    country = "Angola",
    req_id = "R1",
    event_date = c(d, d),
    event_type = c("Delivery", "Round"),
    round_id = c(NA, "C01-R01"),
    doses = c(doses, doses),
    vaccine = c("Shanchol", "Shanchol"),
    stringsAsFactors = FALSE
  )
  fake <- make_fake_gtfcc(rows)
  cfg <- make_fake_config("AGO", N, t0)

  out <- est_initial_V1_V2(fake$PATHS, cfg,
                           omega_1 = omega_1, omega_2 = 0.001, t_lag = t_lag,
                           phi_1 = 0.8, phi_2 = 0.9,
                           cv = 0.1, verbose = FALSE)

  age <- as.numeric(t0 - d) - t_lag
  expected_V1 <- 0.8 * doses * exp(-omega_1 * age) / N
  beta_V1 <- out$parameters_location$prop_V1_initial$location$AGO$parameters
  observed_mean <- beta_V1$shape1 / (beta_V1$shape1 + beta_V1$shape2)
  expect_equal(observed_mean, expected_V1, tolerance = 1e-6)
})

# -----------------------------------------------------------------------------
# 2. Round pairing (engine semantics): V2 = phi_2 * R02 * waning,
#    V1 = (phi_1 * R01 - phi_2 * R02) * waning
# -----------------------------------------------------------------------------
test_that("paired two-dose campaign routes effective second doses V1 -> V2", {
  N <- 1e7
  r1_doses <- 1e6
  r2_doses <- 0.8e6
  t0 <- as.Date("2025-01-01")
  d_r1 <- as.Date("2024-06-01")
  d_r2 <- as.Date("2024-07-01")
  omega_1 <- 1e-3
  omega_2 <- 5e-4
  t_lag <- 14

  rows <- data.frame(
    country = "Kenya",
    req_id = c("R1", "R1", "R1"),
    event_date = c(d_r1, d_r1, d_r2),
    event_type = c("Delivery", "Round", "Round"),
    round_id = c(NA, "C01-R01", "C01-R02"),
    doses = c(r1_doses, r1_doses, r2_doses),
    vaccine = c("Shanchol", "Shanchol", "Shanchol"),
    stringsAsFactors = FALSE
  )
  fake <- make_fake_gtfcc(rows)
  cfg <- make_fake_config("KEN", N, t0)

  phi_1 <- 0.9
  phi_2 <- 0.95
  out <- est_initial_V1_V2(fake$PATHS, cfg, omega_1 = omega_1, omega_2 = omega_2,
                           phi_1 = phi_1, phi_2 = phi_2,
                           t_lag = t_lag, cv = 0.1, verbose = FALSE)

  age1 <- as.numeric(t0 - d_r1) - t_lag
  age2 <- as.numeric(t0 - d_r2) - t_lag
  # r2 (0.8e6) <= phi_1 * r1 (0.9e6), so every second dose finds an effective
  # first-dose recipient
  expected_V1 <- (phi_1 * r1_doses - phi_2 * r2_doses) * exp(-omega_1 * age1) / N
  expected_V2 <- phi_2 * r2_doses * exp(-omega_2 * age2) / N

  b1 <- out$parameters_location$prop_V1_initial$location$KEN$parameters
  b2 <- out$parameters_location$prop_V2_initial$location$KEN$parameters
  expect_equal(b1$shape1 / (b1$shape1 + b1$shape2), expected_V1, tolerance = 1e-6)
  expect_equal(b2$shape1 / (b2$shape1 + b2$shape2), expected_V2, tolerance = 1e-6)
})

# -----------------------------------------------------------------------------
# 3. Single-dose Euvichol-S: all doses → V1, V2 == 0 (falls to fallback Beta)
# -----------------------------------------------------------------------------
test_that("Euvichol-S single-dose campaign contributes to V1 only", {
  N <- 1e7
  doses <- 5e5
  t0 <- as.Date("2025-01-01")
  d <- as.Date("2024-06-01")

  rows <- data.frame(
    country = "Ethiopia",
    req_id = c("R1", "R1"),
    event_date = c(d, d),
    event_type = c("Delivery", "Round"),
    round_id = c(NA, "C01-R01"),
    doses = c(doses, doses),
    vaccine = c("EuvicholS", "EuvicholS"),
    stringsAsFactors = FALSE
  )
  fake <- make_fake_gtfcc(rows)
  cfg <- make_fake_config("ETH", N, t0)

  out <- est_initial_V1_V2(fake$PATHS, cfg, omega_1 = 1e-3, omega_2 = 5e-4,
                           t_lag = 14, cv = 0.1, verbose = FALSE)

  # V2 should be the fallback prior (Beta(0.5, 99.5)) since no V2 doses
  b2 <- out$parameters_location$prop_V2_initial$location$ETH$parameters
  expect_equal(b2$shape1, 0.5)
  expect_equal(b2$shape2, 99.5)
  # V1 should be data-derived (not the fallback)
  b1 <- out$parameters_location$prop_V1_initial$location$ETH$parameters
  expect_true(b1$shape1 != 0.5 || b1$shape2 != 49.5)
})

# -----------------------------------------------------------------------------
# 4. Coverage ceiling: V1 + V2 capped at vacc_ceiling_frac * N
# -----------------------------------------------------------------------------
test_that("coverage ceiling caps V1+V2 proportion below vacc_ceiling_frac", {
  # Use a tiny population so the computed V1+V2 exceeds the 60% ceiling
  N <- 1e5
  r1_doses <- 8e4
  r2_doses <- 5e4
  t0 <- as.Date("2025-01-01")
  d_r1 <- as.Date("2024-11-01")
  d_r2 <- as.Date("2024-12-01")

  rows <- data.frame(
    country = "Mozambique",
    req_id = c("R1", "R1", "R1"),
    event_date = c(d_r1, d_r1, d_r2),
    event_type = c("Delivery", "Round", "Round"),
    round_id = c(NA, "C01-R01", "C01-R02"),
    doses = c(r1_doses, r1_doses, r2_doses),
    vaccine = c("Shanchol", "Shanchol", "Shanchol"),
    stringsAsFactors = FALSE
  )
  fake <- make_fake_gtfcc(rows)
  cfg <- make_fake_config("MOZ", N, t0)

  out <- est_initial_V1_V2(fake$PATHS, cfg, omega_1 = 1e-4, omega_2 = 5e-5,
                           t_lag = 14, cv = 0.1,
                           vacc_ceiling_frac = 0.6, verbose = FALSE)
  b1 <- out$parameters_location$prop_V1_initial$location$MOZ$parameters
  b2 <- out$parameters_location$prop_V2_initial$location$MOZ$parameters
  mean_V1 <- b1$shape1 / (b1$shape1 + b1$shape2)
  mean_V2 <- b2$shape1 / (b2$shape1 + b2$shape2)
  expect_lte(mean_V1 + mean_V2, 0.6 + 1e-9)
})

# -----------------------------------------------------------------------------
# 5. Fallback: country not in CSV → returns the specified fallback Beta
# -----------------------------------------------------------------------------
test_that("countries absent from the CSV receive the fallback Beta priors", {
  # Put a row for a different country; the config's country has no data
  rows <- data.frame(
    country = "Kenya", req_id = "R1",
    event_date = as.Date("2024-01-01"),
    event_type = "Round", round_id = "C01-R01",
    doses = 1e6, vaccine = "Shanchol",
    stringsAsFactors = FALSE
  )
  fake <- make_fake_gtfcc(rows)
  cfg <- make_fake_config("GMB", 2e6, as.Date("2025-01-01"))

  out <- est_initial_V1_V2(fake$PATHS, cfg, verbose = FALSE,
                           fallback_shape1_V1 = 0.5, fallback_shape2_V1 = 49.5,
                           fallback_shape1_V2 = 0.5, fallback_shape2_V2 = 99.5)
  b1 <- out$parameters_location$prop_V1_initial$location$GMB$parameters
  b2 <- out$parameters_location$prop_V2_initial$location$GMB$parameters
  expect_equal(b1$shape1, 0.5); expect_equal(b1$shape2, 49.5)
  expect_equal(b2$shape1, 0.5); expect_equal(b2$shape2, 99.5)
})

# -----------------------------------------------------------------------------
# 6. Regression (v0.99.11): V1/V2 are EFFECTIVE-immune compartments in the
#    engine (V1 += phi_1 * nu_1), so the IC must scale doses by phi. The old
#    implementation counted raw doses and overstated V by 1/phi.
# -----------------------------------------------------------------------------
test_that("est_initial_V1_V2 scales doses by phi_1 / phi_2 (effective coverage)", {
  N <- 1e7
  t0 <- as.Date("2025-01-01")
  rows <- data.frame(
    country = "Angola", req_id = "R1",
    event_date = as.Date("2024-06-01"), event_type = "Round",
    round_id = "C01-R01", doses = 1e6, vaccine = "Shanchol",
    stringsAsFactors = FALSE
  )
  fake <- make_fake_gtfcc(rows)
  cfg <- make_fake_config("AGO", N, t0)
  mean_V1 <- function(phi_1) {
    out <- est_initial_V1_V2(fake$PATHS, cfg, omega_1 = 0, omega_2 = 0,
                             t_lag = 0, cv = 0.1, phi_1 = phi_1, phi_2 = 0.9,
                             verbose = FALSE)
    b <- out$parameters_location$prop_V1_initial$location$AGO$parameters
    b$shape1 / (b$shape1 + b$shape2)
  }
  expect_equal(mean_V1(1), 0.1, tolerance = 1e-8)
  expect_equal(mean_V1(0.5), 0.05, tolerance = 1e-8)

  # Defaults are the priors_default Beta means, not 1
  pg <- MOSAIC::priors_default$parameters_global
  phi_1_mean <- pg$phi_1$parameters$shape1 /
    (pg$phi_1$parameters$shape1 + pg$phi_1$parameters$shape2)
  out <- est_initial_V1_V2(fake$PATHS, cfg, omega_1 = 0, omega_2 = 0,
                           t_lag = 0, cv = 0.1, verbose = FALSE)
  b <- out$parameters_location$prop_V1_initial$location$AGO$parameters
  expect_equal(b$shape1 / (b$shape1 + b$shape2), phi_1_mean * 0.1, tolerance = 1e-8)

  expect_error(est_initial_V1_V2(fake$PATHS, cfg, phi_1 = 1.5, verbose = FALSE),
               "phi_1")
})

# -----------------------------------------------------------------------------
# 7. Only pre-t0 campaigns contribute (post-t0 doses must be ignored)
# -----------------------------------------------------------------------------
test_that("campaigns on or after date_start are excluded", {
  N <- 1e7
  t0 <- as.Date("2024-01-01")
  rows <- data.frame(
    country = c("Angola", "Angola"),
    req_id = c("R1", "R2"),
    event_date = c(as.Date("2025-06-01"),   # AFTER t0 — must be ignored
                   as.Date("2023-06-01")),  # BEFORE t0 — should contribute
    event_type = c("Round", "Round"),
    round_id = c("C01-R01", "C01-R01"),
    doses = c(5e6, 1e5),
    vaccine = c("Shanchol", "Shanchol"),
    stringsAsFactors = FALSE
  )
  fake <- make_fake_gtfcc(rows)
  cfg <- make_fake_config("AGO", N, t0)
  out <- est_initial_V1_V2(fake$PATHS, cfg,
                           omega_1 = 0, omega_2 = 0, t_lag = 0, cv = 0.1,
                           phi_1 = 1, phi_2 = 1, verbose = FALSE)
  b1 <- out$parameters_location$prop_V1_initial$location$AGO$parameters
  mean_V1 <- b1$shape1 / (b1$shape1 + b1$shape2)
  # Only the 1e5-dose pre-t0 campaign should contribute → prop ≈ 1e5/1e7 = 0.01
  expect_equal(mean_V1, 0.01, tolerance = 1e-6)
})
