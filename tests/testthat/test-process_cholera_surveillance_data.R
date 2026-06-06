# Tests for the 4-way (WHO > JHU > AI > SUPP) surveillance merge.

# Build a minimal set of per-source processed CSVs in a temp PATHS layout.
# One country (MOZ), four consecutive ISO weeks:
#   wk1: WHO only            -> WHO
#   wk2: WHO + JHU           -> WHO (priority)
#   wk3: JHU + AI            -> JHU (priority over AI)
#   wk4: AI only (gap)       -> AI (gap-fill)
make_fixture <- function() {
  tmp <- tempfile("survtest"); dir.create(tmp)
  P <- list(
    DATA_WHO_WEEKLY     = file.path(tmp, "who"),
    DATA_JHU_WEEKLY     = file.path(tmp, "jhu"),
    DATA_SUPP_WEEKLY    = file.path(tmp, "supp"),
    DATA_AI_WEEKLY      = file.path(tmp, "ai"),
    DATA_CHOLERA_WEEKLY = file.path(tmp, "cw"),
    DATA_CHOLERA_DAILY  = file.path(tmp, "cd")
  )
  for (d in P) dir.create(d, showWarnings = FALSE, recursive = TRUE)

  ws <- seq(as.Date("2020-06-01"), by = "week", length.out = 4)  # Mondays
  base <- data.frame(
    iso_code   = "MOZ",
    country    = "Mozambique",
    year       = as.integer(format(ws, "%Y")),
    week       = as.integer(format(ws, "%V")),
    date_start = as.character(ws),
    date_stop  = as.character(ws + 6),
    month      = as.integer(format(ws, "%m")),
    stringsAsFactors = FALSE
  )
  who <- cbind(base[c(1, 2), ], cases = c(100, 200), deaths = c(2, 4))
  jhu <- cbind(base[c(2, 3), ], cases = c(999, 300), deaths = c(9, 6))
  ai  <- cbind(base[c(3, 4), ], cases = c(888, 50), deaths = c(8, 1),
               confidence_weight = c(0.5, 0.5),
               disaggregation_method = c("fourier_country_k2", "observed"))

  write.csv(who, file.path(P$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv"), row.names = FALSE)
  write.csv(jhu, file.path(P$DATA_JHU_WEEKLY, "cholera_country_weekly_processed.csv"), row.names = FALSE)
  write.csv(ai,  file.path(P$DATA_AI_WEEKLY,  "cholera_country_weekly_processed.csv"), row.names = FALSE)
  list(P = P, weeks = base$week)
}

read_combined <- function(P) {
  utils::read.csv(file.path(P$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv"),
                  stringsAsFactors = FALSE)
}

test_that("include_ai = FALSE merges 3 sources; confidence metadata defaults present", {
  fx <- make_fixture(); P <- fx$P
  suppressMessages(process_cholera_surveillance_data(P, include_ai = FALSE))
  out <- read_combined(P)
  obs <- out[!is.na(out$cases), ]

  # Columns are always present (stable schema); trusted observations are 1.0.
  expect_true("confidence_weight" %in% names(out))
  expect_true("disaggregation_method" %in% names(out))
  expect_true(all(obs$confidence_weight == 1.0))              # WHO/JHU/SUPP trusted
  expect_true(all(is.na(obs$disaggregation_method)))          # no AI -> NA method
  expect_true(all(is.na(out$confidence_weight[is.na(out$cases)])))  # empty cells NA

  # wk2 WHO+JHU -> WHO; wk3 JHU only (AI excluded) -> JHU; wk4 has no data -> NA
  expect_equal(obs$source[obs$week == fx$weeks[2]], "WHO")
  expect_equal(obs$source[obs$week == fx$weeks[3]], "JHU")
  expect_equal(obs$cases[obs$week == fx$weeks[2]], 200)  # WHO value, not JHU's 999
  expect_true(all(is.na(out$cases[out$week == fx$weeks[4]])))  # wk4 AI not included
})

test_that("include_ai = TRUE adds columns, respects priority, fills gaps", {
  fx <- make_fixture(); P <- fx$P
  # Fix #3 interlock: a loud warning fires because confidence_weight is not yet consumed.
  expect_warning(
    suppressMessages(process_cholera_surveillance_data(P, include_ai = TRUE)),
    "do NOT yet consume confidence_weight"
  )
  out <- read_combined(P)
  obs <- out[!is.na(out$cases), ]

  expect_true(all(c("confidence_weight", "disaggregation_method") %in% names(out)))

  # priority: wk2 WHO beats JHU; wk3 JHU beats AI
  expect_equal(obs$source[obs$week == fx$weeks[2]], "WHO")
  expect_equal(obs$source[obs$week == fx$weeks[3]], "JHU")
  expect_equal(obs$cases[obs$week == fx$weeks[3]], 300)  # JHU value, not AI's 888

  # gap-fill: wk4 had no WHO/JHU/SUPP -> AI kept
  expect_equal(obs$source[obs$week == fx$weeks[4]], "AI")
  expect_equal(obs$cases[obs$week == fx$weeks[4]], 50)

  # confidence_weight: trusted WHO row = 1.0; AI gap-fill row keeps its 0.5
  expect_equal(obs$confidence_weight[obs$week == fx$weeks[2]], 1.0)
  expect_true(is.na(obs$disaggregation_method[obs$week == fx$weeks[2]]))  # WHO -> NA method
  expect_equal(obs$confidence_weight[obs$week == fx$weeks[4]], 0.5)
  expect_equal(obs$disaggregation_method[obs$week == fx$weeks[4]], "observed")
  # empty grid cells stay NA = "no observation"
  expect_true(all(is.na(out$confidence_weight[is.na(out$cases)])))

  # Fix #2: AI rows are EXCLUDED from the daily (calibration) file. Trusted weekly
  # cases sum to 100+200+300 = 600 (wk2 WHO, wk3 JHU); the AI gap-fill wk4 (50) must
  # not appear in the daily downscale.
  daily <- utils::read.csv(file.path(P$DATA_CHOLERA_DAILY, "cholera_surveillance_daily_combined.csv"),
                           stringsAsFactors = FALSE)
  expect_equal(sum(daily$cases, na.rm = TRUE), 600)
})

test_that("include_ai = TRUE warns and falls back when AI file is missing", {
  fx <- make_fixture(); P <- fx$P
  file.remove(file.path(P$DATA_AI_WEEKLY, "cholera_country_weekly_processed.csv"))
  expect_warning(
    suppressMessages(process_cholera_surveillance_data(P, include_ai = TRUE)),
    "no usable AI data"
  )
  out <- read_combined(P)
  # falls back to 3-source: wk4 (AI-only) has no observation
  expect_true(all(is.na(out$cases[out$week == fx$weeks[4]])))
})
