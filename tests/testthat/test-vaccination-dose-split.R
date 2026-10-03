# First- and second-dose OCV rates (MOSAIC v0.103.0): process_GTFCC_vaccination_data()
# attributes each request's doses to rounds from its GTFCC Round events,
# est_vaccination_rate() splits the daily series into nu_1 / nu_2, and the
# config builder reads them back through .vacc_nu_jt().

.vsplit_campaigns <- function(iso, dates, doses, round_sequence = NA_character_) {
     data.frame(year = as.integer(substr(dates, 1, 4)), country = iso,
                request_number = seq_along(dates), status = "Approved",
                context = "Outbreak response", decision_date = dates,
                doses_requested = doses, doses_approved = doses, doses_shipped = doses,
                campaign_date = dates, id = seq_along(dates), iso_code = iso, delay = 0,
                round_sequence = round_sequence, stringsAsFactors = FALSE)
}

# Run est_vaccination_rate() on a synthetic campaign file and return the
# nu, nu_1 and nu_2 parameter files plus the redistributed data file.
.vsplit_run <- function(campaigns, date_start = "2020-01-01", date_stop = "2020-06-30",
                        max_rate_per_day = 20000, data_source = "GTFCC") {
     tmp <- withr::local_tempdir(.local_envir = parent.frame())
     file <- c(GTFCC = "data_vaccinations_GTFCC.csv", WHO = "data_vaccinations_WHO.csv")[[data_source]]
     utils::write.csv(campaigns, file.path(tmp, file), row.names = FALSE)
     utils::write.csv(data.frame(iso_code = MOSAIC::iso_codes_mosaic, year = 2023, population = 1e7),
                      file.path(tmp, "demographics_africa_2000_2023.csv"), row.names = FALSE)
     suppressMessages(est_vaccination_rate(list(MODEL_INPUT = tmp, DATA_DEMOGRAPHICS = tmp),
                                           date_start = date_start, date_stop = date_stop,
                                           max_rate_per_day = max_rate_per_day,
                                           data_source = data_source))
     files <- MOSAIC:::.vacc_nu_files(tmp, data_source)
     read <- function(f) { d <- utils::read.csv(f, stringsAsFactors = FALSE); d$t <- as.Date(d$t); d }
     list(nu = read(files$nu), nu_1 = read(files$nu_1), nu_2 = read(files$nu_2),
          redistributed = utils::read.csv(file.path(tmp, sprintf("data_vaccinations_%s_redistributed.csv", data_source)),
                                          stringsAsFactors = FALSE),
          dir = tmp)
}

.vsplit_series <- function(p, iso) {
     sel <- p$nu$j == iso
     data.frame(t = p$nu$t[sel], nu = p$nu$parameter_value[sel],
                nu_1 = p$nu_1$parameter_value[p$nu_1$j == iso],
                nu_2 = p$nu_2$parameter_value[p$nu_2$j == iso])
}

test_that("a two-round campaign sends its second round to nu_2, after its first", {
     # 200,000 doses at 20,000/day: days 1-5 first round, days 6-10 second round
     p <- .vsplit_run(.vsplit_campaigns("MOZ", "2020-02-01", 2e5, "1:100000;2:100000"))
     s <- .vsplit_series(p, "MOZ")
     days <- as.Date("2020-02-01") + 0:9
     expect_equal(s$nu_1[match(days, s$t)], c(rep(20000, 5), rep(0, 5)))
     expect_equal(s$nu_2[match(days, s$t)], c(rep(0, 5), rep(20000, 5)))
     expect_equal(sum(s$nu_1), 1e5)
     expect_equal(sum(s$nu_2), 1e5)
     expect_equal(min(s$t[s$nu_2 > 0]), max(s$t[s$nu_1 > 0]) + 1)
     # the parameter files carry their own variable names
     expect_equal(unique(p$nu_1$variable_name), "nu_1")
     expect_equal(unique(p$nu_2$variable_name), "nu_2")
     expect_equal(unique(p$nu$variable_name), "nu")
})

test_that("round shares split shipped doses in whole doses, inside a day where needed", {
     # 150,000 doses, administered rounds 2:1 -> 100,000 first doses, 50,000 second
     p <- .vsplit_run(.vsplit_campaigns("ETH", "2020-03-01", 1.5e5, "1:400000;2:200000"))
     s <- .vsplit_series(p, "ETH")
     days <- as.Date("2020-03-01") + 0:7
     expect_equal(s$nu_1[match(days, s$t)], c(rep(20000, 5), 0, 0, 0))
     expect_equal(s$nu_2[match(days, s$t)], c(rep(0, 5), 20000, 20000, 10000))
     # equal shares put the boundary at 75,000, inside day 4 (60,000-80,000)
     p <- .vsplit_run(.vsplit_campaigns("ETH", "2020-03-01", 1.5e5, "1:1;2:1"))
     s <- .vsplit_series(p, "ETH")
     day4 <- s$t == as.Date("2020-03-04")
     expect_equal(c(s$nu_1[day4], s$nu_2[day4]), c(15000, 5000))
     expect_equal(c(sum(s$nu_1), sum(s$nu_2)), c(75000, 75000))
})

test_that("multi-campaign sequences take consecutive stretches in campaign order", {
     # R1 / R2 / R1 / R2 blocks of 40,000 doses each: 2 days apiece
     p <- .vsplit_run(.vsplit_campaigns("UGA", "2020-04-01", 1.6e5, "1:1;2:1;1:1;2:1"))
     s <- .vsplit_series(p, "UGA")
     days <- as.Date("2020-04-01") + 0:7
     expect_equal(s$nu_2[match(days, s$t)] > 0, rep(c(FALSE, FALSE, TRUE, TRUE), 2))
})

test_that("doses with unknown round count as first doses", {
     # NA round_sequence (GTFCC request without Round events)
     p <- .vsplit_run(.vsplit_campaigns(c("NGA", "NGA"), c("2020-01-10", "2020-03-01"),
                                        c(90000, 50000), c(NA, "1:1;2:1")))
     s <- .vsplit_series(p, "NGA")
     jan <- s$t < as.Date("2020-02-01")
     expect_equal(sum(s$nu_1[jan]), 90000)
     expect_equal(sum(s$nu_2[jan]), 0)
     expect_equal(sum(s$nu_2), 25000)
     # a source with no round columns at all (the WHO ICG table)
     who <- .vsplit_campaigns("NGA", "2020-01-10", 90000)
     who$round_sequence <- NULL
     p <- .vsplit_run(who, data_source = "WHO")
     expect_true(all(p$nu_2$parameter_value == 0))
     expect_equal(p$nu_1$parameter_value, p$nu$parameter_value)
})

test_that("a one-round (2023-style) campaign leaves nu_2 at zero", {
     p <- .vsplit_run(.vsplit_campaigns("COD", "2020-05-01", 2.5e5, "1:1"))
     expect_true(all(p$nu_2$parameter_value == 0))
     expect_equal(p$nu_1$parameter_value, p$nu$parameter_value)
     expect_equal(sum(p$nu_1$parameter_value), 2.5e5)
})

test_that("first plus second doses equal the doses distributed on every location-day", {
     # overlapping campaigns in one location, mixed round structure, odd sizes
     camp <- .vsplit_campaigns(c("ZMB", "ZMB", "ZMB", "ZWE", "SSD"),
                               c("2020-01-05", "2020-01-12", "2020-02-20", "2020-01-05", "2020-03-03"),
                               c(123457, 98765, 300001, 54321, 77777),
                               c("1:3;2:2", NA, "1:147600;2:123100;1:1100000;2:1200000", "2:1", "1:1"))
     p <- .vsplit_run(camp)
     expect_identical(p$nu$j, p$nu_1$j)
     expect_identical(p$nu$t, p$nu_1$t)
     expect_identical(p$nu$t, p$nu_2$t)
     expect_equal(p$nu_1$parameter_value + p$nu_2$parameter_value, p$nu$parameter_value, tolerance = 0)
     expect_true(all(p$nu_1$parameter_value %% 1 == 0 & p$nu_2$parameter_value %% 1 == 0))
     expect_equal(sum(p$nu$parameter_value), sum(camp$doses_shipped))
     # the redistributed data file carries the same split
     r <- p$redistributed
     expect_equal(r$doses_distributed_dose1 + r$doses_distributed_dose2, r$doses_distributed, tolerance = 0)
     expect_equal(names(r)[1:6], c("country", "iso_code", "date", "doses_distributed",
                                   "doses_distributed_cumulative", "prop_vaccinated"))
     # a second-round-only request (a separate GTFCC decision for round 2) is all nu_2
     zwe <- .vsplit_series(p, "ZWE")
     expect_equal(c(sum(zwe$nu_1), sum(zwe$nu_2)), c(0, 54321))
})

test_that(".vacc_round_sequence reads the Round events of a request", {
     rs <- MOSAIC:::.vacc_round_sequence
     # weights are administered doses, blocks in campaign then round order
     expect_equal(rs(c("C01-R01", "C01-R02", "C02-R01", "C02-R02"), c(357200, 332900, 562000, 693900)),
                  list(sequence = "1:357200;2:332900;1:562000;2:693900", basis = "rounds"))
     # R02 listed (and dated) before R01: campaign/round order wins
     expect_equal(rs(c("C01-R02", "C01-R01"), c(400, 500))$sequence, "1:500;2:400")
     # a single dose throughout: 100% whatever the dose counts say
     expect_equal(rs(c("C01-R01", "C02-R01"), c(NA, 1e6)), list(sequence = "1:1", basis = "rounds"))
     expect_equal(rs("C01-R02", NA)$sequence, "2:1")
     # consecutive rounds of one dose merge
     expect_equal(rs(c("C01-R01", "C01-R02", "C02-R01", "C03-R01"), c(10, 9, 5, 6))$sequence, "1:10;2:9;1:11")
     # an unreported round carries the mean reported round of the request
     expect_equal(rs(c("C01-R01", "C01-R02"), c(1200000, NA)),
                  list(sequence = "1:1200000;2:1200000", basis = "rounds_imputed"))
     # none reported: equal weights
     expect_equal(rs(c("C01-R01", "C01-R02"), c(NA, NA)), list(sequence = "1:1;2:1", basis = "rounds_imputed"))
     # a campaign-round listed twice counts once, with its reported doses
     expect_equal(rs(c("C01-R01", "C01-R01", "C01-R02", "C01-R02"), c(NA, 1300000, 1500000, NA)),
                  list(sequence = "1:1300000;2:1500000", basis = "rounds"))
     # rounds 3+ are second doses
     expect_equal(rs(c("C01-R01", "C01-R03"), c(5, 4))$sequence, "1:5;2:4")
     # no parseable round id
     expect_equal(rs(character(0), numeric(0)), list(sequence = NA_character_, basis = "unknown"))
     expect_equal(rs("R01", 100), list(sequence = NA_character_, basis = "unknown"))
})

test_that("a malformed round_sequence stops est_vaccination_rate()", {
     expect_error(.vsplit_run(.vsplit_campaigns("MOZ", "2020-02-01", 1e5, "1:1;3:1")), "Malformed round_sequence")
     expect_error(.vsplit_run(.vsplit_campaigns("MOZ", "2020-02-01", 1e5, "1:0;2:0")), "Malformed round_sequence")
     expect_error(.vsplit_run(.vsplit_campaigns("MOZ", "2020-02-01", 1e5, "first")), "Malformed round_sequence")
})

test_that("process_GTFCC_vaccination_data() writes the round columns from the raw log", {
     root <- withr::local_tempdir()
     gtfcc_dir <- file.path(root, "ees-cholera-mapping", "data", "cholera", "epicentre", "gtfcc")
     dir.create(gtfcc_dir, recursive = TRUE)
     ev <- function(country, req, date, type, round_id = "", doses = NA, vaccine = "", via = "") {
          data.frame(country = country, raw_tooltip = "", raw_content = "", req_id = req,
                     req_year = as.integer(substr(req, 1, 4)), event_date = date, event_type = type,
                     round_id = round_id, doses = doses, vaccine = vaccine, duration_days = NA,
                     via_tool = via, by_org = "", stringsAsFactors = FALSE)
     }
     raw <- rbind(
          ev("Mozambique", "2019-I05-D01", "2019-04-15", "Decision", doses = 1670400, via = "ICG"),
          ev("Mozambique", "2019-I05-D01", "2019-04-24", "Delivery", doses = 835200, vaccine = "Euvichol+"),
          ev("Mozambique", "2019-I05-D01", "2019-05-28", "Round", "C01-R01", 786900),
          ev("Mozambique", "2019-I05-D01", "2019-09-25", "Delivery", doses = 835200, vaccine = "Euvichol+"),
          ev("Mozambique", "2019-I05-D01", "2019-10-30", "Round", "C01-R02", 795800),
          ev("Kenya", "2023-I10-D01", "2023-06-20", "Decision", doses = 1578000, via = "ICG"),
          ev("Kenya", "2023-I10-D01", "2023-06-29", "Delivery", doses = 1578000, vaccine = "Euvichol+"),
          ev("Kenya", "2023-I10-D01", "2023-08-03", "Round", "C01-R01", 1500000),
          ev("Cameroon", "2022-I10-D01", "2022-07-07", "Decision", doses = 4300000, via = "ICG"),
          ev("Cameroon", "2022-I10-D01", "2022-07-14", "Delivery", doses = 2100000, vaccine = "Euvichol+"))
     utils::write.csv(raw, file.path(gtfcc_dir, "cholera_vacc_requests.csv"), row.names = FALSE)
     out_dir <- withr::local_tempdir()
     out <- suppressMessages(process_GTFCC_vaccination_data(list(ROOT = root, MODEL_INPUT = out_dir)))
     written <- utils::read.csv(file.path(out_dir, "data_vaccinations_GTFCC.csv"), stringsAsFactors = FALSE)
     expect_equal(utils::tail(names(written), 3), c("req_id", "round_sequence", "round_basis"))
     got <- written[order(written$req_id), c("req_id", "iso_code", "doses_shipped", "round_sequence", "round_basis")]
     expect_equal(got$req_id, c("2019-I05-D01", "2022-I10-D01", "2023-I10-D01"))
     expect_equal(got$doses_shipped, c(1670400, 2100000, 1578000))
     expect_equal(got$round_sequence, c("1:786900;2:795800", NA, "1:1"))
     expect_equal(got$round_basis, c("rounds", "unknown", "rounds"))
})

test_that("combine_vaccination_data() carries the round columns; WHO-only rows are unknown", {
     tmp <- withr::local_tempdir()
     gtfcc <- .vsplit_campaigns(c("COD", "MOZ"), c("2019-04-24", "2019-07-07"), c(1670400, 849500),
                                c("1:786900;2:795800", "2:1"))
     gtfcc$request_number <- c(201905, 201901)
     gtfcc$req_id <- c("2019-I05-D01", "2019-I01-D02")
     gtfcc$round_basis <- "rounds"
     who <- .vsplit_campaigns(c("COD", "MWI"), c("2019-05-28", "2018-04-15"), c(835190, 500600))
     who$round_sequence <- NULL
     who$request_number <- c(20195, 20182)
     utils::write.csv(gtfcc, file.path(tmp, "data_vaccinations_GTFCC.csv"), row.names = FALSE)
     utils::write.csv(who, file.path(tmp, "data_vaccinations_WHO.csv"), row.names = FALSE)
     utils::capture.output(out <- suppressMessages(combine_vaccination_data(list(MODEL_INPUT = tmp))))
     expect_equal(names(out)[ncol(out)], "match_confidence")
     expect_true(all(c("req_id", "round_sequence", "round_basis") %in% names(out)))
     cod <- out[out$iso_code == "COD", ]
     expect_equal(nrow(cod), 1L)
     expect_equal(cod$round_sequence, "1:786900;2:795800")
     mwi <- out[out$iso_code == "MWI", ]
     expect_equal(mwi$source, "WHO_only")
     expect_true(is.na(mwi$round_sequence))
     expect_equal(mwi$round_basis, "unknown")
     expect_equal(out$round_sequence[out$iso_code == "MOZ"], "2:1")
})

test_that(".vacc_nu_jt() returns aligned matrices and refuses out-of-step files", {
     p <- .vsplit_run(.vsplit_campaigns(c("MOZ", "ETH"), c("2020-02-01", "2020-02-10"), c(2e5, 1e5),
                                        c("1:1;2:1", NA)))
     dates <- seq(as.Date("2020-02-01"), as.Date("2020-02-29"), by = "day")
     loc <- c("MOZ", "ETH", "KEN")
     m <- MOSAIC:::.vacc_nu_jt(p$dir, "GTFCC", location_name = loc, dates = dates)
     expect_equal(dim(m$nu_1_jt), c(3L, length(dates)))
     expect_equal(rownames(m$nu_2_jt), loc)
     expect_equal(colnames(m$nu_2_jt), as.character(dates))
     expect_equal(m$nu_1_jt + m$nu_2_jt, m$nu_jt)
     expect_equal(sum(m$nu_2_jt["MOZ", ]), 1e5)
     expect_equal(sum(m$nu_2_jt["ETH", ]), 0)
     expect_equal(sum(m$nu_1_jt["ETH", ]), 1e5)
     expect_true(all(m$nu_jt["KEN", ] == 0))

     # a stale nu_2 file (out of step with nu) is refused
     f <- MOSAIC:::.vacc_nu_files(p$dir, "GTFCC")
     n2 <- utils::read.csv(f$nu_2, stringsAsFactors = FALSE)
     n2$parameter_value[n2$j == "MOZ"] <- 0
     utils::write.csv(n2, f$nu_2, row.names = FALSE)
     expect_error(MOSAIC:::.vacc_nu_jt(p$dir, "GTFCC", loc, dates), "out of step")
     # a window the files do not cover is refused
     expect_error(MOSAIC:::.vacc_nu_jt(p$dir, "GTFCC", loc, seq(as.Date("2020-06-01"), as.Date("2020-07-31"), by = "day")),
                  "does not cover")
     # missing dose files are refused, not silently replaced by nu
     file.remove(f$nu_1)
     expect_error(MOSAIC:::.vacc_nu_jt(p$dir, "GTFCC", loc, dates), "Missing vaccination rate file")
})
