# Regression tests: combine_vaccination_data() matches WHO shipments to GTFCC
# campaigns by ICG request. GTFCC rows are per-request totals (all deliveries
# summed by process_GTFCC_vaccination_data()); WHO rows are per shipment.

.vacc_req_rows <- function(req, iso, dates, doses, decision = dates) {
     data.frame(year = as.integer(substr(dates, 1, 4)), country = iso,
                request_number = req, status = "Approved", context = "Outbreak response",
                decision_date = decision, doses_requested = doses, doses_approved = doses,
                doses_shipped = doses, campaign_date = dates, id = seq_along(req),
                iso_code = iso, delay = 0, stringsAsFactors = FALSE)
}

.run_combine <- function(gtfcc, who) {
     tmp <- withr::local_tempdir(.local_envir = parent.frame())
     utils::write.csv(gtfcc, file.path(tmp, "data_vaccinations_GTFCC.csv"), row.names = FALSE)
     utils::write.csv(who,   file.path(tmp, "data_vaccinations_WHO.csv"),   row.names = FALSE)
     out <- NULL
     utils::capture.output(out <- suppressMessages(combine_vaccination_data(list(MODEL_INPUT = tmp))))
     out
}

test_that(".vacc_request_key maps WHO and GTFCC request numbers to one key", {
     expect_equal(.vacc_request_key(c(20174, 201704, 201919, "202317")),
                  c("2017:4", "2017:4", "2019:19", "2023:17"))
     expect_true(all(is.na(.vacc_request_key(c(NA, 11, "2017G01")))))
})

test_that("a second shipment of a GTFCC-recorded request is not added again (MOZ 2017-I04)", {
     # GTFCC 2017-I04-D01: 709.1K doses (354.6K + 354.5K deliveries);
     # WHO request 20174: two shipment rows, 329,630 and 354,550.
     gtfcc <- .vacc_req_rows(201704, "MOZ", "2017-04-28", 709100, decision = "2017-04-05")
     who <- .vacc_req_rows(c(20174, 20174), "MOZ", c("2017-05-19", "2017-05-08"),
                           c(329630, 354550), decision = "2017-04-05")
     out <- .run_combine(gtfcc, who)
     expect_equal(nrow(out), 1L)
     expect_equal(out$source, "GTFCC_WHO_matched")
     expect_equal(sum(out$doses_shipped), 709100)
})

test_that("request identity beats date proximity to another request (COD 2019)", {
     # WHO 2nd round of 20195 (2019-10-30) is 15 days from GTFCC 201914 but
     # belongs to GTFCC 201905; WHO 201914 is 62 days from its GTFCC row.
     gtfcc <- .vacc_req_rows(c(201905, 201914), "COD", c("2019-04-24", "2019-11-14"),
                             c(1670400, 961700), decision = c("2019-04-15", "2019-10-28"))
     who <- .vacc_req_rows(c(20195, 20195, 201914), "COD",
                           c("2019-05-28", "2019-10-30", "2020-01-15"),
                           c(835190, 835190, 961670),
                           decision = c("2019-04-15", "2019-04-15", "2019-10-28"))
     out <- .run_combine(gtfcc, who)
     expect_false(any(out$source == "WHO_only"))
     expect_equal(sum(out$doses_shipped), 1670400 + 961700)
})

test_that("a renumbered request is matched on the ICG decision date (MOZ 2020)", {
     gtfcc <- .vacc_req_rows(202002, "MOZ", "2020-03-27", 733500, decision = "2020-03-12")
     who <- .vacc_req_rows(20203, "MOZ", "2020-09-21", 733500, decision = "2020-03-12")
     out <- .run_combine(gtfcc, who)
     expect_equal(nrow(out), 1L)
     expect_equal(out$match_confidence, "low")
})

test_that("extra WHO doses beyond the GTFCC request total are kept as WHO_only", {
     gtfcc <- .vacc_req_rows(201704, "MOZ", "2017-04-28", 709100, decision = "2017-04-05")
     who <- .vacc_req_rows(c(20174, 20174), "MOZ", c("2017-05-01", "2017-11-01"),
                           c(700000, 700000), decision = "2017-04-05")
     out <- .run_combine(gtfcc, who)
     who_only <- out[out$source == "WHO_only", ]
     expect_equal(nrow(who_only), 1L)
     expect_equal(who_only$doses_shipped, 700000)
})

test_that("a WHO-only request listed twice is not double-counted (MWI 20182)", {
     gtfcc <- .vacc_req_rows(201801, "ZMB", "2018-01-10", 2e6, decision = "2018-01-05")
     who <- .vacc_req_rows(c(20182, 20182), "MWI", c("2018-04-15", "2018-04-17"),
                           c(500600, 500600), decision = "2018-03-02")
     out <- .run_combine(gtfcc, who)
     mwi <- out[out$iso_code == "MWI", ]
     expect_equal(nrow(mwi), 1L)
     expect_equal(sum(mwi$doses_shipped), 500600)
     expect_equal(as.character(mwi$campaign_date), "2018-04-15")
})

test_that("genuine multi-shipment WHO-only requests within the approved total are kept", {
     gtfcc <- .vacc_req_rows(201801, "ZMB", "2018-01-10", 2e6, decision = "2018-01-05")
     who <- .vacc_req_rows(c(20185, 20185), "MWI", c("2018-06-01", "2018-06-20"),
                           c(300000, 300000), decision = "2018-05-20")
     who$doses_approved <- 600000
     out <- .run_combine(gtfcc, who)
     expect_equal(sum(out$doses_shipped[out$iso_code == "MWI"]), 600000)
})
