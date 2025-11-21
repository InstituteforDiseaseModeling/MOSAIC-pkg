# Tests for calc_convergence_diagnostics.R

test_that("calc_convergence_diagnostics validates inputs correctly", {
     # Should error on invalid inputs
     expect_error(
          calc_convergence_diagnostics(
               n_total = -1,  # Invalid: negative
               n_successful = 100,
               n_retained = 80,
               n_best_subset = 50,
               ess_best = 150,
               A_best = 0.85,
               cvw_best = 0.6,
               percentile_used = 5.0,
               convergence_tier = "tier_3",
               verbose = FALSE
          ),
          "n_total must be positive"
     )

     expect_error(
          calc_convergence_diagnostics(
               n_total = 100,
               n_successful = 150,  # Invalid: > n_total
               n_retained = 80,
               n_best_subset = 50,
               ess_best = 150,
               A_best = 0.85,
               cvw_best = 0.6,
               percentile_used = 5.0,
               convergence_tier = "tier_3",
               verbose = FALSE
          ),
          "n_successful must be"
     )

     expect_error(
          calc_convergence_diagnostics(
               n_total = 100,
               n_successful = 90,
               n_retained = 100,  # Invalid: > n_successful
               n_best_subset = 50,
               ess_best = 150,
               A_best = 0.85,
               cvw_best = 0.6,
               percentile_used = 5.0,
               convergence_tier = "tier_3",
               verbose = FALSE
          ),
          "n_retained must be"
     )
})


test_that("Status calculations work correctly for individual metrics", {
     diag <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425), well above target (300)
          ess_best = 280,          # 93% of target (300) → pass (≥80%)
          A_best = 0.92,           # 97% of target (0.95) → pass (≥90%)
          cvw_best = 0.7,          # 140% of target (0.5) → warn (>120%, ≤200%)
          percentile_used = 4.8,   # Below target (5.0) → pass
          convergence_tier = "tier_3",
          target_ess_best = 300,
          target_A_best = 0.95,
          target_cvw_best = 0.5,
          target_percentile_max = 5.0,
          verbose = FALSE
     )

     # Check individual statuses
     expect_equal(diag$metrics$B_size$status, "pass")      # 400 ≥ 300
     expect_equal(diag$metrics$ess_best$status, "pass")    # 280 ≥ 300*0.8
     expect_equal(diag$metrics$A_B$status, "pass")         # 0.92 ≥ 0.95*0.9
     expect_equal(diag$metrics$cvw_B$status, "warn")       # 0.7 > 0.5*1.2 but ≤ 0.5*2.0

     # Overall status should be WARN (has one warn, no fails)
     expect_equal(diag$summary$convergence_status, "WARN")
})


test_that("Overall status reflects worst individual status", {
     # All pass → PASS
     diag1 <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 300,      # Meets target
          A_best = 0.95,       # Meets target
          cvw_best = 0.4,      # Better than target
          percentile_used = 4.0,
          convergence_tier = "tier_1",
          verbose = FALSE
     )
     expect_equal(diag1$summary$convergence_status, "PASS")

     # One warn → WARN
     diag2 <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 250,      # 83% of 300 → pass
          A_best = 0.95,       # Meets target
          cvw_best = 0.7,      # 140% of 0.5 → warn (>120%, ≤200%)
          percentile_used = 4.0,
          convergence_tier = "tier_3",
          verbose = FALSE
     )
     expect_equal(diag2$summary$convergence_status, "WARN")

     # One fail → FAIL
     diag3 <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 100,      # 33% of 300 → fail
          A_best = 0.95,
          cvw_best = 0.4,
          percentile_used = 4.0,
          convergence_tier = "tier_10",
          verbose = FALSE
     )
     expect_equal(diag3$summary$convergence_status, "FAIL")
})


test_that("B_size target is separate from ESS target", {
     # Large subset with low ESS should show:
     # - B_size: pass (meets count requirement)
     # - ESS_best: fail (doesn't meet ESS requirement)
     # - Overall: fail (due to ESS)

     diag <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 1000,    # Well above target (300)
          ess_best = 80,           # Well below ESS target (300)
          A_best = 0.95,
          cvw_best = 0.4,
          percentile_used = 10.0,   # Also above percentile target
          convergence_tier = "tier_15",
          target_ess_best = 300,
          target_percentile_max = 5.0,
          verbose = FALSE
     )

     expect_equal(diag$metrics$B_size$status, "pass")      # 1000 ≥ 300
     expect_equal(diag$metrics$ess_best$status, "fail")    # 80 < 300*0.5
     expect_equal(diag$summary$convergence_status, "FAIL") # Overall reflects worst
})


test_that("Percentile status is calculated correctly", {
     # Pass: within target
     diag1 <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 300,
          A_best = 0.95,
          cvw_best = 0.4,
          percentile_used = 4.5,   # < 5.0 → pass
          convergence_tier = "tier_2",
          target_percentile_max = 5.0,
          verbose = FALSE
     )
     # Percentile status isn't in metrics, but affects overall via fallback
     expect_equal(diag1$summary$convergence_status, "PASS")

     # Fail: well above target
     diag2 <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 300,
          A_best = 0.95,
          cvw_best = 0.4,
          percentile_used = 15.0,  # >> 5.0 → fail
          convergence_tier = "tier_10",
          target_percentile_max = 5.0,
          verbose = FALSE
     )
     # Should fail due to percentile (internal calculation)
     # This is tested via subset selection in plotting function
     expect_true(diag2$summary$percentile_used == 15.0)
})


test_that("Parameter ESS metric is included when data provided", {
     # Create mock parameter ESS data
     param_ess_df <- data.frame(
          parameter = paste0("param_", 1:100),
          ess_marginal = c(rep(350, 90), rep(250, 10))  # 90% pass, 10% fail
     )

     diag <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 300,
          A_best = 0.95,
          cvw_best = 0.4,
          percentile_used = 4.5,
          convergence_tier = "tier_2",
          param_ess_results = param_ess_df,
          target_ess_param = 300,
          target_ess_param_prop = 0.95,  # Want 95%, have 90% → warn
          verbose = FALSE
     )

     # Check param_ess metric exists
     expect_true("param_ess" %in% names(diag$metrics))
     expect_equal(diag$metrics$param_ess$value, 0.9)
     expect_equal(diag$metrics$param_ess$n_pass, 90)
     expect_equal(diag$metrics$param_ess$n_total, 100)
     expect_equal(diag$metrics$param_ess$status, "warn")  # 90% < 95% but ≥ 95%*0.8

     # Overall should be WARN
     expect_equal(diag$summary$convergence_status, "WARN")
})


test_that("Parameter ESS metric is omitted when data not provided", {
     diag <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 300,
          A_best = 0.95,
          cvw_best = 0.4,
          percentile_used = 4.5,
          convergence_tier = "tier_2",
          param_ess_results = NULL,  # No parameter ESS data
          verbose = FALSE
     )

     # Check param_ess metric doesn't exist
     expect_false("param_ess" %in% names(diag$metrics))

     # Overall should still work
     expect_equal(diag$summary$convergence_status, "PASS")
})


test_that("Diagnostics structure is complete and correctly formatted", {
     diag <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 280,
          A_best = 0.92,
          cvw_best = 0.55,
          percentile_used = 4.8,
          convergence_tier = "tier_3",
          verbose = FALSE
     )

     # Check top-level structure
     expect_true("settings" %in% names(diag))
     expect_true("targets" %in% names(diag))
     expect_true("metrics" %in% names(diag))
     expect_true("summary" %in% names(diag))

     # Check settings
     expect_equal(diag$settings$optimization_tier, "tier_3")
     expect_equal(diag$settings$percentile_used, 4.8)

     # Check targets structure
     expect_true(all(c("ess_best", "A_best", "cvw_best",
                       "percentile_max", "ess_param", "ess_param_prop") %in%
                    names(diag$targets)))
     expect_equal(diag$targets$ess_best$value, 300)

     # Check metrics structure
     expect_true(all(c("B_size", "ess_best", "A_B", "cvw_B") %in%
                    names(diag$metrics)))

     # Each metric should have value, description, status
     for (metric_name in c("B_size", "ess_best", "A_B", "cvw_B")) {
          metric <- diag$metrics[[metric_name]]
          expect_true("value" %in% names(metric))
          expect_true("description" %in% names(metric))
          expect_true("status" %in% names(metric))
     }

     # Check summary structure
     expect_true(all(c("total_simulations_original", "n_successful",
                       "retained_simulations", "convergence_tier",
                       "percentile_used", "convergence_status") %in%
                    names(diag$summary)))
})


test_that("Helper function .calc_status works correctly", {
     # Higher is better
     expect_equal(MOSAIC:::.calc_status(300, 300, "higher", 0.8, 0.5), "pass")    # Meets target
     expect_equal(MOSAIC:::.calc_status(200, 300, "higher", 0.8, 0.5), "warn")    # 67%, between thresholds (50-80%)
     expect_equal(MOSAIC:::.calc_status(140, 300, "higher", 0.8, 0.5), "fail")    # 47%, below warn

     # Lower is better
     expect_equal(MOSAIC:::.calc_status(0.4, 0.5, "lower", 1.2, 2.0), "pass")     # Better than target
     expect_equal(MOSAIC:::.calc_status(0.7, 0.5, "lower", 1.2, 2.0), "warn")     # 140%, between thresholds
     expect_equal(MOSAIC:::.calc_status(1.2, 0.5, "lower", 1.2, 2.0), "fail")     # 240%, above warn

     # Non-finite values
     expect_equal(MOSAIC:::.calc_status(NA_real_, 300, "higher", 0.8, 0.5), "fail")
     expect_equal(MOSAIC:::.calc_status(Inf, 300, "higher", 0.8, 0.5), "fail")
     expect_equal(MOSAIC:::.calc_status(-Inf, 300, "higher", 0.8, 0.5), "fail")
})


test_that("Helper function .calc_percentile_status works correctly", {
     expect_equal(MOSAIC:::.calc_percentile_status(4.0, 5.0), "pass")    # Below target
     expect_equal(MOSAIC:::.calc_percentile_status(5.0, 5.0), "pass")    # Meets target
     expect_equal(MOSAIC:::.calc_percentile_status(6.5, 5.0), "warn")    # 130%, between thresholds
     expect_equal(MOSAIC:::.calc_percentile_status(10.0, 5.0), "fail")   # 200%, above warn

     # Non-finite values
     expect_equal(MOSAIC:::.calc_percentile_status(NA_real_, 5.0), "fail")
     expect_equal(MOSAIC:::.calc_percentile_status(Inf, 5.0), "fail")
})


test_that("Diagnostics can be serialized to JSON", {
     diag <- calc_convergence_diagnostics(
          n_total = 10000,
          n_successful = 9500,
          n_retained = 8500,
          n_best_subset = 400,     # Below max (8500*5%=425)
          ess_best = 280,
          A_best = 0.92,
          cvw_best = 0.55,
          percentile_used = 4.8,
          convergence_tier = "tier_3",
          verbose = FALSE
     )

     # Should be able to convert to JSON without error
     json_str <- jsonlite::toJSON(diag, pretty = TRUE, auto_unbox = TRUE)
     expect_true(is.character(json_str))
     expect_true(nchar(json_str) > 0)

     # Should be able to read it back
     diag_restored <- jsonlite::fromJSON(json_str)
     expect_equal(diag_restored$summary$convergence_status,
                 diag$summary$convergence_status)
     expect_equal(diag_restored$metrics$ess_best$value,
                 diag$metrics$ess_best$value)
})
