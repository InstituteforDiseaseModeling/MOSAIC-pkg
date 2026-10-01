# plot_vaccine_effectiveness(): panels B, C, E and F draw the distributions that
# est_vaccine_effectiveness() fitted to the Xu et al. (2024) estimates
# (param_vaccine_effectiveness.csv), not the phi/omega priors in priors_default,
# which make_priors_default.R widens and refits. The figure used to present
# them as prior distributions; these tests pin the data-fit labelling and that
# the panels are drawn from the fit file.

.write_ve_plot_inputs <- function(dir) {
     days <- 0:(365 * 10)
     pred <- rbind(
          data.frame(day = days, predicted = 0.79 * exp(-7e-4 * days),
                     predicted_lo = 0.75 * exp(-9e-4 * days),
                     predicted_hi = 0.82 * exp(-5e-4 * days), dose_regimen = "One-dose OCV"),
          data.frame(day = days, predicted = 0.79 * exp(-3.6e-4 * days),
                     predicted_lo = 0.78 * exp(-6e-4 * days),
                     predicted_hi = 0.80 * exp(-1e-4 * days), dose_regimen = "Two-dose OCV"))
     utils::write.csv(pred, file.path(dir, "pred_vaccine_effectiveness.csv"), row.names = FALSE)
     utils::write.csv(data.frame(months = c(6, 12, 12, 24), days = c(183, 366, 366, 731),
                                 effectiveness = c(0.70, 0.60, 0.68, 0.60),
                                 effectiveness_lo = c(0.60, 0.51, 0.58, 0.40),
                                 effectiveness_hi = c(0.77, 0.68, 0.78, 0.73),
                                 dose_regimen = rep(c("One-dose OCV", "Two-dose OCV"), each = 2),
                                 source = "Xu et al (2024)"),
                      file.path(dir, "data_vaccine_effectiveness.csv"), row.names = FALSE)
     param <- data.frame(
          variable_name = rep(c("omega_1", "phi_1", "omega_2", "phi_2"), each = 5),
          parameter_name = c("low", "mean", "high", "shape", "rate",
                             "low", "mean", "high", "shape1", "shape2",
                             "low", "mean", "high", "shape", "rate",
                             "low", "mean", "high", "shape1", "shape2"),
          parameter_value = c(4.7e-4, 7.0e-4, 1.1e-3, 28.23, 38647.6,
                              0.7528, 0.7876, 0.8224, 433.55, 117.63,
                              9.8e-5, 3.6e-4, 1.05e-3, 3.25, 6299.6,
                              0.7772, 0.7876, 0.7981, 4803.6, 1295.9))
     utils::write.csv(param, file.path(dir, "param_vaccine_effectiveness.csv"), row.names = FALSE)
     invisible(param)
}

test_that("the fitted-distribution panels are labelled as data fits, not priors", {
     local_null_device()
     inp <- withr::local_tempdir()
     fig <- withr::local_tempdir()
     param <- .write_ve_plot_inputs(inp)
     res <- suppressMessages(plot_vaccine_effectiveness(list(MODEL_INPUT = inp, DOCS_FIGURES = fig)))

     for (f in c("vaccine_one_dose_combined.pdf", "vaccine_two_dose_combined.pdf",
                 "vaccine_all_combined.pdf")) {
          expect_true(file.exists(file.path(fig, f)), info = f)
     }
     expect_named(res, c("one_dose", "two_dose", "all", "panels"))
     expect_named(res$panels, LETTERS[1:6])
     for (panel in c("B", "C", "E", "F")) {
          expect_match(res$panels[[panel]]$labels$subtitle, "Data fit .*not the prior", info = panel)
          expect_false(grepl("prior distribution", res$panels[[panel]]$labels$subtitle))
     }

     # Panel B is the est_vaccine_effectiveness() Beta fit read from the file
     # (normalised over its plotted grid), not the shipped phi_1 prior
     b <- res$panels$B$data
     pv <- function(v, p) param$parameter_value[param$variable_name == v & param$parameter_name == p]
     fit <- stats::dbeta(b$x, pv("phi_1", "shape1"), pv("phi_1", "shape2"))
     expect_equal(b$density, fit / sum(fit))
     prior <- priors_default$parameters_global$phi_1$parameters
     dens_prior <- stats::dbeta(b$x, prior$shape1, prior$shape2)
     expect_false(isTRUE(all.equal(b$density, dens_prior / sum(dens_prior))))
})
