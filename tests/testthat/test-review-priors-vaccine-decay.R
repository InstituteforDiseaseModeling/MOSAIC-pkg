# Regression tests (deep review, priors group) for est_immune_decay_vaccine():
# it must plot the est_vaccine_effectiveness() fit, not refit its own.

.write_ve_inputs <- function(dir) {
     days <- 0:(365 * 5)
     pred <- rbind(
          data.frame(day = days, predicted = 0.5 * exp(-1e-3 * days),
                     predicted_lo = 0.4 * exp(-1e-3 * days),
                     predicted_hi = 0.6 * exp(-1e-3 * days), dose_regimen = "One-dose OCV"),
          data.frame(day = days, predicted = 0.55 * exp(-5e-4 * days),
                     predicted_lo = 0.45 * exp(-5e-4 * days),
                     predicted_hi = 0.65 * exp(-5e-4 * days), dose_regimen = "Two-dose OCV"))
     utils::write.csv(pred, file.path(dir, "pred_vaccine_effectiveness.csv"), row.names = FALSE)
     utils::write.csv(data.frame(months = c(6, 12), days = c(183, 366),
                                 effectiveness = c(0.45, 0.35), effectiveness_lo = c(0.3, 0.2),
                                 effectiveness_hi = c(0.6, 0.5),
                                 dose_regimen = c("One-dose OCV", "Two-dose OCV"),
                                 source = "test"),
                      file.path(dir, "data_vaccine_effectiveness.csv"), row.names = FALSE)
     utils::write.csv(data.frame(variable_name = c("phi_1", "omega_1", "phi_2", "omega_2"),
                                 parameter_distribution = "point", parameter_name = "mean",
                                 parameter_value = c(0.5, 1e-3, 0.55, 5e-4)),
                      file.path(dir, "param_vaccine_effectiveness.csv"), row.names = FALSE)
}

test_that("est_immune_decay_vaccine plots the est_vaccine_effectiveness outputs without refitting", {
     skip_if_not_installed("cowplot")
     inp <- withr::local_tempdir()
     fig <- withr::local_tempdir()
     .write_ve_inputs(inp)
     before <- tools::md5sum(list.files(inp, full.names = TRUE))
     p <- suppressMessages(est_immune_decay_vaccine(list(MODEL_INPUT = inp, DOCS_FIGURES = fig)))
     expect_true(file.exists(file.path(fig, "vaccine_effectiveness_decay.png")))
     # Inputs untouched and nothing new written to MODEL_INPUT
     expect_identical(tools::md5sum(list.files(inp, full.names = TRUE)), before)
     expect_false(is.null(p))
})

test_that("est_immune_decay_vaccine explains a missing est_vaccine_effectiveness run", {
     expect_error(est_immune_decay_vaccine(list(MODEL_INPUT = withr::local_tempdir(),
                                                DOCS_FIGURES = withr::local_tempdir())),
                  "est_vaccine_effectiveness")
})
