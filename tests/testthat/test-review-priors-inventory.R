# Regression test (deep review, priors group): the estimated_parameters
# inventory must agree with priors_default on scale and prior family.
# alpha_1 was listed as "global" (priors hold it per-location since v15.16) and
# tau_i as "beta" (the shipped prior is lognormal), so posterior quantiles
# silently dropped alpha_1_<ISO> columns and fitted tau_i in the wrong family.

test_that("estimated_parameters scale and family match priors_default", {
     inv <- MOSAIC::estimated_parameters
     skip_if(utils::compareVersion(as.character(attr(inv, "version")), "1.3.0") < 0,
             "estimated_parameters not yet rebuilt from data-raw (needs >= 1.3.0)")
     pd <- MOSAIC::priors_default
     derived <- c("decay_days_long", "zeta_2")
     glob <- inv$parameter_name[inv$scale == "global"]
     loc  <- inv$parameter_name[inv$scale == "location"]
     expect_true(all(setdiff(glob, derived) %in% names(pd$parameters_global)))
     expect_true(all(loc %in% names(pd$parameters_location)))
     expect_equal(inv$scale[inv$parameter_name == "alpha_1"], "location")
     tau_fam <- unique(vapply(pd$parameters_location$tau_i$location,
                              function(x) x$distribution, character(1)))
     expect_equal(inv$distribution[inv$parameter_name == "tau_i"], tau_fam)
})
