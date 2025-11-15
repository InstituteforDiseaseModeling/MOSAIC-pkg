# Compare beta and gamma fitting approaches

Compare beta and gamma fitting approaches

## Usage

``` r
compare_distributions()
```

## Examples

``` r
if (FALSE) { # \dontrun{
# For parameters bounded in [0,1], compare beta distribution
phi_beta <- fit_beta_from_ci(mode_val = 0.788, 
                              ci_lower = 0.753, 
                              ci_upper = 0.822,
                              method = "optimization")

# For positive unbounded parameters, use gamma distribution
source("fit_gamma_from_ci.R")
omega_gamma <- fit_gamma_from_ci(mode_val = 0.000705, 
                                  ci_lower = 0.000471, 
                                  ci_upper = 0.001107,
                                  method = "optimization")

cat("Beta for bounded [0,1] parameters:\n")
cat("  Use for: effectiveness, proportions, probabilities\n")
cat("  Example: phi_1 ~ Beta(", phi_beta$shape1, ", ", phi_beta$shape2, ")\n\n")

cat("Gamma for positive unbounded parameters:\n")  
cat("  Use for: rates, times, positive continuous values\n")
cat("  Example: omega_1 ~ Gamma(", omega_gamma$shape, ", ", omega_gamma$rate, ")\n")
} # }
```
