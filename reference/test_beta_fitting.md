# Test the beta fitting function with vaccine effectiveness parameters

Test the beta fitting function with vaccine effectiveness parameters

## Usage

``` r
test_beta_fitting()
```

## Examples

``` r
if (FALSE) { # \dontrun{
# Test with phi_1 values from the vaccine effectiveness analysis
phi_1_fit <- fit_beta_from_ci(
  mode_val = 0.788,
  ci_lower = 0.753, 
  ci_upper = 0.822,
  method = "optimization",
  verbose = TRUE
)

# Test with phi_2 values
phi_2_fit <- fit_beta_from_ci(
  mode_val = 0.769,
  ci_lower = 0.739,
  ci_upper = 0.798,
  method = "optimization",
  verbose = TRUE
)

# Plot the fitted distributions
library(ggplot2)

x_vals <- seq(0, 1, length.out = 1000)
df <- data.frame(
  x = rep(x_vals, 2),
  density = c(
    dbeta(x_vals, shape1 = phi_1_fit$shape1, shape2 = phi_1_fit$shape2),
    dbeta(x_vals, shape1 = phi_2_fit$shape1, shape2 = phi_2_fit$shape2)
  ),
  parameter = rep(c("phi_1", "phi_2"), each = length(x_vals))
)

ggplot(df, aes(x = x, y = density, color = parameter)) +
  geom_line(size = 1.5) +
  theme_minimal() +
  labs(title = "Fitted Beta Distributions for Initial Vaccine Effectiveness",
       x = "Initial effectiveness (phi)",
       y = "Probability density")
} # }
```
