# Importance-sampling diagnostics of a vector of log importance ratios

Reports the effective sample size and the Pareto \\\hat k\\ of the
importance weights \\r_i \propto \exp(x_i)\\, \\x\\ = `log_lik`. MOSAIC
draws parameters from the prior and scores them with the log-likelihood
\\\ell\\, so the weights a posterior gives the draws follow from the
scored values; what the numbers describe depends on what is passed in.

## Usage

``` r
calc_is_diagnostics(log_lik, method = c("kish", "perplexity"))
```

## Arguments

- log_lik:

  Numeric vector of log-likelihoods, one per draw. Non-finite entries
  are dropped.

- method:

  Character; ESS estimator passed to
  [`calc_model_ess`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md).
  One of `"kish"` (default) or `"perplexity"`.

## Value

A named list with `n`, `ess_is`, `ess_is_prop`, `khat`, `khat_status`
and `n_positive_ratios`.

## Details

- Tempered input, `eta * log_lik`:

  The weights of the coarsened posterior \\w_i \propto \exp(\eta
  \ell_i)\\, \\\eta = \alpha / n\_{obs}\\, that MOSAIC actually builds.
  Its \\\hat k\\ is the validity guard of
  [`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md)
  (`.mosaic_khat_tempered()`): below 0.5 the ESS and its projection can
  be trusted; 0.5-0.7 usable but the Monte Carlo error converges slowly;
  at or above 0.7 the importance-sampling estimates are unreliable
  (Vehtari et al. 2024).

- Untempered input, `log_lik` (\\\eta = 1\\):

  The likelihood-concentration numbers. With thousands of scored cells
  and a stochastic likelihood the untempered ratios span thousands of
  log units, so `ess_is` is about 1 and \\\hat k\\ is huge or undefined
  whatever the fit. They measure how sharply the raw likelihood ranks
  the draws; they do **not** say whether the sampler explored the
  posterior, no posterior is built from them, and MOSAIC reports them in
  JSON only.

&nbsp;

- `ess_is`:

  The Kish (or perplexity) ESS of the normalised ratios, without any
  truncation.

- `khat`:

  The Pareto \\\hat k\\ shape statistic of PSIS (Vehtari et al. 2024),
  estimated with the empirical-Bayes profile likelihood of Zhang &
  Stephens (2009). `NA` with an explanatory `khat_status` when the tail
  cannot be fitted: fewer than 20 draws, tied tail ratios (nearly flat
  weights), or ratios that underflow to zero.

## References

Vehtari A, Simpson D, Gelman A, Yao Y, Gabry J (2024). Pareto smoothed
importance sampling. *JMLR* 25(72):1-58.

Zhang J, Stephens MA (2009). A new and efficient estimation method for
the generalized Pareto distribution. *Technometrics* 51(3):316-325.

## Examples

``` r
# Well-behaved importance weights: ESS a healthy fraction of n, small khat
set.seed(1)
calc_is_diagnostics(rnorm(1000, sd = 0.5))
#> $n
#> [1] 1000
#> 
#> $ess_is
#> [1] 765.7633
#> 
#> $ess_is_prop
#> [1] 0.7657633
#> 
#> $khat
#> [1] 0.1238871
#> 
#> $khat_status
#> [1] "good: finite IS variance"
#> 
#> $n_positive_ratios
#> [1] 1000
#> 

# Coarsened posterior of scored draws: pass the tempered log-likelihood
ll <- -2000 * (runif(5000) - 0.4)^2
calc_is_diagnostics((10 / 1000) * ll)$khat
#> [1] -1.752454
```
