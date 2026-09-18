# Exact importance-sampling diagnostics for BFRS draws

Reports the importance-sampling (IS) quality of a set of scored draws
WITHOUT the \\\Delta\\AIC truncation that
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
applies when it builds the best-subset posterior. Because MOSAIC draws
parameters from the prior and scores them with the likelihood, the
(unnormalised) importance ratio of draw \\i\\ is \\r_i \propto
\mathcal{L}(\Theta^{(i)})\\, so the ratios follow directly from the
log-likelihoods.

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

Two numbers are returned:

- `ess_is`:

  the exact IS effective sample size. This is the quantity that says
  whether the importance sampler has actually explored the posterior. It
  is deliberately NOT the `ESS_B` reported by the convergence gate,
  which is computed on truncated weights and is therefore bounded away
  from its worst case by construction.

- `khat`:

  the Pareto \\\hat k\\ shape statistic of PSIS (Vehtari et al. 2024),
  estimated with the empirical-Bayes profile likelihood of Zhang &
  Stephens (2009). \\\hat k \< 0.5\\ indicates finite IS variance; \\0.5
  \le \hat k \< 0.7\\ is marginal; \\\hat k \ge 0.7\\ means the IS
  estimate is unreliable.

In BFRS runs at production scale the likelihood is evaluated over
\\O(10^5)\\ observations, so \\\Delta\\AIC values across prior draws are
routinely \\O(10^6)\\. The raw ratios then underflow to zero for all but
a handful of draws. That is not a numerical defect to be worked around –
it is the finding – so the degenerate case is reported explicitly via
`khat_status` rather than silently smoothed.

## References

Vehtari A, Simpson D, Gelman A, Yao Y, Gabry J (2024). Pareto smoothed
importance sampling. *JMLR* 25(72):1-58.

Zhang J, Stephens MA (2009). A new and efficient estimation method for
the generalized Pareto distribution. *Technometrics* 51(3):316-325.

## Examples

``` r
# A well-behaved sampler: ESS is a healthy fraction of n, khat is small
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
```
