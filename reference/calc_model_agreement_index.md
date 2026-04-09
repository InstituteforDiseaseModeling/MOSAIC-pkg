# Agreement index A based on entropy of weights

For the retained set \\\mathcal B = \\i : w_i \> 0\\\\, computes entropy
\\H(\tilde{\mathbf w}) = -\sum\_{i\in \mathcal B}\tilde w_i \log \tilde
w_i\\ and \\A = H(\tilde{\mathbf w}) / \log\|\mathcal B\|\\, with the
convention \\A=0\\ when \\\|\mathcal B\| \le 1\\.

## Usage

``` r
calc_model_agreement_index(w)
```

## Arguments

- w:

  Numeric vector of weights (raw or normalized). Only strictly positive
  entries form \\\mathcal B\\. Internally normalized within \\\mathcal
  B\\; larger \\A\\ is better.

## Value

A list with `A` (agreement index in \[0,1\]), `H` (entropy), and
`B_size` (retained set size).

## See also

[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md)
