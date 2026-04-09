# Coefficient of variation of (retained) weights

Computes \\\mathrm{CV}\_{\tilde{\mathbf w}} = \mathrm{sd}(\tilde{\mathbf
w}) / \mathrm{mean}(\tilde{\mathbf w})\\ over the retained set
\\\mathcal B = \\i : w_i \> 0\\\\, after normalizing within \\\mathcal
B\\.

## Usage

``` r
calc_model_cvw(w)
```

## Arguments

- w:

  Numeric vector of weights (raw or normalized). Smaller CV indicates
  less skew.

## Value

Scalar CV of weights over the retained set; `NA` if \\\|\mathcal B\| \<
2\\.

## See also

[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md)
