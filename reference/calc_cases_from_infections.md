# Map infections to suspected and true cholera cases

Given a vector of incident *symptomatic infections*
\\I\_{\text{sym}}(t)\\ (e.g., from LASER model) and constant \\\rho\\
(reporting probability among symptomatic), this function returns vectors
of expected **suspected** and **true/confirmed** cases aligned on report
day using a simple time shift \\\Delta\\ (days).

By default, the positive predictive value \\\chi\\ among suspected cases
is set to `chi_endemic` for all times. If `epidemic_threshold` is
provided, the function switches \\\chi\\ between `chi_endemic` and
`chi_epidemic` according to whether
\\I\_{\text{new}}(t-\Delta)/N(t-\Delta)\\ exceeds the threshold.

## Usage

``` r
calc_cases_from_infections(
  infections,
  N = NULL,
  rho,
  chi_endemic,
  chi_epidemic,
  epidemic_threshold = NULL,
  delta_t = 0
)
```

## Arguments

- infections:

  Numeric vector of incident symptomatic infections
  \\I\_{\text{sym}}(t)\\ (length \\n\\). Must be non-negative; `NA`s are
  allowed and propagate.

- N:

  Numeric scalar or vector of length \\n\\: population size used to
  compute the infection proportion \\I\_{\text{sym}}(t)/N(t)\\ for the
  switching rule. **Ignored if** `epidemic_threshold` is `NULL`. Default
  is `NULL`.

- rho:

  Scalar in \\\[0,1\]\\: probability a symptomatic cholera infection is
  reported as *suspected*.

- chi_endemic:

  Scalar in \\(0,1\]\\: PPV among suspected during endemic levels (also
  the default PPV if `epidemic_threshold` is `NULL`).

- chi_epidemic:

  Scalar in \\(0,1\]\\: PPV among suspected during epidemic levels (used
  only when `epidemic_threshold` is provided).

- epidemic_threshold:

  `NULL` or a scalar in \\\[0,1\]\\: threshold on
  \\I\_{\text{sym}}(t)/N(t)\\ that determines whether `chi_epidemic`
  (above threshold) or `chi_endemic` (otherwise) is used. If `NULL`, no
  switching is applied and `chi_endemic` is used for all times. Default
  is `NULL`.

- delta_t:

  Non-negative integer number of days for the symptomatic
  infection→report delay \\\Delta\\. Default is 0.

## Value

A named list with two numeric vectors, each length \\n\\:

- `cases_suspected`: expected suspected cholera cases
  \\C\_{\text{sus}}(t)\\.

- `cases_true`: expected true/confirmed cholera cases
  \\C\_{\text{true}}(t)\\.

## Details

Shift-only delay model with constant \\\rho\\:
\$\$\mathbb{E}\[C\_{\text{true}}(t)\] \\=\\
\rho\\I\_{\text{sym}}(t-\Delta),\$\$
\$\$\mathbb{E}\[C\_{\text{sus}}(t)\] \\=\\
\frac{\rho}{\chi_t}\\I\_{\text{sym}}(t-\Delta),\$\$ where \\\chi_t \in
(0,1\]\\ is the PPV among suspected cases (fraction that are truly
cholera).

**Epidemic switching rule (optional):** \$\$\chi_t \\=\\ \begin{cases}
\text{chi\\epidemic}, & \text{if } I\_{\text{sym}}(t-\Delta)/N(t-\Delta)
\> \text{epidemic\\threshold},\\ \text{chi\\endemic}, &
\text{otherwise.} \end{cases}\$\$ If `epidemic_threshold` is `NULL`, the
function uses `chi_endemic` for all times (i.e., the switching is
bypassed and `N` is ignored).

When `delta_t > 0`, the first `delta_t` elements of the outputs are `NA`
(no earlier infections available to map forward). When `delta_t = 0`,
outputs are aligned one-to-one with `infections` and contain no leading
`NA`s.

## Alignment note

Outputs are indexed by report day \\t\\. Values at indices \\1:\Delta\\
are `NA` because they depend on \\I\_{\text{sym}}(t-\Delta)\\ before the
start of the series.

## Examples

``` r
infections <- c(0, 1, 2, 5, 40, 120, 200, 75, 50, 30, 15, 10, 3, 0, 0)
N <- 20000

# 1) No switching: epidemic_threshold = NULL (chi_endemic used everywhere; N ignored)
out1 <- calc_cases_from_infections(
  infections = infections,
  N = NULL,
  rho = 0.70,
  chi_endemic = 0.50,
  chi_epidemic = 0.75,
  epidemic_threshold = NULL,
  delta_t = 0
)
out1$cases_suspected; out1$cases_true
#>  [1]   0.0   1.4   2.8   7.0  56.0 168.0 280.0 105.0  70.0  42.0  21.0  14.0
#> [13]   4.2   0.0   0.0
#>  [1]   0.0   0.7   1.4   3.5  28.0  84.0 140.0  52.5  35.0  21.0  10.5   7.0
#> [13]   2.1   0.0   0.0

# 2) Switching on: use chi_epidemic when infections/N > 15/100,000
out2 <- calc_cases_from_infections(
  infections = infections,
  N = N,
  rho = 0.70,
  chi_endemic = 0.50,
  chi_epidemic = 0.75,
  epidemic_threshold = 15/100000,
  delta_t = 2
)
out2$cases_suspected; out2$cases_true
#>  [1]         NA         NA   0.000000   1.400000   2.800000   4.666667
#>  [7]  37.333333 112.000000 186.666667  70.000000  46.666667  28.000000
#> [13]  14.000000   9.333333   4.200000
#>  [1]    NA    NA   0.0   0.7   1.4   3.5  28.0  84.0 140.0  52.5  35.0  21.0
#> [13]  10.5   7.0   2.1
```
