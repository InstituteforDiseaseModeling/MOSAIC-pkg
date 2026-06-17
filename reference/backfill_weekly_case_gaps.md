# Backfill Short Reporting Gaps in a Weekly Case Series

Linearly interpolates *interior* runs of missing weekly case counts that
are (a) no longer than `max_interp_weeks` consecutive weeks, (b) bounded
by reported (non-`NA`) weeks on both sides, and (c) shouldered by
reported weeks that both exceed `min_anchor`. This targets the common
surveillance artifact where reporting lapses over public holidays (most
often the Christmas / New-Year weeks) leave a 1–2 week hole in an
otherwise-reported series.

## Usage

``` r
backfill_weekly_case_gaps(
  d,
  max_interp_weeks = 2L,
  method = c("linear"),
  cases_col = "cases",
  group_col = "iso_code",
  date_col = "date",
  flag_col = "cases_interpolated",
  min_anchor = 0,
  round_counts = TRUE,
  verbose = TRUE
)
```

## Arguments

- d:

  A long data frame with one row per group-week, containing at least the
  columns named by `group_col`, `date_col` and `cases_col`.

- max_interp_weeks:

  Integer \\\ge 1\\ (default `2L`). Maximum length, in consecutive
  missing weeks, of an interior gap that will be interpolated. Holiday
  lapses are typically 1–2 weeks.

- method:

  Interpolation method. Currently only `"linear"` (default, via
  [`na.approx`](https://rdrr.io/pkg/zoo/man/na.approx.html)); exposed as
  an argument so richer methods (e.g. spline, seasonal) can be added
  later without changing call sites.

- cases_col, group_col, date_col:

  Column names (defaults `"cases"`, `"iso_code"`, `"date"`).

- flag_col:

  Name of the logical column added to mark interpolated weeks (default
  `"cases_interpolated"`); set `TRUE` only for cells this function
  actually filled with a positive value.

- min_anchor:

  Numeric (default `0`). A gap is interpolated only when *both* of its
  bounding reported weeks are strictly greater than `min_anchor`. The
  default leaves gaps shouldered by a reported zero unfilled (see
  Guards).

- round_counts:

  Logical (default `TRUE`). Round *interpolated* values to whole counts
  (reported anchors are never rounded). Rounding uses R's
  round-half-to-even.

- verbose:

  Logical (default `TRUE`). Emit a one-line summary.

## Value

`d` with `cases_col` backfilled in place for qualifying gaps, a logical
`flag_col` marking the filled weeks, and the original row order
preserved.

## Details

Without backfilling, the suitability pipeline's NA\\\to\\0 sanitiser
(the keras target cannot contain `NaN`) stamps those weeks as
*zero-incidence*, which during an active outbreak injects a false "no
transmission" signal into the training target. See
[`compile_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_suitability_data.md)
(the intended call site).

**What is and is not modified.** Only the qualifying interior gap cells
are written; *reported anchor values are returned bit-identical* (the
rounding and non-negative clamp apply to filled cells only, never to
observed counts – which may legitimately be fractional, e.g.
disaggregated AI/monthly values). A filled value that rounds to `0` is
left `NA` (handed to the downstream zero-fill) rather than written as a
misleading "observed" zero.

**Interpolation basis.** Interpolation is done on the observation *date*
([`na.approx`](https://rdrr.io/pkg/zoo/man/na.approx.html) with
`x = date`), so an unevenly-spaced row – e.g. a dropped ISO week 53
leaving a 14-day step across the New-Year gap – is weighted by real
elapsed time rather than as an even row step. `maxgap` caps the run
length (in consecutive missing weeks) and `na.approx` never
extrapolates, so leading / trailing `NA` (incl. the post-cutoff forecast
tail) and runs longer than `max_interp_weeks` are left untouched.

**Guards.** (i) Gaps shouldered by a reported week at or below
`min_anchor` (default `0`, i.e. a reported zero) are *not* filled – a
near-zero shoulder is far more likely a genuine low-transmission /
non-outbreak week than a fillable outbreak interior, so it is left for
the downstream zero-fill. (ii) A missing row that merely duplicates the
`(group, date)` of a reported row (an upstream phantom-duplicate week)
is left untouched – the observed row already represents that week.

## See also

[`compile_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_suitability_data.md)
