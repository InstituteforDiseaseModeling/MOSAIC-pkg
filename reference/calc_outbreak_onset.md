# Detect cholera outbreak onsets in a weekly case series

Finds every outbreak in a weekly series of suspected cases and returns
its onset, end and (in real time) detection week, by a growth-rate rule
on a quasi-Poisson smooth (`method = "growth"`), the same rule with a
lower bar for small outbreaks (`method = "growth_sensitive"`), or the
Zheng et al. outbreak definition (`method = "zheng"`). Each rule runs
either on the full series (`mode = "retrospective"`) or one week at a
time on the data known by that week (`mode = "realtime"`), which records
the week an outbreak would have been declared. The growth rules work on
the smooth by default (`source = "smooth"`) or on growth rates and
levels computed directly from the reported counts (`source = "raw"`).

## Usage

``` r
calc_outbreak_onset(
  cases,
  week_start,
  population,
  observed = !is.na(cases),
  method = c("growth", "growth_sensitive", "zheng"),
  mode = c("retrospective", "realtime"),
  source = c("smooth", "raw"),
  growth_min = 0.05,
  growth_z = 1.28,
  growth_weeks = 3L,
  floor_per100k = 0.05,
  min_size = 1.5,
  end_frac_peak = 0.5,
  end_run_weeks = 6L,
  merge_gap_weeks = 5L,
  realtime_start = 53L,
  raw_weeks = 2L,
  segment_weeks = 7.5,
  gcv_gamma = 1.4,
  lambda = 10^seq(-2, 5, by = 0.5)
)
```

## Arguments

- cases:

  Numeric vector of weekly suspected cases, one per consecutive week;
  values in unreported weeks are ignored.

- week_start:

  Dates of the weeks' first days, consecutive 7 days apart.

- population:

  Population, one value or one per week (used for the per-capita floor
  and the Zheng incidence).

- observed:

  Logical vector, `TRUE` for reported weeks (default: non-missing
  `cases`).

- method:

  `"growth"` (default), `"growth_sensitive"` or `"zheng"`.

- mode:

  `"retrospective"` (default, full series) or `"realtime"` (weekly step
  forward).

- source:

  What the growth rules work on: `"smooth"` (default, the quasi-Poisson
  smooth) or `"raw"` (the reported counts; see Raw source). The Zheng
  rule always works on the counts.

- growth_min:

  Minimum weekly growth of a growth week, as a proportion (0.05 = 5% a
  week).

- growth_z:

  Multiple of the growth standard error that r must exceed (1.28: lower
  80% bound \> 0; 0.84 for `"growth_sensitive"`); 0 drops the check.

- growth_weeks:

  Consecutive growth weeks that start an outbreak in the full series
  (real time uses its own three-week rule).

- floor_per100k:

  Floor on the threshold, in suspected cases per 100,000 per week (0.05;
  0.02 for `"growth_sensitive"`).

- min_size:

  An outbreak's smoothed peak must reach this multiple of the threshold.

- end_frac_peak:

  A week below this fraction of the outbreak's running peak is low.

- end_run_weeks:

  Number of consecutive low weeks that end an outbreak (growth rule).

- merge_gap_weeks:

  A start within this many weeks of the previous end continues that
  outbreak.

- realtime_start:

  Week number, from the first reported week, of the first real-time
  estimate.

- raw_weeks:

  With `source = "raw"`: weeks in each of the two consecutive totals
  whose ratio gives the growth rate, and in the trailing mean that gives
  the level.

- segment_weeks:

  Width of a B-spline segment, in weeks.

- gcv_gamma:

  Inflation of the effective degrees of freedom in the GCV score.

- lambda:

  Grid of smoothing parameters searched by GCV.

## Value

A data frame with one row per outbreak (zero rows when there is none):

- outbreak_id:

  1, 2, ... in onset order.

- onset_week:

  First week of the outbreak.

- detection_week:

  Week the outbreak was declared (real time); `NA` retrospectively.

- peak_week, peak_cases:

  Week and count of the largest reported weekly count in the outbreak.

- peak_smoothed:

  Largest smoothed mean over the outbreak's reported weeks (in real
  time, of the newest estimates; with `source = "raw"`, the largest
  level); `NA` for the Zheng rule.

- end_week:

  Last week of the outbreak; `NA` while it is still open at the end of
  the series.

- complete:

  `FALSE` when the outbreak is still open at the last reported week (for
  the Zheng rule, also when it closes only inside the trailing padding).

- left_censored:

  `TRUE` when the onset is the first analysed week or follows an
  unreported week, so the outbreak may have begun earlier.

- duration_weeks, total_cases:

  Weeks and reported cases from onset to end (or to the last reported
  week).

- threshold_cases, threshold_per100k:

  The level threshold at the onset week (real time: at the detection
  week), in cases per week and per 100,000 per week.

- method, mode:

  As called.

- source:

  What the rule worked on: `"smooth"` or `"raw"` (always `"raw"` for the
  Zheng rule).

The attribute `"series"` is a data frame with one row per analysed week:
`week_start`, `cases`, `observed`, `mu` (smoothed mean; in real time the
newest estimate at each week), `r`, `r_lo`, `r_hi` (growth rate and its
95% interval), `threshold` (cases per week) and `in_outbreak`. The
smooth columns are `NA` for the Zheng rule. Over unreported weeks they
interpolate or extrapolate the spline with no data there and can be far
off over a long gap; the rules use them only at reported weeks, so plot
them on reported weeks only. With `source = "raw"`, `mu` is the level
and `r` the raw growth rate (see Raw source), `NA` at unreported weeks
and, for `r`, wherever its 2 x `raw_weeks` weeks are not all reported.

## Details

The series is analysed from its first to its last reported week; week
numbers below count from the first reported week.

**Growth rule** (the "tuned2" settings of the onset-detection handoff of
18 November 2026). The series is smoothed by a quasi-Poisson P-spline:
cubic B-splines on segments of `segment_weeks` weeks from the first
analysed week, a second-order difference penalty, and the smoothing
parameter chosen by GCV (with `gcv_gamma`) over `lambda`; unreported
weeks are left out of the fit. This gives the smoothed mean mu and the
weekly growth rate r = d log(mu) / dt with its standard error. The level
threshold is max(B, floor), where the floor is `floor_per100k` cases per
100,000 per week and B is the mean of mu over reported weeks outside
outbreaks, iterated with the outbreak set until it repeats (at most 25
rounds). An outbreak starts at a reported week where mu is at or above
the threshold and the next `growth_weeks` weeks (that week included) are
reported growth weeks: r \>= log(1 + growth_min) and r - growth_z x se
\> 0. A week is low when unreported or when mu is below max(threshold,
`end_frac_peak` x the outbreak's running peak); `end_run_weeks`
consecutive low weeks end the outbreak at the second of them. A start
within `merge_gap_weeks` of that end continues the same outbreak, still
judged against its earlier peak. An outbreak whose smoothed peak stays
below `min_size` x the threshold is discarded.

In real time the smooth is refitted at every week T from week
`realtime_start` on the reported weeks up to T (warm-started from the
previous week's fit). Because the segments have a fixed width, each fit
uses only the data known at T: the result up to T is the same whether or
not the series continues. Week T - 2 becomes a candidate at T (so the
first decision is at week `realtime_start` + 1) when weeks T - 2 to T
are reported, its smoothed level is at or above the threshold, and the
growth condition holds at T - 1 and T in fit T and at T - 2 and T - 1 in
fit T - 1; this three-week condition replaces `growth_weeks`. The
candidate is declared at the first reported week whose newest smoothed
estimate reaches `min_size` x the threshold (the detection week), and
dropped if it ends first. The real-time threshold is a different
statistic from the retrospective one: max(mean of the counts to date
outside declared outbreaks, unreported weeks counted as zero, floor). An
outbreak ends as in the full series, but its running peak restarts at an
end, so a merged continuation is judged against its own peak.

**Sensitive growth rule** (`method = "growth_sensitive"`). The growth
rule, in both modes, with two defaults changed, for small outbreaks in
large populations: `floor_per100k = 0.02`, so the threshold can fall to
the series' own baseline (a 0.05 floor is about 29 cases a week in
Kenya, above its baseline of about 15), and `growth_z = 0.84`, so a
growth week needs the lower 60% bound of r above zero instead of the
lower 80% bound. Arguments given explicitly override these defaults. On
the 40 MOSAIC countries it finds every outbreak of the growth rule plus
smaller ones, such as Kenya's 2024-2025 waves of 46-70 cases a week, and
dates some shared onsets earlier; it also raises more outbreaks in
sparse or blocky noise.

**Raw source** (`source = "raw"`). The growth rules, in both modes and
with the same settings, on quantities computed from the reported counts
instead of the smooth, with k = `raw_weeks`: the level at week t is the
mean of the reported counts among the k weeks ending at t; the growth
rate r is log((S_t + 0.5) / (S_t-k + 0.5)) / k, where S_t is the total
of the k weeks ending at t; and its standard error is the Poisson one,
sqrt(1 / (S_t + 0.5) + 1 / (S_t-k + 0.5)) / k. A week has no growth rate
unless all 2k weeks are reported. The windows trail, so a rise registers
about (k - 1) / 2 weeks later than in the smooth. Nothing is refitted,
so real time takes no longer than the full series, and in real time a
candidate needs three consecutive growth weeks (T - 2, T - 1 and T).
`segment_weeks`, `gcv_gamma` and `lambda` do not apply. On the 40 MOSAIC
countries (imputed days unreported) the raw growth rule with
`raw_weeks = 2` finds 90 outbreaks in the full series against the
smooth's 85, 53 of them within 4 weeks of a smooth onset; in real time
82 against 81 (50 matched, detected a median of one week later). With
`raw_weeks = 1` it finds only 37 (20 matched): a single dip breaks a run
of rising weeks. In outbreak-free series it raises false outbreaks where
the counts are over-dispersed, because its Poisson standard error
ignores that (see Caveats).

**Zheng rule** (Zheng et al. 2022, Int J Infect Dis 122:215-221; as
implemented in OutbreakExtractR, HopkinsIDD, main @ 4fcdc21). Eight zero
weeks pad the series before its first and after its last reported week,
and unreported weeks count as zero. The threshold is the mean weekly
incidence, sum(cases / population) / (weeks - 1). A week is high when
its incidence is at or above the threshold with cases \> 0. An outbreak
starts at a high week followed by two strict increases in the counts
(only when at least three weeks are high), and ends the week after the
first week that begins a run of at least six low weeks; a start at or
before that end belongs to the same outbreak. In real time the threshold
at week T uses the weeks up to T and the eight leading padding weeks,
week T - 2 is declared at T, and an outbreak ends at the second of six
consecutive weeks that are not high. The growth-rule arguments do not
apply.

**Caveats.**

- GCV usually selects the smallest lambda of the grid, so the knots
  (their width and position) set most of the smoothing, and shifting
  them moves growth onsets: 7.5-week segments reproduce 45 of the
  handoff's 96 onsets exactly and 75 within two weeks.

- The dispersion estimate collapses in very sparse series (mostly
  zeros), which shrinks the growth standard error; there only the floor
  guards against small rises.

- With `source = "raw"`, the growth standard error is Poisson's, which
  ignores the over-dispersion of reported counts. In outbreak-free
  negative binomial series of 10-50 cases a week it raises 4-10 false
  outbreaks per 10 years with `raw_weeks = 2` (17 with 3), where the
  smooth raises none; raising `growth_z` barely helps (6 at
  `growth_z = 3`). In Poisson noise it raises almost none. The settings
  were chosen on the smooth.

- The real-time 95% growth intervals at the newest week under-cover.

- A reporting gap of seven or more weeks always ends an outbreak.

- An onset at the first analysed week or just after an unreported week
  (`left_censored`) may have begun earlier.

## See also

[`est_outbreak_onsets`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_outbreak_onsets.md)
to run every location of a MOSAIC config.

## Examples

``` r
weeks <- seq(as.Date("2020-01-06"), by = 7, length.out = 160)
lambda_t <- 5 + 400 * exp(-((seq_along(weeks) - 100) / 6)^2)
set.seed(1)
x <- calc_outbreak_onset(rpois(160, lambda_t), weeks, population = 1e6)
x[, c("onset_week", "end_week", "peak_cases")]
#>   onset_week   end_week peak_cases
#> 1 2021-08-23 2022-01-10        402
```
