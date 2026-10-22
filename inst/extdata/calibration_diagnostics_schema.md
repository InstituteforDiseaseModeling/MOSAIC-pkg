# MOSAIC output contract: summary.json schema 2 and calibration_diagnostics.json

Versioned with the package (MOSAIC 1.5.1; installed at
`system.file("extdata", "calibration_diagnostics_schema.md", package = "MOSAIC")`).
It is sections 1-2 of the project's contract (`claude/plan_convergence/SCHEMA_v2.md`),
including the 1.5.1 additions. Words, formulas, target templates and the
rich-text tokens of the `*_display` strings are defined in
`inst/extdata/metric_glossary.yml` (its header documents its schema).
A test checks that every field of a real calibration_diagnostics.json is named here.

## 1. `3_results/summary.json`: new top-level fields (all scalars unless noted)

| Field | Type | Meaning |
|---|---|---|
| `schema_version` | string | `"2"` |
| `posterior_weighting` | string | `"bestfit"` (default), `"coarsened"` (SIR resample, opt-in), `"saturated"` (legacy, opt-in) |
| `convergence_status` | string | `"green"`, `"yellow"` or `"red"`: simulation convergence only (SPEC §5) |
| `convergence_label` | string | "Converged: enough simulations" / "Usable with caution" / "Not converged: more simulations needed"; a fourth label, "Run failed: no usable draws" (RED), only when fewer than two draws are valid (round 3) |
| `convergence_reasons` | array of string | One plain-language reason per non-green check (empty when green) |
| `convergence_reasons_text` | string | The reasons joined with `"; "` (`""` when green), for scalar-only consumers |
| `convergence_flags` | array of string | Zero or more of `"budget_cap_reached"`, `"ess_growth_stalled"`, `"ensemble_size_bound"`, `"fixed_budget"` |
| `convergence_flags_text` | string | The flags joined with `"; "` |
| `check_ess_status` | string | green / yellow / red, for the ensemble-ESS check |
| `check_mce_status` | string | green / yellow / red, for the Monte Carlo error check |
| `check_khat_status` | string | green / yellow / red, for the validity guard |
| `check_informativeness_status` | string | green / yellow, for the data-informativeness check |
| `stopping_rule_met` | bool or null | Adaptive mode: TRUE iff the run stopped on the rule: the three sampling checks (ESS, MCE, k-hat) green at two consecutive checks after the minimum run length (round 1; informativeness does not block the stop, so a flat run can stop YELLOW). Fixed mode: null |
| `mode` | string | `"fixed"` or `"auto"` (existing field, unchanged) |
| `coarsening_alpha` | number | α (default 10) |
| `eta` | number | α / n_obs_scored |
| `n_obs_scored` | integer | Scored observation cells |
| `ess_all` | number | Kish ESS of the posterior weights over all valid draws |
| `ess_all_target` | number | `ess_best_target / best_subset_ratio` under "bestfit"; `ess_best_target` under "coarsened" (round 2) |
| `ess_best` | number | Kish ESS of the best-fit ensemble (equals `ratio × ess_all` by construction unless bounded) |
| `ess_best_target` | number | Default 200 |
| `best_subset_ratio` | number | Default 0.2 |
| `best_subset_mass` | number | Share of the posterior weight inside the ensemble (0–1) |
| `ensemble_members` | integer | Members of the ensemble (bestfit: draws selected; coarsened: resample size K) |
| `ensemble_members_unique` | integer | Unique parameter sets among the members |
| `ensemble_binding` | string | `"ratio"`, `"min_members"` or `"max_members"` |
| `mce_median`, `mce_p90`, `mce_max` | number | Monte Carlo error of the members' posterior means, as a fraction of posterior SD (0.068 = 6.8%) |
| `khat_tempered` | number or null | Pareto k-hat of the tempered weights; null when undefined (weights nearly flat) |
| `khat_status` | string | e.g. `"0.12 (good)"`, `"undefined (weights nearly flat)"` |
| `eta_star` | number or null | Largest η supported at the ESS target (diagnostic only since round 1) |
| `eta_star_n_eff` | number or null | `eta_star × n_obs_scored` |
| `flat_likelihood` | bool or null | TRUE iff `ess_untempered_frac >= status_thresholds$flat_ess_frac` (round 1; was `eta_star >= 0.999`); null when not assessable |
| `ess_growth_slope` | number or null | Log–log slope of ESS_all vs draws over the latter part of the run |
| `sims_used` | integer | Valid draws used (finite likelihood); may be fewer than `n_simulations_successful` |
| `sims_projected_for_target` | number or null | `sims_used × ess_all_target / ess_all` when the target is not met; null when met. Units: valid draws |
| `budget_exhausted` | bool | Adaptive run stopped by `max_simulations_total` before GREEN |
| `sims_projected_for_green` | number or null | Simulations projected for GREEN: the larger of the ESS projection and the MCE projection `N·(mce_p90/mce_green)²`, converted from valid draws to simulations at the run's success rate, plus one confirming batch; null when the three sampling checks are green (round 2). Units: simulations |
| `projection_reliable` | bool | FALSE when the ESS growth slope is < 0.8 or unknown; projections are then indicative only (round 1) |
| `ess_untempered_frac` | number or null | Kish ESS of the untempered (η = 1) weights / valid draws; the N-invariant informativeness measure (round 1) |
| `n_locations` | integer | Locations in the run (round 1) |
| `alpha_per_location` | number | `coarsening_alpha / n_locations`: η is pooled across locations (round 1) |
| `param_ess_min`, `param_ess_median` | number | Per-parameter ESS of the members (diagnostic only) |

**Unchanged:** all fit fields (`r2_cases_ensemble`, `bias_ratio_cases_ensemble`, …, `central_method_*`), timing, counts and identifiers.

**Removed:** the 1.3.0 weight-health fields `weights_used_*`, `batch_ratio_coarsened_*`, `ess_coarsened_prop`, `eta_ref`, `weight_health_reasons`, `likelihood_concentration_note` (the η = 1 numbers live only in `convergence_diagnostics.json`).

### Deprecated aliases (written for one release, then removed)

| Alias | Value |
|---|---|
| `converged` | Adaptive: `stopping_rule_met`. Fixed: FALSE (old semantics) |
| `convergence_evaluated` | TRUE in adaptive mode, FALSE in fixed |
| `posthoc_criteria_met` | `convergence_status == "green"` |
| `weight_health_status` | `"ok"` if green, else `"warn"` |
| `ess_coarsened` | `ess_all` |
| `n_best_subset` | `ensemble_members` |
| `ess_min`, `ess_median` | `param_ess_min`, `param_ess_median` |

## 2. `3_results/calibration_diagnostics.json` (new; computed regardless of `plots`)

```
{
  "schema_version": "2",
  "schema_revision": "1.5.1",                                                                  # 1.5.1
  "run": {mosaic_version, locations[], n_locations, n_simulations, mode, posterior_weighting, coarsening_alpha, eta,
          n_obs_scored, burn_in_days, n_iterations, n_iter_ensemble, date_start, date_stop,
          n_simulations_requested, max_simulations_total, min_ensemble_members, max_ensemble_members,   # 1.5.1
          central_method: {cases, deaths}, glossary_version},                                         # 1.5.1
  "status": {convergence_status, convergence_label, reasons[], flags[], flags_text, stopping_rule_met,
             checks: [{check, label, value, threshold_green, threshold_yellow, colour, reason,
                       key, operator, operator_yellow, value_display, threshold_green_display,          # 1.5.1
                       threshold_yellow_display, target_display, target_note, status_display}],        # 1.5.1
             flags_detail: [{id, text, display}],                                                      # 1.5.1
             thresholds: {ess_yellow_frac, mce_green, mce_yellow, khat_green, khat_yellow,             # 1.5.1
                          slope_stall, flat_ess_frac}, thresholds_source,                              # 1.5.1
             headline_display, palette: {green, yellow, red, grey}},                                   # 1.5.1
  "sampling": {n_locations, alpha_per_location, ess_untempered_frac, sims_projected_for_green, projection_reliable,
               ess_all, ess_all_target, ess_best, ess_best_target, best_subset_ratio, best_subset_mass,
               ensemble_members, ensemble_members_unique, ensemble_binding, khat_tempered, khat_status,
               eta_star, eta_star_n_eff, flat_likelihood, ess_growth_slope, sims_used,
               sims_projected_for_target, budget_exhausted,
               ess_path: [{n, ess_all}], loglik_path: [{n, best_gap, top100_gap}],
               details_display: [{key, value}]},                                                      # 1.5.1
  "info": {                                                                                           # 1.5.1
    ess_growth:    {key, value, display, target, operator, target_value_display, target_display, status, status_display},
    budget:        {key, value, cap, display, sub_display, target, target_display, target_note, status, status_display},
    ensemble_size: {key, value, min, max, binding, display, sub_display, target_display, status, status_display}},
  "parameters": [{parameter, base, location, prior_family, mce, kl, kl_band}],
  "mce_summary": {median, p90, max, worst: [{parameter, mce}]},
  "kl_summary": {largely_prior, partly_informed, data_informed, withheld, band_edges: [0.05, 0.5]},
  "fit": {table: [{metric, subset, r2, bias, cov50, cov95, wis, n_scored}],
          table_display: [{metric, subset, label, r2, bias, cov50, cov95, wis, n_scored}],             # 1.5.1
          per_location: [{location, metric, r2, bias, cov95, observed_total}],
          weekly: [{location, metric, week, scored, observed, predicted, lo50, hi50, lo95, hi95}],      # scored: 1.5.1
          weekly_meta: {source, central_method: {cases, deaths}, intervals: {cases, deaths},           # 1.5.1
                        quantiles: [0.025, 0.25, 0.75, 0.975], week_start, week_offset: {<location>: 0-6},
                        all_locations_id, all_locations_week_offset}},
  "likelihood_concentration": {ess_is_all, note},
  "upgrade": {from_schema_revision, by_mosaic_version, weekly_source, control_read}                   # 1.5.1, upgraded files only
}
```

Arrays of numbers are plain JSON numbers. Paths have ≤ 60 points. `fit.weekly`, `fit.per_location` and the paths are rounded to 4 significant digits; every other number keeps full precision.

Field notes (added 2026-10-20 from the WS3 renderer):
- `run.locations` is always an array of ISO codes, even for one location.
- `status.checks[].label` is the human-readable check name (e.g. "Ensemble ESS"); `check` is the id (`ess`, `mce`, `khat`, `informativeness`).
- `status.flags_text` and `status.stopping_rule_met` mirror the summary.json fields of the same names (`stopping_rule_met` null in fixed mode).
- `run.n_simulations` is the simulations attempted (valid draws are `sampling.sims_used`); round 2.
- `kl_summary.withheld` counts parameters whose KL band is withheld because the ensemble ESS is below 100; round 2.
- `flat_likelihood` additionally requires an untempered Kish ESS >= 20 (round 2).
- `kl_summary.band_edges` are the KL cut points (nats on the PIT scale) separating largely prior / partly informed / data informed.
- `fit.table[].subset` is `"scored"` (the same days as the summary.json fit fields) or `"tier12"` (observed and reconstructed weeks only); round 1.
- `fit.weekly.week` is the ISO-8601 date (`YYYY-MM-DD`) of the Monday that starts the reporting week.

### 2.1 Additions in 1.5.1 (the report contract)

The report (`plot_model_calibration_report()`) reads only this file and `inst/extdata/metric_glossary.yml`; the dashboard can reproduce page 1 from the same two files with no formatting logic. Every `*_display` string is **rich text**: `{name}` is a token of the glossary's `tokens` table (`ge`, `le`, `lt`, `khat`, `alpha`, `eta`, `f1`, `ESSall`, `ESSE`, `E`), rendered as its `text` (Unicode), `ascii`, `plotmath` or `katex` form. Everything is computed in-run whatever `plots` is, by `.mosaic_calrep_complete()` (`R/calibration_diagnostics_display.R`) from the file's numbers and the glossary.

- `schema_revision` `"1.5.1"`: a file without it predates these fields (see 2.3).
- `run.n_simulations_requested` (control `calibration$n_simulations`; null in adaptive mode), `run.max_simulations_total`, `run.min_ensemble_members`, `run.max_ensemble_members`: the budget and member bounds. `run.central_method`: the per-channel central method of the predictions. `run.glossary_version`: the glossary the display strings were composed with.
- `status.thresholds`: the full `targets$status_thresholds` the run applied (supersedes §5.6 for this file); `status.thresholds_source` is `"run control"`, or `"package default"` when an older file without control.json was completed.
- `status.checks[].label` (changed in 1.5.1): the glossary `short_label` of the check's `key`, as text — the name the report prints (one source); before 1.5.1 it was `calc_convergence_status()`'s label (e.g. "Validity guard (Pareto k-hat)").
- Numbers in `*_display` strings use a real minus sign (U+2212); the PDF prints it as the WinAnsi minus.
- `status.checks[]`: `key` (glossary key: `ess_best`, `mce`, `khat_tempered`, `flat_likelihood`); `operator` / `operator_yellow` (`"ge"`, `"le"`, `"lt"`: green when the value is `operator` `threshold_green`, yellow when `operator_yellow` `threshold_yellow`; informativeness is `lt` / `ge` on the same threshold, no red); `value_display` (never rounded onto the other side of a threshold, `.mosaic_fmt_thr()`), `threshold_green_display`, `threshold_yellow_display`, `target_display` (from the glossary `target_template`, e.g. `"{ge} 200 / {ge} 100"`), `target_note` (e.g. "flat; no red"), `status_display` ("GREEN").
- `status.flags_detail[]`: one record per flag, `id` (as in `flags[]`), its own `text`, and `display` (FALSE for `fixed_budget`, which the run line already states). `flags_text` stays the joined string.
- `status.headline_display`: "OVERALL: RED — Not converged: more simulations needed". `status.palette`: the status colours (`mosaic_colors("status")`).
- `info.ess_growth`: `value` = `ess_growth_slope`, `target` = `slope_stall`, `status` `ok` / `stalled` / `na`. `info.budget`: `value` = simulations attempted, `cap` = `max_simulations_total` (adaptive) or `n_simulations_requested` (fixed), `target` = `sims_projected_for_green`, `target_display` "already green" / "n/a" (with `target_note` "sampling checks green") / "green: ~227k" (with "(indicative)" when `projection_reliable` is FALSE), `status` `ok` / `at_cap` / `failed` (fewer than two valid draws). `info.ensemble_size`: `value` = members, `min` / `max` bounds, `binding`, `sub_display` "by ratio" / "at min" / "at max", `status` `ok` / `at_bound` / `failed`. `info.ess_growth.status` is `na` when the slope is unknown. The words are the glossary's `report$info`.
- `fit.table_display[]`: the fit-table rows as shown (cases then deaths; every scored day, then tier 1–2), all strings.
- `sampling.details_display[]`: the six sampling-detail values as shown; `key` names the glossary label (`report$sampling_details$<key>`). In a multi-location run the posterior value adds alpha per location.

### 2.2 `fit.weekly` semantics (changed in 1.5.1)

1.4.0–1.5.0 held **scored weeks only**, with `predicted` the sum of the daily central line and the bounds the sums of the daily interval bounds, so the line hugged zero on sparse series (the daily median is 0 on 69–93% of days for CIV, CAF and ZAF cases) and the bands were blocky. From 1.5.1 (`.mosaic_calibration_weekly()`):
- one row per **complete reporting week** of each location-channel series: seven days from Monday plus the location's `week_offset` (the observation model's blocks; 0, Monday–Sunday, for every production location today; recorded in `weekly_meta.week_offset`), all inside `date_start`..`date_stop` (partial edge weeks left out). `week` is the first day of the reporting week;
- in a multi-location run, also one **all-locations** row per week and channel, `location` = `weekly_meta.all_locations_id` ("ALL", suffixed with "_" if a location already has that name): each trajectory's daily series is summed over the locations and then to the week, and summarised exactly as below, never by adding per-location lines; a day is scored only when every location is scored; its weeks start on Monday plus the locations' common offset, or Monday when they differ (`all_locations_week_offset`). The report's page 3 draws this row for a multi-location run; page 4 uses the per-location rows;
- `scored` TRUE when all seven days are scored (finite observation and central line, the days of `fit.table`); `observed` is their total, **null when not scored**;
- `predicted`, `lo50`, `hi50`, `lo95`, `hi95` null for a week touching the channel's unscored head (burn-in); otherwise from **weekly-summed trajectories**: each member × stochastic run's daily series is summed to the week, X_{r,w} = Σ_{t∈w} x_{r,t}, with trajectory weight v_r = w_p / S (member weight shared by its S runs; a trajectory missing a day that week is dropped and the rest renormalised). `predicted` is the weighted median (`central_method` "median") or weighted mean ("mean") of the **engine-level** totals (the daily central-line convention); the bounds are the weighted 2.5 / 25 / 75 / 97.5% quantiles (`weighted_quantiles()`) of the **observation-level** totals (the draws behind `cases_array` / `deaths_array`). `fit.weekly_meta` records `source` (`"trajectories"`), `central_method`, `intervals` per channel (`"observation"`, or `"engine"` for an ensemble without an observation model), `quantiles` and `week_start`.
- `weekly_meta.source = "daily_sums"`: only an upgraded file (2.3) whose run did not persist its ensemble arrays; the model values are then weekly sums of the daily table (the all-locations row: the line only, no bands), so they differ slightly from what the run would have written; the report says so under the series.

### 2.3 Older files

`render_MOSAIC_figures()` (group `"convergence"`) upgrades a file without `schema_revision` before drawing (`.mosaic_upgrade_calibration_diagnostics()`): the run fields and thresholds from `1_inputs/control.json`, `fit.weekly` from `3_results/predictions/predictions_ensemble_*.csv` and the persisted ensemble (`2_calibration/ensemble_optimized.rds`, else `ensemble_candidate.rds`; trajectories when its arrays were persisted, as production sets `persist_ensemble_arrays = TRUE`), then the display fields by the same function the writer uses, so an upgraded 1.5.0 file equals what 1.5.1 writes for that run (tested). It **replaces** `calibration_diagnostics.json` (atomically), keeps the original byte for byte as `calibration_diagnostics.pre151.json` (never overwritten), leaves `run.mosaic_version` as the version that ran the calibration, and adds the `upgrade` block. `plot_model_calibration_report()` given an older file completes the display fields in memory (package-default thresholds when the file has none; the weekly series it holds, scored weeks only, labelled as such). Pre-1.4.0 runs keep the legacy notice.
