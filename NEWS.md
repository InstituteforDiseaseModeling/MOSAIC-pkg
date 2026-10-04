# MOSAIC 0.103.0

The default window moves to a 2018-01-01 start (`config_default` v7.0, `priors_default` v18.0). This release is the candidate for MOSAIC 1.0, which follows only on a GO or GO-WITH-CAVEATS verdict of the frozen acceptance rubric on the 2018-start production suite. OCV doses are split into first and second doses and each GTFCC delivery is released on its own date; imputed surveillance rows no longer seed the initial infections where the start window holds a count; a cases-dispersion fit within its 95% interval of the 0.1 lower bound is censored like a clamped one (likelihood tag `R/v0.103.0+near_bound_k_trend`, so a resume refuses shards scored by earlier versions); and the 0.102.0 wording of the psi amplitude floor is corrected.

## Default data objects
- `config_default` v7.0 and `priors_default` v18.0 are rebuilt at `date_start` 2018-01-01. The window is 2018-01-01 to 2027-04-29, 3,406 days (1,580 before); `date_stop` is unchanged (the psi horizon), and psi C3 starts on 2018-01-01 for every location, which is the builder's floor. The surveillance, demographic, CFR and psi inputs are those of v6.2 and v17.1: MOSAIC-data 922ef89 surveillance (built at 4957df4, whose later commits add only the psi provenance bundles), ees-cholera-mapping 780eb54, and psi C3 as re-corrected in 0.102.0. The vaccination inputs are the regenerated files of the dose split and the delivery-dated release (see Vaccination).
  - **Built in two passes, checked by a third.** `est_initial_V1_V2()` divides doses by the installed `config_default`'s `N_j_initial`, so a priors build against the installed v6.2 divided 2018 vaccinations by 2023 populations. The shipped priors are the second pass, against config v7.0: `prop_V1_initial` x1.11-1.19 in 13 locations, `prop_V2_initial` in NGA and ZMB and `prop_S_initial` through the residual, with every other value identical. A third pass reproduced both objects byte for byte: `priors_default.rda` 174987ce20985a54fac13c858f0456df, `.json` bbc4437a09d14a15c79362ea131be988; `config_default.rda` 1672781775d206f3b1ce1657ecebc0a0, `.json` dc0364b282b515c31d955bc4d8e16e89.
  - **The window.** Over 2023-01-01 to 2027-04-29 every time-indexed field equals v6.2 (`nu_1_jt` is now stored as double; its values are identical). The 2018-2022 head adds 42,180 case and 28,085 death cells: the window holds 77,392 and 62,034 (35,212 and 33,949 before), 1,275,087 cases and 22,360 deaths (832,887 and 14,347). The head's `reported_tier` location-days are 27,453 observed, 0 reconstructed and 14,727 imputed: 35% of its case cells are AI Fourier reconstructions (1.8% of the 2023+ cells), scored at their confidence weight (mean 0.81 over the head). `N_j_initial` is the 2018-01-01 population (sum 1.033e9; 1.173e9 before). `epidemic_peaks` grows from 64 to 97 rows (33 before 2023, none removed). `mu_jt` is 40 x 3,406 (median 2.05%), so the integrated deaths likelihood spans ten CFR years, 2018-2027, instead of five.
  - **Initial conditions** are re-estimated at 2018-01-01 (E/I from the 28-day window 2017-12-18 to 2018-01-14):
    - The quiet-start set grows from 16 to 18: BDI BEN BFA CAF CIV CMR GHA GIN NAM NER RWA SSD SWZ TCD TGO UGA ZAF ZWE. BEN, CMR and GIN join (no cases in their 2018 window, cases later); BDI joins because its window holds only imputed rows beside observed zeros; COG and TZA leave (their 2018 windows hold cases).
    - Expected initial E + I, N x (E[prop_E] + E[prop_I]) at the UN WPP population of each start: ETH 149 -> 1,022 (its window has no observed or reconstructed count and is read from its country-level reconstructions, `metadata$imputed_window_fallback`), KEN 1,676 -> 666 and ZMB 36 -> 1,105 (windows read without their imputed rows), BDI 54 -> 234 (seeded), RWA 247 (seeded, as in v17.1: its window holds only a regional reconstruction), MLI 4 (the near-zero template: its later cases are all imputed), NGA 297 -> 1,126, COD 2,580 -> 3,632, MWI 7,956 -> 225, MOZ 1,374 -> 134, SOM 1,037 -> 271, TZA 1,313 -> 633, AGO 4 -> 189. In the config, E + I is 15,882 people (23,184 before).
    - `prop_R_initial` moves in all 40 locations, median ratio 1.77 (IQR 1.47-1.87): mostly five fewer years of waning on older immunity, lower where 2018-2022 outbreaks were large (NGA 0.53, MWI 0.78, NER 0.84, CMR 0.85).
    - `prop_V1_initial` (16 locations) and `prop_V2_initial` (13) follow the campaigns before 2018-01-01: 19.8M -> 11.8M people in V1 and 13.4M -> 5.0M in V2. 27 locations carry the V1 template and 38 the V2 template (24 and 27 before; see Disclosures).
  - **Unchanged:** every global prior and every other location prior (`epidemic_threshold`, the `mu_jt` block, seasonal a/b, `tau_i`, `beta_j0_tot`, `psi_star_*`, `alpha_1` and the rest), and every point parameter of the config: their inputs and estimators are those of v17.1 and none reads the start date.
  - **The cases-dispersion panel trend** is refitted on v7.0 (observed weeks, `burn_in_days = 45`). With every fit of its own in, as the 0.103.0 development builds had it, it moved from intercept -0.355, slope 0.220, residual SD 1.19 on log k (22 locations, v6.1) to -0.512, 0.257 and 1.04 (23): BFA (0.62) and ZAF (0.94), with too few observed weeks, and NER (1.55) and ZWE (2.36), whose fits collapse, took it; CIV, CMR and UGA have fits of their own, and no fit is clamped. UGA's fit is censored and left out of the shipped trend (next bullet). Refitted from day 31 (the control default `burn_in_days = 30`) the shipped trend differs by up to 0.32 on log k (BFA; 0.014 on v6.1). The integrated deaths dispersion falls back to every scored week for BEN, BFA, CIV, LBR, MLI, NAM and ZAF (MLI is new).
  - Cases dispersion: a fit within its own 95% interval of the 0.1 lower bound (`k*exp(-1.96*se/k) <= 0.1`, status `near_lower_bound`) is censored like a clamped fit: it takes the panel trend at every scale and is left out of fitting it. On config_default v7.0 this is UGA (own fit 0.117, interval 0.079–0.173), which takes 1.34. The trend is re-derived without it (intercept −0.294, slope 0.226, SD 0.93, n 22): BFA 0.62→0.77, ZAF 0.94→1.11, NER 1.55→1.72, ZWE 2.36→2.49. Likelihood tag `R/v0.103.0+near_bound_k_trend`; resume refuses earlier shards. Near-bound deaths fits are likewise left out of the deaths shrinkage-trend fit (TGO on v7.0, which moves the other deaths NB k slightly in the 40-location panel, e.g. BDI 0.59→0.64); deaths take no panel trend, and `run_MOSAIC()` scores deaths with the integrated CFR core, so the deaths NB k is a diagnostic there.
  - Cases k moves with the 2018 window (national, burn-in 45, 0.102.0/v6.2 → 0.103.0/v7.0): COD 29.7→6.5, LBR 6.6→2.4, NER 3.6→1.7, CIV 0.98→0.42, CMR 1.65→0.93; KEN 0.27→0.60, ZMB 0.58→1.28, TCD 1.01→2.12, ZWE 1.07→2.49, ETH 3.07→5.20; UGA 0.96→1.34. Which locations take the trend depends on burn-in (production 45).
  - **Size.** `inst/extdata/config_default.json` 7.8 MB -> 16.6 MB, `data/config_default.rda` 0.70 MB -> 1.65 MB, about 11.7 MB in memory (5.2 MB before); the installed package grows from 17.9 Mb to 28.1 Mb (R CMD check).
  - `estimated_parameters` is unchanged apart from its creation date, and the toy endemic and epidemic configs (fixed 2020 windows) do not read `config_default`; none is rebuilt.

## Downstream consumers
- Code that runs on `config_default` itself gets the 2018 window. The engine's per-tick cost is flat, so a simulation costs about 2.2x as much (3,406 / 1,580 days), and so does every per-run array: member arrays, observation-level draws and the configs a parallel ensemble broadcasts to its workers.
- What the dependent repositories see depends on how they build their configs:
  - MOSAIC-Mozambique's config builders (`code/R/make_config_MOZ.R`, `subnational_sandbox/code/02_build_config_subnational.R`, `code/R/forecast_validation_cutoff_sandbox.R`) set their own window (2017-08-01) and read the all-dose vaccination file with `nu_2 = 0`: they get the delivery-dated timing of the doses, not the first/second-dose split.
  - MOSAIC-OCV builds on pinned calibrated models and inherits nothing until it rebases.
  - Forecast-CV psi caches predicted from 2023 (`prefit_rolling_cv_psi()`) are rejected by `run_rolling_cv()` against the 2018 config ("predicted from ..., after the config start"): it fails loudly. Re-run the prefit with `pred_date_start` at or before 2018-01-01.
- A 2023 start needs the 0.102.0 objects (config v6.2, priors v17.1) or a rebuild with `MOSAIC_BUILD_DATE_START=2023-01-01`, which must run the priors builder twice (see Builders): a single pass from v7.0 divides the 2023 V1/V2 by 2018 populations, about x1.13 too high.

## Vaccination
- OCV doses are split into first doses (`nu_1_jt`) and second doses (`nu_2_jt`) from the GTFCC campaign rounds, so a two-dose campaign no longer counts both rounds as first doses (about twice the distinct people immunised). Up to `config_default` v6.2 every dose went to `nu_1_jt` and `nu_2_jt` was zero; the limitation was documented in `data-raw/make_config_default.R` and mattered only for windows starting before 2023.
  - `process_GTFCC_vaccination_data()` adds `req_id`, `round_sequence` and `round_basis` (and `delivery_schedule`, below) to each request. `round_sequence` lists the request's rounds as `<dose>:<weight>` blocks in campaign order, R01 before R02 within a campaign, weighted by the doses administered in each round; `round_basis` is `"rounds"`, `"rounds_imputed"` (an unreported round count carries the mean reported round of the request, or all rounds weigh equally) or `"unknown"` (no Round events). `combine_vaccination_data()` carries the columns; WHO-only rows are `"unknown"`.
  - `est_vaccination_rate()` splits each request's daily doses across its round blocks in proportion to their weights, in order and in whole doses, and writes `param_nu_1_vaccination_rate_<suffix>.csv` and `param_nu_2_vaccination_rate_<suffix>.csv` beside the all-dose `param_nu_vaccination_rate_<suffix>.csv`, plus `doses_distributed_dose1`/`doses_distributed_dose2` in the redistributed data file. Doses with no round information are first doses. On every location-day `nu_1 + nu_2 = nu`.
  - `data-raw/make_config_default.R` reads the three files through `.vacc_nu_jt()`, which stops if a file is missing, does not cover the window, or `nu_1 + nu_2 != nu`.
  - With the split alone (ees-cholera-mapping 780eb54, before the delivery-dated release below), 2018-2022 carried 81.61M doses: 50.19M first and 31.43M second (38.5%). Of those doses, 89.6% had a fully reported round split, 6.4% an imputed share (MWI 2017-G03-D01, ZMB 2017-G07-D01, SSD 2018-I08-D01, ETH 2021-I01-D01) and 4.0% no round information (CMR 2022-I10-D01, MWI 2017-G03-D02 and a WHO-only MWI shipment). The shipped state, after the release, is in the last bullet of this section. No request delivered from 2023 has a second round, so over 2023+ `nu_1` equals the old `nu` on every location-day and `nu_2` is zero, before and after the release: the 2023-window `nu_1_jt` is identical to `config_default` v6.2's.
  - The split alone left the all-dose file value-identical. After the release below it differs from 0.102.0's on 1,987 location-days, all before 2023 (1,713 of them in 2018-2022), and its total is unchanged at 182.83M doses. The campaign files gain `req_id`, `round_sequence` and `round_basis` (the split) and `delivery_schedule` (the release); the redistributed file gains `doses_distributed_dose1`/`doses_distributed_dose2`, and its daily doses before 2023 move with the release.
- `est_vaccination_rate()` releases each GTFCC delivery on its own date. `process_GTFCC_vaccination_data()` sums a request's deliveries into one row, and up to v0.102.0 every delivery was spread at 20,000 doses a day from the request's first delivery date, so later deliveries -- typically the second round, or later campaigns of a GTFCC preventive programme -- were placed up to 2.6 years early (961 days, SSD 2019-G01-D01). Each request now carries a `delivery_schedule` (`<date>:<doses>` pairs; WHO-only rows have none and start at their campaign date): deliveries join a stock that is administered at up to 20,000 doses a day, and a request whose stock runs out resumes at its next delivery.
  - A request with one delivery, or whose next delivery arrives before its stock runs out, is unchanged, so the 2023+ series is identical on every location-day. 22 requests delivered before 2023 (52.2M doses) move later, by a dose-weighted mean of 81 days (SSD 2019-G01-D01 746 days, MWI 2017-G03-D01 278, UGA 2018-G03-D01 205; 1,713 location-days change in 2018-2022), and 2.37M doses move from before 2018 into 2018-2022 (MWI 2017-G03-D01 +1.87M, SSD 2017-G04-D01 +0.50M), where neither a 2018 window's initial conditions nor its `nu` saw them.
  - In the shipped files, 2018-2022 carries 83.99M doses: 51.91M first and 32.08M second (38.2%); 87.6% of them have a fully reported round split, 8.5% an imputed share and 3.9% no round information.

## Initial conditions
- `est_initial_E_I()` no longer back-calculates E/I from imputed (tier-3, AI Fourier) rows where the 28-day window holds an observed or reconstructed count (a reconstruction spreads a year's total along a seasonal shape: in the 2018 window ETH's December 2017 rows ran ~300 cases/week after an observed 61). A window without such a count reads its country-level reconstructions (`metadata$imputed_window_fallback`); regional reconstructions never count; the quiet-start test counts observed or reconstructed later cases only. `est_initial_E_I_location()` treats a day without a count as unobserved, not zero.
  - `est_initial_E_I()` is version 1.3.0, and `priors_default$metadata$imputed_window_fallback` records the fallback locations (ETH at 2018-01-01). Effect at 2018 in Default data objects.

## Builders
- `data-raw/make_config_default.R` builds at 2018-01-01 unless `MOSAIC_BUILD_DATE_START` is set. Its header now names the real floor, the psi prediction start (2018-01-01 for psi C3); it still said psi ran from 2010 and that a 2015 start was possible.
- `data-raw/make_priors_default.R` stops before any estimation when `MOSAIC_BUILD_DATE_START` is unset and the installed `config_default`'s `date_start` differs from the config builder's default, the counterpart of the config builder's check of `build_date_start`: without it, the first build after the default moves would produce priors for the old window. Export the variable for every step of a window move, and run the priors builder twice (see Default data objects).
- The `{quiet_start_seeded}` placeholder is filled in the newest `priors_default` changelog head only; older entries carry their literal lists.
- `model/LAUNCH_sanitized.R`: `DATE_START` is 2018-01-01, `DATE_STOP` is 2030-12-31 (the end of the shipped `nu` files, which step 2D writes), and step 4B carries the `est_suitability()` call of psi C3 (with `pred_date_start` 2018-01-01, the 2018 config's psi floor) instead of the stale "G" recipe and its TODO.

## Tests
- Pinned to the Sunday 2023-01-01 start, now derived from the config's own dates: the burn-in worker test and the reporting-week test of `test-calc_model_likelihood_weekly.R`; the synthetic psi caches of `test-prefit_rolling_cv_psi.R` and `test-review-cfr-cv-rolling.R`, which `run_rolling_cv()` rightly rejected against a config that starts earlier; `test-review-priors-seasonality.R`, which moved `date_start` without `date_stop`; and `test-review-runmosaic-score-window.R`, which assumed MOZ's 2023 peak on day 86 and skipped silently at 2018.
- `test-calibrate_psi_predictions.R`: noise-free boundary fixtures at the amplitude floor (slope 0.45 collapses, 0.55 is fitted), the floor follows `amp_range[1]` (slope 0.4 is fitted under `amp_range = c(0.3, 2)`), and the collapse test allows exactly one warning. Floors at 0.30 and 0.65 of the input sd and a hard-coded 0.5 passed the previous tests and fail these.
- New: `test-est_initial_E_I_tiers.R` (imputed rows in the E/I window), `test-vaccination-dose-split.R` (the dose split and delivery dating), and a contract test of the shipped objects in `test-config_default.R`: `build_date_start` equals the config's `date_start`, the config's R, V1, V2 and S proportions sit at the priors' Beta means (a config built against first-pass priors fails it), and `nu_1_jt`/`nu_2_jt` are whole and non-negative with `nu_2_jt` zero from 2023.
- `config_default`'s censored-fit dispersion test runs on v7.0 again, on UGA's near-bound fit (no fit is clamped there); synthetic tests carry the clamped and the near-bound rules.
- Full suite on v7.0 / v18.0: 1,863 tests, 14,370 expectations passed, 0 failed, 38 skipped.

## Documentation
- The PDF reference manual builds again. The roxygen of nine topics carried Unicode maths (Greek letters, the approximately, at-least and not-equal signs, subscript digits) that LaTeX rejects; they are now `\eqn{}` forms or words, and `?prefit_rolling_cv_psi` no longer nests `\code{}` inside `\eqn{}`. The release checks had run with `--no-manual`, which hid the error.
- `?priors_default` documents the metadata fields `build_date_start`, `quiet_start_seeded` and `imputed_window_fallback`.

## Changelog corrections
- The 0.102.0 Suitability entry is corrected here rather than in place:
  - The amplitude floor is exactly a cutoff on the clamped slope: below `amp_range[1]` the fit is not applied (identity). It does not test whether the slope is identified, and its output jumps at the cutoff. "It binds where the outbreak weeks do not identify a slope" describes C3's four floor countries (outbreak-week slope t of -2.1 to 1.6), not the rule.
  - The old floor map came from the guard constants only for CIV and GMB (slope 0.505 and offset -2.64, 0.66 times the slope and offset clamps 0.25 and -4). UGA's slope was clamped but its offset fitted (-1.21), and TGO's map (0.501, 0.624) was an unclamped fit blended to the floor.
  - A fit above the 2x ceiling is blended toward identity at the 0.02-grid weight nearest the ceiling, which can land slightly above it (C3: SWZ 2.02, ZAF 2.005).
  - The roxygen, the collapse warning, `model/README_psi_provenance.md` (which now points at the MOSAIC-data bundle `processed/psi_provenance/v0.102.0_C3_recorrected/`, 4957df4, instead of laptop paths) and the v6.2 entry of `config_default`'s changelog are corrected.
- The v17.1 entry of `priors_default`'s changelog, whose correction 0.101.0 promised for the next rebuild, now gives the cause of the tighter seasonal SDs: the fits' standard errors fell (ZAF's from 0.52 to 0.21 before the envelope scaling, which itself rose from 0.41 to 0.46).

## Disclosures
- At the simulation start the initial vaccinated compartments and the vaccination series can count the same doses. `est_initial_V1_V2()` counts every OCV round administered before t0 in full (or, without a reported round, the request's deliveries before t0), while `nu_jt` releases each request's shipped doses from their delivery dates at 20,000 a day, so the part released after t0 is counted again. At the 2018-01-01 start of config_default v7.0 this concerns NGA 2017-I14 (256,900 doses after t0, all second doses, which add no protected people) and SSD 2017-G04 (37,800 doses, about 30,000 people, 0.3% of the population). In the 2023-01-01 objects (config v6.1/v6.2, priors v17.1) behind the v2026-10.02 and v2026-10.03 suites it concerns four single-dose requests: MWI 2022-I13 (1.80M doses, about 1.41M people, 6.7% of the population), CMR 2022-I17 (0.94M, 2.6%), KEN 2022-I21 (0.56M, 0.8%) and SOM 2022-I15 (0.26M, 1.1%); those runs started with that much extra vaccine immunity, delivered in January-March 2023 during their early-2023 outbreaks. To be fixed after 1.0 by deriving the initial V1/V2 from the same release series.
- Pre-existing approximations in the initial V1/V2, to be fixed after 1.0 by an expected-value replay of `sim_phase_vaccinated()`:
  - the template priors place about 8M people in V1/V2 at the 2018 start where the campaign log has no doses: V1 Beta(0.5, 49.5), mean 1%, in the 27 locations without doses before t0 (4.1M people), and V2 Beta(0.5, 99.5), mean 0.5%, in the 38 without second doses (4.05M);
  - rounds filed under different requests are not paired (NGA 2017-I11/I14, about 0.66M people; ZMB 2016-I04/I08, about 0.22M);
  - two-dose campaigns whose round doses are unreported enter the initial V1 entirely as first doses, while the dose series imputes their split (about 55K people in SSD at 2018);
  - the pairing's V2 is 12-21% below what the engine produces from the same doses.
- Known limitations of the 2018 start:
  - **Re-ignition.** The engine has no importation term. In the planning analysis of the processed surveillance (`claude/plan_2018_start/reignition_gaps_national.csv`, not the shipped objects), 22 of the 28 national models must re-ignite at least once after 52 or more silent weeks (14 at a 2023 start), with gaps of up to 5-8 years (CAF 435 weeks, CIV 387, BFA 378); the quiet-start seed stands in for undetected circulation. The G2 window pilot is the gate for the 2018 suite.
  - **Pre-2023 surveillance.** 35% of the 2018-2022 case cells are imputed, and surveillance deaths are 0.79 of the WHO annual totals over 2018-2022 (CMR 0.28, KEN 0.40, ZWE 0.49), against 1.02 over 2023-2025. A calibration on `config_default` scores deaths over the whole window unless `control$likelihood$deaths_score_start` is set.

# MOSAIC 0.102.0

## Default data objects
- `config_default` v6.2 changes `psi_jt` in four countries only: CIV, GMB, TGO and UGA. Their production psi (C3) sat on the bias correction's old 0.5x amplitude floor. Under the new collapse rule (see Suitability) they fall back to the identity correction, so their psi is the LSTM's own smoothed prediction. C3's stored predictions were re-corrected without retraining; the other 36 countries are byte-identical.
  - Mean psi over the window: CIV 0.011 -> 0.029, GMB 0.007 -> 0.010, TGO 0.256 -> 0.068, UGA 0.091 -> 0.123.
  - TGO sat on the floor in C3 only, of the seven 2026 refits, so its level is borderline.
  - Every other field is identical to v6.1, and the build is byte-reproducible.
  - `priors_default` stays at v17.1, because the priors do not read psi. The cases-dispersion panel trend reproduces exactly on v6.2. The toy configs and `estimated_parameters` do not read psi and are unchanged.

## Suitability
- `calibrate_psi_predictions()` no longer applies a per-country fit whose slope would shrink psi's logit-scale amplitude below `amp_range[1]` (0.5) of the model's. Such a country now falls back to the identity correction, as it already did with too few outbreak weeks or a degenerate predictor. Its diagnostic status is the new `"collapsed"`, and a warning names it.
  - **Old behaviour.** Up to v0.101.0 such a fit was blended toward identity until it sat on the 0.5x floor. The slope was clamped without re-fitting the intercept, so the map came from the guard constants rather than the data.
  - **Evidence the old map was not an estimate.** CIV received slope 0.505 and offset -2.64 in all seven 2026 production refits. That took the square root of its seasonal odds contrast and put its psi about 4x below the median target of its outbreak weeks.
  - **When the floor binds.** It binds where the outbreak weeks do not identify a slope. On C3's stored 2018+ window, the four floor-clamped countries have outbreak-week slope t of -2.1 to 1.6; every fitted country has 3.4 or more.
  - **Evidence for identity.** Against the training target, in-sample, the LSTM's own amplitude is close to calibrated: the median all-weeks calibration slope is 1.15 over 29 countries. Out of sample, forward-chained over the rolling-CV fold predictions, identity and the floor map are indistinguishable.
  - **Unchanged.** Fits that would inflate the amplitude are still shrunk to the 2x ceiling, and every other country's correction is bit-identical.
  - **Production psi.** On C3 the rule changes CIV, GMB, TGO and UGA; it is re-applied to C3's stored predictions without retraining in `claude/v0102_psi/`.

# MOSAIC 0.101.0

Surveillance artifact fixes, cases dispersions estimated from observed weeks (when the config carries `reported_tier`) with a cross-country trend for locations without a usable estimate of their own, an optional weekly cases scoring rule (the default stays one cell per day), observation-level predictive intervals, the default data objects rebuilt on the corrected surveillance (`priors_default` v17.1, `config_default` v6.1 with `reported_tier`, psi C3), and the figure and documentation fixes prepared for 1.0.0. **Calibration results change**: the resume guard refuses to pool simulations scored by earlier versions (likelihood tag `R/v0.101.0+clamped_k_trend`).

## Default data objects
- `priors_default` v17.1 and `config_default` v6.1 are rebuilt on the corrected surveillance, the re-estimated seasonal dynamics and a new production psi; each was built twice, byte-identical, and the window stays 2023-01-01 to 2027-04-29. Data provenance: `priors_default` v17.1 and the panel psi C3 was trained on were built on MOSAIC-data 04a6d0f, and `config_default` v6.1 on 922ef89, which differs from 04a6d0f only in two ZAF 2023 deaths cells (weeks 21 and 23); the priors rebuilt on 922ef89 are byte-identical, and the panel compiled on 922ef89 differs from C3's only in those two deaths cells, which psi never reads. Other inputs: ees-cholera-mapping 780eb54, open-meteo-pipeline fddf00f, enso-data 3729c87.
- `psi_jt` is the production refit C3: `est_suitability()` lstm_v2 (feature set v7.3, target D), 10 seeds 11-110, fit 2015-01-01 to 2026-09-17 on the corrected surveillance with backfill off. A disjoint 10-seed replicate (seeds 121-220) agrees over 2023+ at a median per-country r of 0.972 and a median per-country mean |difference| of 0.024 (0.025 pooled over countries). Against the v0.100.1 psi over the window, the median per-location r is 0.92 and the median per-location mean |difference| 0.042 (0.050 pooled); the mean level moves most in CIV (x0.04), ZAF (x0.50), GHA (x1.71), TGO (x2.42) and SWZ (x3.17).
- C3's ENSO input is the Nino 4 NMME gap-fill variant of the canonical enso-data 3729c87 compile. NOAA PSL Nino 4 ends in August 2026, so the compile anchored 1 September on BOM's relative Nino 4 index, which sits about one degree below NOAA; that anchor (0.49, between 1.29 for August and 2.13 for October) put a spurious dip into August-September 2026. The variant takes the anchor from the compile's next source, the NOAA-baselined NMME ensemble mean (1.88). This changes 10 weekly ENSO4 values and is closer to NOAA CPC's weekly observations (mean absolute error 0.28-0.34, against 0.41-0.47). The provenance bundle (the variant ENSO file, derivation scripts, md5s, panel compile and fit commands, repository revisions) is in MOSAIC-data `processed/psi_provenance/v0.101.0_C3/` (b5feb59).
- The quiet-start seeding prior covers 16 countries instead of 11: SSD, TZA, UGA, ZAF and ZWE join, because the cases in their 28-day window around 1 January 2023 were AI Fourier reconstructions, which the reconciliation removed (none had an observed case in the window). The seeded countries start with about 25 (SWZ) to 1,332 (TZA) expected people in E + I; for the five, expected initial E + I moves SSD 45 -> 230, TZA 231 -> 1,332, UGA 4 -> 973, ZAF 93 -> 1,264 and ZWE 551 -> 327. Window-based priors follow their window's cases in AGO, BDI, BEN, COD, ETH, KEN, MWI and ZMB, from x0.06 (ZMB, whose AI rows were removed) to x1.34 (KEN, where a WHO report is now spread into the window), prop_R and prop_S move by at most 0.51% (ZAF's prop_R), and the config's initial R, S, V1 and V2 counts move by at most 0.42% through those priors and the row normalisation (V1 and V2, whose priors are unchanged, by up to 0.41% in 9 locations). In 300 `sample_parameters()` draws every seeded country starts with E + I >= 1 in every draw except SWZ (298); the lowest window-based country is AGO (295).
- `epidemic_threshold` moves in 18 countries with the outbreak weeks of the corrected surveillance. ZAF leaves the Zheng fallback: the shaped 2023 window gives it 26 outbreak weeks (7 before; 10 are needed), so its threshold falls x0.009, from 1.18e-5 to 1.07e-7 Isym/N, at its own median weekly incidence (0.006 per 100,000). The threshold only switches the case-reporting PPV from chi_endemic to chi_epidemic. The other 17 move x0.61 (GHA) to x1.28 (RWA).
- The seasonal priors and config coefficients follow the re-estimated seasonal dynamics in 16 countries (see Surveillance); the share of seasonal prior draws whose envelope dips below zero is 32% (33% in v0.100.1). Where the SDs tighten (ZAF 0.30 -> 0.14, CIV 0.22 -> 0.14), the cause is a much smaller standard error from the re-fitted case regression, not the envelope scaling that `priors_default$metadata$description` credits (to be corrected at the next rebuild): ZAF's envelope scale rose (0.41 -> 0.46) while its fit's standard error before scaling fell 2.5x (0.52 -> 0.21), because the one 2023 report that holds 99% of the cases in its fit window is now spread along the outbreak's curve (20 weeks with cases) instead of booked in a single week (CIV's fell x0.70, NAM's x0.28-0.39).
- `config_default` carries `reported_tier` (33,209 observed, 1,365 reconstructed and 638 imputed location-days; NA where unobserved), so the cases dispersion of a run on the default config is now estimated from observed weeks, and the deaths dispersion too where they are enough (see Likelihood). Its `metadata$description` says both dispersions "use tier-1 weeks only"; the deaths dispersion falls back to every scored week where the observed weeks are too few (for the integrated deaths likelihood at `burn_in_days = 45`: BEN, BFA, CIV, LBR, NAM and ZAF), and the description will be corrected at the next rebuild. `reported_cases`/`reported_deaths` follow the corrected surveillance: observed case cells 36,984 -> 35,212 (490 NA -> value, 2,262 value -> NA) and 1,627 values change; window cases 845,234 -> 832,887 (ZAF 3,086 -> 1,404, CIV 1,016 -> 505, SSD -13%); observed data now run to 2026-09-20 (2026-08-23). `config_default$epidemic_peaks` 62 -> 64 rows (161 peaks).
- The panel trend of the cases dispersion (`.NB_DISP_PANEL_TREND`) is refitted on `config_default` v6.1, on observed weeks at `burn_in_days = 45`: intercept -0.195 -> -0.355, slope 0.143 -> 0.220, residual SD 1.56 -> 1.19 on log k, 23 -> 22 locations. BFA (0.89), CIV (0.98) and ZAF (1.10), with too few observed weeks, CMR (1.65), whose fit collapses, and UGA (0.96), whose fit sits at the lower bound of 0.1 (see Likelihood), take it; on v6.0 it was CMR, UGA and ZAF (1.41, 1.00, 1.20). Refitted at the control default burn-in (30) it moves k by at most 0.014 on log k.
- Unchanged: N, birth and death rates, `nu_1_jt`/`nu_2_jt`, `mu_jt` (the CFR GAM input is identical), beta_j0, psi_star, tau_i, mobility, theta_j, every other point parameter apart from the initial conditions above, and every global prior. The toy endemic and epidemic configs, `estimated_parameters` (v1.3.0) and the suitability region maps are unchanged.

## Surveillance
- `process_WHO_weekly_data()` spreads WHO multi-week reports. The dashboard enters 0 for "no report" as well as "no cases" and books late or batched reports in the week they arrive, so a report of at least 20 cases after a silent week of the same epi year, followed by a fall (at least twice each of the next four reported weeks, or the next two reported weeks zero), is spread over the silent weeks back to the previous non-zero report, never before week 1. A report that passes only the first test covers at most four reported zeros, unless at least two of the next four reported weeks are zero (batch reporting), so an explosive onset after a long silence is not spread. Counts stay whole by cumulative rounding, halves rounded up so that every week gets the floor or ceiling of its share for any weights, and every country-year total is unchanged. New columns record the as-published counts and the window (`cases_reported`, `deaths_reported`, `catchup_*`), a `confidence_weight` by window length and `disaggregation_method = "who_catchup_uniform"`.
- Documented windows come from a curated table, `inst/extdata/surveillance_curation.csv` (`who_catchup_curated`): NAM 2025 (from the week of the first case, week 9), CIV 2025 (weeks 30-33), NGA 2023 (weeks 46-52) and ZAF 2023 (the 1,390 cases and 47 deaths WHO booked in week 35, over 1 February - 31 July: the outbreak period of the Department of Health's statement of 5 July 2023 and the date of WHO AFRO's after-action review count).
- A curated window can follow documented curves: a new `shape` column, with cumulative-count anchors in `inst/extdata/surveillance_curation_shapes.csv` (a case curve and an optional deaths curve; without one, deaths follow the case curve). Each Monday-Sunday week gets the curve's increment, linear between anchors; the curve is rescaled to the report but must count between 0.5 and 1.02 times it. ZAF 2023 follows WHO's epidemic curve of the outbreak (external situation report #5, Figure 5, digitized) moved from symptom onset to report dates, 2 days later (the onset-to-notification lag of situation report #4's notification-date curve), so it is dated like the other WHO rows for the model's single reporting delay; the imported case of 14 July is placed by the same rule. Its weeks of 15, 22 and 29 May carry 220, 432 and 249 cases. Its deaths follow their own curve, the national counts reported by date (the first in February; 10 and 14 in the weeks of 15 and 22 May; 47 by 4 July). These rows are `who_catchup_curated_shaped` (reconstructed, confidence 0.9), and the combiner does not reshape the window from another source; `data-raw/make_surveillance_curation_shapes.R` rebuilds the anchors from the WHO report.
- WHO epi weeks run Monday to Sunday and are stamped with their Monday, as the WHO dashboard states; the code and documentation assumed Sunday to Saturday. A curated date now falls in the WHO week whose Monday is on or before it, so NAM 2025's first case (Sunday 2 March) is in week 9 and its 22 cases cover weeks 9-12 (6/5/6/5) instead of weeks 10-12; no other window moves. Rounding the spread halves up moves one count between weeks in 16 cases or deaths series of 10 windows (BDI, CIV, GHA, KEN, NGA, TZA, ZMB), totals unchanged.
- `process_cholera_surveillance_data()` ranks sources observed > reconstructed (`who_catchup_*`) > imputed and removes cross-source double counting. A WHO window is kept whole: non-WHO rows repeating the dashboard value are dropped, a source with a positive count in every week of the window shapes the WHO total (`who_catchup_shaped`; a curated shaped window keeps its curve), and other rows in the window are absorbed. AI weeks that are aggregates are dropped: an observed week of at least 20 cases at five times every direct-source week within four weeks (NGA 2023 week 21, the year-to-date total), or within 15% of a WHO weekly cumulative of its year (COG 2023 week 29). Imputed rows only fill the gap between the observed weeks and the WHO account of the year (the AFRO annual total, else the WHO weekly year-to-date total), in every year; 349 country-years change and pre-2023 imputed cases fall from 3.77M to 3.32M. Rows rescaled below half a case are emptied. Documented absences remove imputed rows (SSD 2023-05-17 to 2024-09-27, AGO 2023) and BFA 2025 is flagged. Every changed week is listed in `cholera_surveillance_weekly_adjustments.csv`.
- `update_mosaic_data()`: a failed WHO annual step now blocks the surveillance combiner that depends on it, and skipping that step warns.
- `est_epidemic_peaks()` counts spread WHO windows as observed (blanking them carved troughs into outbreaks). `epidemic_peaks` and `param_epidemic_peaks.csv` are rebuilt from the regenerated surveillance, 159 -> 161 rows: the ZAF 2023-08-31 dump artifact and the GHA 2024-11-18 peak that rode on a dump are gone, the documented GHA 2024 (peak 2024-12-08) and CIV 2025 (2025-07-07) outbreaks are added by hand because the spread reports leave their smoothed curves below the detector's prominence threshold, TCD 2026-08-23 is new, the shaped ZAF 2023 window gives the Hammanskraal outbreak a peak (2023-05-15 to 06-12, peak 05-29), and the COD 2026 peak moves from 08-02 to 08-30 with the revised WHO weeks.
- `compile_suitability_data()` no longer backfills short case gaps by default (`backfill_case_gaps = FALSE`): the lstm_v2 suitability model already leaves unobserved weeks out of its training targets, so a filled week only added an interpolated target, and 15 of the 26 weeks filled in the v0.101.0 panel were imputed weeks the reconciliation had emptied. Turning it off leaves those 26 rows without a target, 17 of them inside the 2015+ suitability fit window. With the fill on (it serves the frozen legacy suitability path, which reads unobserved weeks as zeros), a filled week is labelled `backfill_interpolated` (imputed), carries a confidence weight of at most 0.5 and never sets a target anchor (two filled 2009 weeks had set Botswana's), and a deaths-only week is not filled.
- The seasonal dynamics (`param_seasonal_dynamics.csv`, `pred_seasonal_dynamics_day.csv`) are re-estimated on the corrected surveillance (MOSAIC-data 922ef89, the same fit as on 04a6d0f) and ERA5 to 2026-09-21, with unchanged arguments. Against v0.100.1 the cases-response coefficients move in 16 countries, by more than 0.03 only in ZAF (max 1.41: its transmission envelope 1 + f peaked on 31 August at 2.59, the week-35 dump of 1,390 cases, and now peaks on 28 May at 2.51, the Hammanskraal outbreak), NAM (0.13) and CIV (0.11), where the curated windows landed. The precipitation-response coefficients and the neighbour assignments are unchanged, and the envelope stays at or above 0.1 in every country.

## Likelihood
- `calc_model_likelihood()` gains `cases_scoring` and `week_offset`, and `run_MOSAIC()` reads `control$likelihood$cases_scoring`. The default, `"daily"`, is the cell rule of 0.100.1: one negative binomial cell per day at the weekly k. The added `"weekly"` rule scores reporting-week totals instead: the observed and simulated daily cases are summed over the reporting weeks the dispersion is estimated on and each week is one negative binomial cell at the weekly k. The surveillance is weekly totals spread over days, so the daily rule counts a week's level information 7(k + M)/(7k + M) times (median 5.4 over the v0.100.1 national runs) and ranks draws by within-week noise, which the weekly rule removes. The pre-registered likelihood gate (KEN, ZMB, CMR and GHA, 30,000 simulations each under both rules with the same data, dispersions, intervals and seeds) kept `"daily"` as the default, because `"weekly"` was worse on both counts of its direct test of the rule: geometric-mean cases rWIS(weekly/daily) 1.068, and median |log cases bias| 0.254 against 0.187. `"weekly"` stays available and tested while that result is investigated. `"daily"` runs at this version's dispersion, so it does not reproduce a 0.100.1 run: collapsed and clamped fits take the panel trend, `reported_tier` restricts k and (where they are enough) the deaths phi to observed weeks, and the intervals and the cases central line changed (below). Matching a 0.100.1 likelihood also needs that run's `nb_k_cases` and a config without `reported_tier`. Without `ll_deaths_core` the negative binomial deaths core follows `cases_scoring` too (weekly deaths totals on the same weeks under `"weekly"`).
- Under `"weekly"`, a week is scored only when its seven days are in the scored window with a finite observation and weight; its weight is the mean of its days' weights, made mass-preserving over the scored weeks. A simulated count that is not finite on a day of a scored week makes the cases score -Inf (a failed path; dropping the week gave it the best score); the daily rule keeps its earlier treatment, leaving a day with a missing simulated count out. A location short of three scored weeks (weighted: a weight sum of three), with no cases shape term on and no deaths to score, is NA like a location without data, not a constant 0, and `run_MOSAIC()`'s pre-flight check then counts complete weeks too. The shape terms keep their daily definitions, so under `"weekly"` a given shape weight weighs several times more against the cases core (measured below), and the cumulative term (off by default) scores its sums at size `k * n / 7`, `n` days being `n / 7` weekly totals. For the same reason, under `"weekly"` the deaths, already scored weekly, carry several times more weight against the cases in draw ranking at the default outcome weights: on the v0.100.1 national re-selection pools (real engine draws, production seeds), the spread of the cases score across draws shrinks by a median 4.8 (range 1.8-6.5) over all draws and 4.1 (2.9-6.1) among the top 1,000 for the 11 countries whose k is unchanged. There is no change in the Poisson limit (LBR), and the shift runs the other way where the panel trend raised k (CMR and UGA in those pools). Under the default daily rule the cases score keeps the per-day spread of 0.100.1, changing only where the dispersion changed, so this shift toward deaths does not apply at the default.
- The cases dispersion is estimated from observed weeks only when the config carries `reported_tier` (a new location x day matrix: 1 observed, 2 reconstructed, 3 imputed, built by `make_config_default.R` and subset by `get_location_config()`): `est_nb_dispersion()` gains `obs_tier`, and reconstructed or imputed weeks, whose synthetic shape reads as low noise, leave the fit. A location without a usable estimate of its own (a fit run to the zero boundary, now detected; a non-finite SE; failed mean models; a fit clamped at the lower bound of 0.1; or too few observed weeks in a series that has enough overall) takes a cross-country trend of log k on log mean weekly cases, at every scale (`est_nb_dispersion(panel_trend = )`; `run_MOSAIC()` supplies the trend fitted on `config_default` at `burn_in_days = 45`, whatever the run's burn-in, and a drift test re-derives it at each rebuild; the shipped trend, the locations of `config_default` v6.1 that take it and the effect of refitting it at the control default of 30 are under Default data objects). A collapsed fit was previously reported at the 0.1 bound. A clamped fit is censoring, not a measurement: most of UGA's 10 non-zero observed weeks are the edges of short outbreaks whose middle weeks are reconstructed and left out, and on synthetic series of that shape the fit returned the bound in 22-29 of 40 whether the true reporting process was Poisson, k = 1 or k = 5 (median 0.100 each time). At k = 0.1 a twofold level error cost UGA's cases score 3.1 nats over its two observed years under `"daily"` and 0.5 under `"weekly"` (22 and 4.6 at the trend's 0.96), and the observation-level predictive median of a weekly total was 0 for any weekly mean up to 102. The row keeps `status = "clamped_lower_bound"` with `panel_trend = TRUE`. On `config_default` v6.1 only UGA moves, from 0.100 (national and eastern runs), 0.105 (central) and 0.108 (continental) to 0.96 in all four; every other location's k, in every suite scope, and the deaths dispersions are unchanged. The trend fit already left clamped fits out, so the shipped trend is unchanged, and with shrinkage on a routed location is a trend taker rather than a shrunk estimate (it no longer enters shrinkage with a delta-method variance computed from the bound). The deaths NB dispersion (a diagnostic in `run_MOSAIC()`) takes no trend: a deaths location whose observed weeks are too few keeps its every-week estimate rather than the Poisson limit. `nb_dispersion.csv` gains `n_weeks_excluded` (0 where no fit is attempted) and `panel_trend`, `summary.json` gains `n_panel_trend`, and the resolver reports `tier_used` per channel (`FALSE` for a user-supplied k).
- The integrated deaths likelihood estimates its quasi-Poisson dispersion phi from the observed weeks as well, when the config carries `reported_tier`. A location whose observed weeks alone are too few keeps the every-week estimate rather than the Poisson limit. Every scored week is still scored, and the observation-level deaths draws use the same phi. A location whose scored weeks span one calendar year is now estimated with a single year effect (`D ~ offset(log C)`) instead of falling to phi = 1, which glm's one-level year factor forced; on `config_default` v6.1 this moves CAF from 1 to 1.65 and NER from 1 to 2.97 (at `burn_in_days` 45 and 30), and no other location. A config without it (a user config, or `config_default` before v6.1) estimates both dispersions from every week, and the run log says so ("dispersion from every week (config carries no reported_tier)").

## Ensemble and predictions
- `calc_model_ensemble(observation_model = )` draws observation-level posterior predictive intervals. Each member's weekly cases are drawn from a negative binomial with the weekly k the likelihood scored with and apportioned to days in proportion to the member's daily cases; deaths get the integrated deaths likelihood's quasi-Poisson variance around the member's expected deaths. `run_MOSAIC()` supplies its dispersions. On the v0.100.1 national rehearsal the median weekly 95% coverage rises from 0.865 to 0.992 and 50% coverage from 0.365 to 0.698.
- `ci_bounds`, the new `predictive_median`, `cases_array` and `deaths_array` are observation-level; the member trajectories are kept as `cases_engine_array` and `deaths_engine_array`, which the medoid, trajectories, implied CFR, subset optimization and R_eff use. The central lines stay engine-level: the noise is mean-preserving, and a median of observation-level draws collapses where k is small. Without `observation_model` the ensemble is engine-level as before. The cases draw is the weekly negative binomial at the scored k under either cases rule, so under the default `cases_scoring = "daily"` a run's intervals are wider than its per-day likelihood implies (weekly variance about C + C^2/k against C + C^2/(7k)); under `"weekly"` they match it. Either way they are not the engine-level intervals of earlier runs.
- In the prediction CSVs, `predicted_median` is the median of the same draws as the `ci_*` columns: the observation-level `predictive_median` for a channel that received observation noise, else the engine median as before. Every row's quantiles therefore nest (`ci_1_lower <= ci_2_lower <= predicted_median <= ci_2_upper <= ci_1_upper`) and a WIS computed from the CSV pairs a median with intervals from one set of draws. `predicted_central` (the line drawn and scored) and `predicted_mean` stay engine-level. Where k is small the predictive median is the typical observed count and falls far below the central line.
- The default central line is the weighted median for cases and the weighted mean for deaths (`control$predictions$central_method`; both mean in 0.98.0-0.100.x). The `central_method` argument defaults of `run_rolling_cv()` and `plot_model_ensemble()` change the same way (both were `"mean"`), so a `run_rolling_cv()` call that leaves it unset now scores cases on the median. `run_rolling_cv()` predictions gain `pred_median_obs`, which its WIS pairs with the observation-level intervals. Its medoid (and legacy best) rows are re-simulated through `calc_model_ensemble()` with the cutoff run's observation model (the weekly k its candidate ensemble recorded) and deaths integration (`deaths_integration.rds`), so every model's rows in one compile carry observation-level intervals and CFR-redrawn deaths; they were plain engine reruns, with engine-level intervals and deaths at the config's CFR. The reruns are seeded as the run's medoid ensemble is (1001, 1002, ...), so recompiled medoid rows change.

## Prediction figures
- `plot_model_ensemble()` draws the predicted central line and intervals from the first time step. It blanked every step before the scored window (the burn-in and the two-step cases warm-up), so with the production `burn_in_days = 45` the ensemble and medoid figures started on 2023-02-15 although the ensemble holds predictions from 2023-01-01. The unscored steps are now shaded light grey, with a dashed line at the start of the scoring window labelled with its date ("scored from 2023-02-15"; one label per channel when cases and deaths start on different steps). The marker is where the caption R2, bias and totals and the calibration's scoring window begin; under `cases_scoring = "weekly"` the cases likelihood scores the complete reporting weeks inside that window, so its first scored week can start up to six days later. New argument `show_burn_in` (default `TRUE`; `FALSE` draws the previous figure); `render_MOSAIC_figures()` passes `TRUE`. Display only: the caption R2, bias and totals are computed on the scoring window as before, and the exported `predictions_*.csv` files keep their unscored rows `NA`. A supplied `prediction_table` is drawn as given, with its blank unscored rows filled from the ensemble; its `central_method` column now also sets the caption's central label and the series its R2, bias and totals are computed on (an explicit `central_method` argument that disagrees draws a warning), since the line drawn is the table's.
- For an ensemble built with an observation model (a `run_MOSAIC()` ensemble since this release) the ribbons are its observation-level predictive intervals, in the burn-in head too, and the caption says "observation-level predictive intervals". The line stays the engine-level central trajectory (for cases by default the weighted median of the member trajectories, before observation noise), so where the reporting dispersion k is small it can lie above the observation-level 50% band; the captions say this too.

## Documentation figures
- `plot_seasonal_transmission()` and `plot_seasonal_transmission_example()` take the years in their legend labels from the dates of the points drawn, i.e. the `est_seasonal_dynamics()` fit window ("Precipitation (2010-2025)"; the Mozambique example's cases read 2017-2025), instead of the hardcoded "1994-2024" and "2023-2024".
- `plot_seasonal_clustering()` reads the daily fits `est_seasonal_dynamics()` writes (`MODEL_INPUT/pred_seasonal_dynamics_day.csv`) and clusters and draws their weekly means (the scale its clustering options, including dbscan's fixed `eps`, were set for). On the shipped fits, `clustering_method = "ward.D2"` with `k = 4` reproduces, for all 40 countries, the four clusters the estimator computes from the daily fits for its neighbour inference. The title's years come from the fit window instead of a hardcoded "2014-2024". The weekly `DOCS_TABLES/pred_seasonal_dynamics.csv` it required is no longer written by anything; it is still read when the daily fits are absent (clustered the same way, ward.D2 with k = 4, its 2024-11 copy puts 5 of the 40 countries in a different cluster from the daily fits under the best one-to-one matching of cluster labels). It returns the plot, each country's cluster and the file read, invisibly. Its `set_inferred_to_na` documentation is corrected: the argument applies to the cases clustering only (default `TRUE`) and is ignored for precipitation.
- `plot_CFR_by_country()` evaluates the Beta densities on an adaptive grid, so the AFRO Region density (SD about 1e-4) is drawn instead of falling between grid points; the x axis spans the plotted densities and the y axis is on a square-root scale. It returns its two plots invisibly, as documented.
- `plot_vaccine_effectiveness()` subtitles panels B, C, E and F as data fits, and its documentation states that they are the `est_vaccine_effectiveness()` fits, not the phi/omega priors (whose SDs are 2.3 and 4.9 times larger for phi_1 and phi_2, 1.1 and 1.2 times for omega_1 and omega_2). Nothing it computes changes; it returns the figures and panels invisibly.

## Fixes
- A re-run into a directory that holds a finished run no longer draws the earlier run's central line. The renderer reads `3_results/summary.json`'s `central_method_*` first, and `run_MOSAIC()` writes that file only after the in-run render, so the ensemble and medoid figures took the earlier run's central method (every default re-run of a 0.98.0-0.100.x directory under the new default). `run_MOSAIC()` now removes the earlier `summary.json` with the other post-calibration artifacts and passes the central method it resolved to the in-run render.
- An invalid `control$predictions$central_method` (an unnamed `c("median", "mean")`, a misspelling, an unknown channel) is rejected when the control is validated. It was first resolved after calibration and shard consolidation, which ended the run with no ensemble in a directory that cannot be resumed.
- `compile_rolling_cv_predictions()` recompiles a manifest without `central_method` (written by `run_rolling_cv()` v0.32.40-v0.37.x, before the setting existed) on the ensemble median those runs predicted; it fell back to the mean.
- `man/process_WHO_weekly_data.Rd` is regenerated from its source, and a hand-escaped percent sign that printed "15\" in `process_cholera_surveillance_data()`'s manual is fixed.
- `test-get_ggplot_legend.R` and `test-plot_model_likelihood.R` no longer leave `Rplots.pdf` in the test directory.

## Changelog corrections
- The 0.100.1 entry is corrected in place. `fit_beta_from_ci()` keeps the requested mode but does not hit both requested 95% bounds (p_beta 0.144-0.596 for 0.10-0.50; the realized intervals are listed). The initial-infection probabilities are the measured 300-draw values (lowest UGA, 294 of 300), the seeding prior implies about 25-680 expected people in E + I (not ~50-700), seeded single-location runs report a case within 60 days in 87-100% of 100 draws (not 90-100%; 0-4% before the seeding prior), and the ~90% ignition failure it describes was in a first v17.0 build, not in the shipped v16.1 priors.

# MOSAIC 0.100.1

Data-object rebuild on the v0.100.0 estimators and the corrected, refreshed data (MOSAIC-data 64b69ab, 6f41244). **Calibration results change.**

## Default data objects
- `priors_default` v17.0 and `config_default` v6.0 are rebuilt; the window is 2023-01-01 to 2027-04-29 on a new 10-seed production psi (seeds 11-110; an independent 10-seed replicate agrees at median r 0.96 over 2023+). `config_default$date_stop` moves from 2027-02-04 to 2027-04-29.
- sigma prior Beta(4.30, 13.51) -> Beta(3.75, 7.12) (mean 0.24 -> 0.35) after correcting the Harris et al. 2008 row (127/202 = 0.629, previously mistranscribed as 0.184). The builder now reads `param_sigma_prop_symptomatic.csv`; the config sigma is the prior mean (0.345).
- chi priors refit at the published 2.5% quantile (means unchanged).
- alpha_2, phi_1, phi_2, p_beta and theta_j are refit by the corrected `fit_beta_from_ci()`, which keeps each requested centre (the mode) exactly and chooses the one free concentration that best matches the requested 95% interval on the logit scale (e.g. p_beta Beta(7.03, 13.24) -> Beta(5.48, 10.10)); theta_j ERI 0.708 -> 0.720. One concentration cannot in general hit both bounds, so the realized intervals are: alpha_2 exactly 0.25-0.75; phi_1 0.700-0.855 for the requested 0.715-0.864; phi_2 0.732-0.834 for 0.738-0.838; theta_j within 0.026 of each requested bound (largest ZAF); p_beta 0.144-0.596 for the requested 0.10-0.50. tau_i, the mobility Gammas, kappa, zeta_1/zeta_2, beta_j0_tot and the mu_jt block are unchanged in value.
- Initial conditions are estimated at `date_start` (2023-01-01). The v0.100.0 builder estimated them at the month with the most active-case countries within a year of the start (2023-02-01, the month v16.1 also used), so on its first v17.0 build the countries with an outbreak under way on 1 January but none reported just before 1 February (TZA, AGO, BEN, UGA) got the near-zero E/I template and started with E + I >= 1 person in only 7-10% of draws (the shipped v16.1 priors, from the earlier estimator, started all four in every draw). prop_E/I are back-calculated from a 28-day surveillance window straddling `date_start` (`est_initial_E_I(lookahead_days = )`, new). A quiet-start country gets a weak seeding prior, Beta(1, 1e5) for E and for I (`est_initial_E_I(quiet_start = "seed")`, new; mean 1e-5, the v16.1 scale, so about 25 (SWZ) to 680 (GHA) expected people in E + I), standing in for undetected circulation or importation that the model has no mechanism for. A country is a quiet start when it reports at least one case after the window, up to `date_stop`, and either (a) reports no cases in the window or (b) its window-based E/I priors imply fewer than one expected initial infection, N x (E[prop_E] + E[prop_I]) < 1. That seeds BFA CAF CIV GHA NAM NER RWA SWZ TCD TGO under (a) and COG (one case in the window) under (b), listed in `priors_default$metadata$quiet_start_seeded`. Window-based priors implying at least one expected infection (AGO, BEN, UGA, ...) are kept, and the 11 countries with no cases anywhere up to `date_stop` keep the near-zero template. In 300 `sample_parameters()` draws (seeds 1-300; the integer `E_j_initial + I_j_initial` passed to the engine; the 40-location `config_default`), every seeded country starts with E + I >= 1 in all 300 draws except SWZ (299), and every country with window-based priors in all 300 except AGO (299) and UGA (294, i.e. 98.0%, Wilson 95% CI 95.7-99.1%), the two lowest at about 6 and 4 expected initial infections; built per country with `get_location_config()`/`get_location_priors()`, the seeded countries start in 300 of 300 draws, AGO in 298 and UGA in 296. Single-location runs of the seeded countries (`get_location_config()`, `sample_parameters()` and `run_simulation()` with seeds 1-100) report at least one case in the first 60 days in 87-100% of draws (lowest SWZ 87%, CAF 90%), against 0-4% with the v17.0 build that preceded the seeding prior. prop_R priors are about 25x lower (the model's own reporting chain and a mean-keeping refit); V1/V2 are effective immunisations weighted by phi.
- Seasonal priors and the config give a positive transmission envelope at the prior means (29 of 40 countries were negative at the v16.1 means). Independent draws from the seasonal priors still dip below zero in ~33% of draws, which the engine clamps to zero human transmission; the builder reports this share. Shrinking the prior SDs enough to remove it would have pinned the amplitudes.
- zeta_ratio prior truncated at 1; config zeta_ratio 185, zeta_2 1.78e6.
- nu_jt from deduplicated OCV campaigns (-11% doses over 2023-27); theta_j WASH imputation fix (ERI); demographic inputs regenerated (UN WPP 2026-09-18).
- mu_jt is unchanged: the CFR GAM input is identical.
- `reported_cases`/`reported_deaths` follow the corrected surveillance: over the v5.1 window 304 case cells change NA status and 2,437 of 36,865 observed case cells change value; `config_default$epidemic_peaks` 54 -> 62 rows.
- Toy endemic/epidemic configs rebuilt; `estimated_parameters` unchanged (v1.3.0); `epidemic_peaks` 159 peaks detected on observed weeks only.

## Reproducibility
- `est_initial_E_I()`, `est_initial_R()` and `est_initial_S()` gain an optional `seed` argument, derived per location (a collision-free hash of the ISO code) and per draw, identical with or without forking, and restoring the caller's RNG. The priors builder passes `ic_seed` and uses 1000 E/I draws, so `priors_default` rebuilds are byte-reproducible.

## Tests
- `test-two-route-balance.R` evaluates the env/human route balance as the median over five seeds (a single run's p_star ranged 0.48-0.76) against the shipped p_beta prior; `test-ic_select_epoch.R` is removed with the selector.

## Fixes
- `render_MOSAIC_figures()` no longer leaves `Rplots.pdf` in the working directory. Several `plot_*` functions (`plot_spatial_hazard`, `plot_diffusion_pi`, `plot_departure_tau`, `plot_mobility_flux_matrix`, `plot_mobility_flux_network`, and `plot_model_likelihood` when verbose) draw to the current device, which opened R's default PDF device under Rscript; the renderer and its parallel workers now hold a null device while rendering. Written figures are unchanged.
- `vm/launch_mosaic_individual.R` treats a country as complete only when `3_results/summary.json` exists. A run that crashed after its shards were combined was previously skipped as finished; it is now flagged and rerun fresh.
- `run_MOSAIC()` docs: a run that dies after combining shards into `samples.parquet` cannot be resumed and must be restarted with `resume = FALSE`; `3_results/summary.json` marks a completed run.

# MOSAIC 0.100.0

Production-readiness deep review of everything since the pure-R engine refactor (v0.67.0): 18 component and cross-cutting reviewers, every finding adversarially verified (249 confirmed), fixed in 12 file-owner groups, red-teamed, and integrated. **Calibration results change**; the resume guard refuses to pool simulations from earlier versions.

**Deferred by decision:** the per-parameter marginal-ESS stopping rule (KDE, default) is known to pass a collapsed posterior; it is documented and pinned by a known-defect test, and will be redesigned separately.

**Data objects not rebuilt:** the shipped `priors_default` (v16.1) and `config_default` (v5.1) are unchanged. Their builders now produce materially different content and are versioned v17.0 / v6.0 (changelog heads list the changes); several entries below take effect only at that rebuild and are marked "at the next rebuild".


## Engine

- The engine refuses a config missing S/E/R/V1/V2_j_initial for a compartment in the pipeline, or any initial field feeding the gravity populations ("Config is missing `x`"), instead of treating it as zero and silently producing an all-zero epidemic.
- `nu_jt_sources` no longer accepts V1/V2 (they were zero donors that delivered no doses), matching make_simulation_config().
- rng mode draws the t = 0 symptomatic split of I_j_initial as Binom(I_j_initial, sigma) at the new draw site infectious/sigma_split_t0 instead of round(sigma * I), so sparsely seeded patches no longer always start with zero symptomatic. Every rng stream shifts by one draw, so seeded results are not bit-identical to earlier versions; replay mode is unchanged. sim_seed_state() (internal) now requires a draw controller.
- The human-transmission seasonal envelope is evaluated at calendar day-of-year (`season_t0 = yday(date_start) - 1`). Output changes for any config whose date_start is not 1 January; posteriors from earlier non-January runs have their seasonal coefficients out of phase.
- DerivedValues no longer copies the state row list every tick when writing spatial_hazard (~15.6 MB of garbage per 1,398-tick run).
- run_simulation() docs state that only replay mode reproduces laser-cholera 0.16.1, list the rng-mode spec corrections, and note that the engine uses the raw psi_jt (it does not read psi_star_*).

## Configuration and parameter sampling

- create_sampling_args() returns a complete `sample_args` flag list, so patterns sample only what they name (before, every pattern sampled everything) and never un-pin alpha_1, alpha_2, kappa or rho_deaths (use `custom`). spatial_only covers mobility_omega, mobility_gamma and tau_i; new environmental_only covers zeta_1, zeta_ratio and decay_*.
- sample_parameters() defaults `sample_kappa = FALSE`, matching mosaic_control_defaults() (kappa pinned at 1e6), so post-hoc calc_model_ensemble() calls without `sampling_args` no longer redraw kappa. Both read one internal flag list.
- sample_parameters() applies the config's psi_star_a/b/z/k to psi_jt whenever any differs from the identity, even with every sample_psi_star_* flag FALSE; config_default's pinned psi_star_b = 1 was silently dropped.
- A config returned by sample_parameters() is marked as psi_star-calibrated (attribute plus a `psi_star_applied = TRUE` field that survives JSON, e.g. config_medoid.json). Passed back as the template, its psi_jt is kept when psi_star is not redrawn and sampling errors when it is, so calc_psi_star() is never applied twice.
- sample_parameters() warns on unknown sample_* names in `...`; an NA global draw keeps the config value (or stops) instead of writing NA; a location-sampling failure stops instead of leaking `failed_locations` into the global environment.
- convert_config_to_dataframe() and get_param_names() carry the same fields as convert_config_to_matrix() (a_*_j/b_*_j, chi_endemic/chi_epidemic, prop_*_initial).
- make_simulation_config() rejects a vector alpha_2 and a wrong-length epidemic_threshold, returns the validated list when it writes a file, and supports .hdf5 output.
- JSON writers (write_list_to_json(), write_json_or_gz(), write_model_json(), config_medoid.json) write 17 significant digits and atomically, so configs and priors round-trip exactly.
- get_location_priors() works when MOSAIC is loaded but not attached; inflate_priors() logs empirical fits correctly. Docs for config_default, config_simulation_endemic and priors_default corrected (tau_i is lognormal; the prop_S_initial prior is informational only).

## run_MOSAIC() orchestration and artifacts

- A `control$likelihood$score_start_cases` later than the deaths start is honoured: the worker masks that cases prefix, so the likelihood, the cases dispersion and the ensemble R2/bias score the same window; peak-timing peaks in the excluded stretch are skipped for cases.
- The medoid (config_medoid.json, medoid_ensemble.rds, medoid predictions) is chosen over every location and only the scored time steps; before, the first location alone and the burn-in head decided it. Single-location medoids can also change. add_reproductive_numbers(recompute_ci = TRUE) uses the same criterion, so its medoid R_eff member belongs to config_medoid.json's param set.
- A non-resume re-run into an existing `dir_output` no longer reuses the previous run's files. Before, `ensemble_optimized.rds` kept run 1's posterior and figures, run_rolling_cv() and MOSAIC-OCV read it. The posterior/medoid/trajectory/spatial RDS files, config_medoid.json, the per-location prediction and trajectory CSVs, model_fit_windows.csv, optimization_diagnostics.csv, parameter_sensitivity.csv, cfr_posterior.csv and reproductive_numbers.{csv,rds} are deleted before they are rebuilt, and leftover `sim_*.parquet` shards are moved to `2_calibration/samples_stale_<time>/` instead of being pooled. Figures are only replaced when re-rendered.
- `1_inputs/mobility_tau_ci.csv` is written again (an always-false `isTRUE(control$io)` guard had blocked it since v0.53.0), and a stale copy is removed when there is no interval to write.
- Fixed-mode runs (`n_simulations` set) end with status `completed_fixed` (`completed_fixed_partial` without ensemble metrics) instead of `completed_unconverged`. `converged` stays logical (FALSE); summary.json and the returned summary gain `convergence_evaluated` (FALSE in fixed mode). summary.json gains `posthoc_criteria_met` (not in the returned summary), and `[RUN_SUMMARY]` gains `mode=`, `convergence_evaluated=` and `posthoc_criteria_met=`.
- environment.json: `git$sha`/`branch`/`path` still describe the working-directory (country) repo that MOSAIC-OCV reads; new `git$mosaic_sha`/`mosaic_branch`/`mosaic_source` record the MOSAIC code that ran.
- Resume refuses shards simulated or scored under different semantics: environment.json carries an `engine_semantics` stamp (season_t0 phase, t = 0 symptomatic split, pinned psi_star), and the likelihood tag is `R/v0.100.0+review_likelihood`. Run directories from earlier versions cannot be resumed.
- run_MOSAIC() stops with an informative error when no location has a finite observation in the scored window, instead of calibrating on all-NA likelihoods; locations that contribute nothing are logged.
- Calibration parallel robustness: an R-level error in one task counts as one failed simulation (with a warning giving the count and first message) instead of crashing the tally; the gather idle timeout scales with the work per task (option `MOSAIC.calibration_sec_per_engine_run`, default 30 s); worker setup no longer ships the whole run_MOSAIC frame (~10 MB/worker at 40 locations).
- model_fit_windows.csv is correct for multi-location runs (windows over time, with dates; masked cells do not count toward n_obs).
- `io$format = "csv"` is retired (output was always parquet); it is coerced to parquet with a warning.
- control.json stores a per-channel `central_method` as a named `{cases, deaths}` object (older unnamed arrays still read), and mosaic_control_defaults()$sampling comes from sample_parameters()' flag list with corrected labels (kappa is the half-saturation dose, rho the care-seeking probability).
- Docs: removed the nonexistent `targets$percentile_min`, fixed the run_MOSAIC() custom-priors example, set_root_directory() returns the root, get_paths() documents every path, and the Python helpers no longer claim MOSAIC attaches Python on load.

## Likelihood

- The cumulative shape term (weight_cumulative_total > 0) scores Poisson locations as Poisson, sums observations and predictions over the same scored cells (excluding zero-confidence-weight cells), and uses the core eps floor instead of the retired log(1e6) penalty.
- With the default weights, a location whose observations all fall where weights_time is 0 is skipped instead of failing every simulation; a location with no data in either channel is NA, so fully missing input returns NA_real_ as documented.
- calc_log_mean_exp() keeps -Inf replicates as zero likelihoods and drops only NA/NaN, so the n_iterations > 1 collapse no longer rewards parameter sets for failed replicates.
- The integrated deaths likelihood's location-offset width averages the prior SEs over the years that location observes.
- calc_model_likelihood() treats an `epidemic_peaks` read back from JSON as `list()` as no peaks. Docs describe the actual shape-term scaling (WIS by N_obs/length(wis_quantiles), cumulative by N_obs/length(cumulative_timepoints)); calc_log_likelihood() documents its scalar return.

## Weighting, convergence and posterior

- The best-subset tier search (grid_search_best_subset()), optimize_ensemble_subset(), `weight_best`, the posterior quantiles, posteriors.json and the ensemble weights all use `control$targets$best_subset_weighting`, the scheme the final ESS_B/A/CVw gate uses. The search used a scale-free exp(-2*delta/range) weighting, so a tier could converge on weights the gate then reported as WARN. The saturated default, exp(-0.5*min(delta, 4)), is unchanged; under "tempered" (ESS about 0.058 n) runs choose much larger subsets or fall back to the top max_best_subset draws, which is now logged as a warning. Both functions gain `weighting`; optimize_ensemble_subset() gains `ess_method` and names `persist_ensemble_arrays = TRUE` when given stripped arrays.
- Posterior KL in posterior_quantiles.csv, and calc_kl_divergence(), are the information gain KL(posterior || prior) with a weighted-KDE bandwidth (weighted SD/IQR, Kish n_eff) integrated over the posterior's own support. Before, a capped KL(prior || posterior) reported 20 for every well-identified parameter and concentrated weights were smoothed back towards the prior. KL is NA when the weights' Kish n_eff < 2, and calc_model_posterior_quantiles() now warns when that makes every posterior KL NA.
- calc_convergence_diagnostics(): the subset-percentile check uses the n_total denominator (it could never fail) and is reported as summary$percentile_status, not gated; ESS_B thresholds documented as implemented (pass 100%, warn 80%). calc_model_convergence_status() / convergence_status.csv show the subset percentile, the B_size_upper cap, the exact IS diagnostics (ESS_IS, Pareto k-hat with a reliability verdict) and the overall verdict.
- calc_bookend_batch_size() returns phase "no_progress" instead of extrapolating a flat or declining ESS, and "low_confidence" when the fit puts the target below the current n while ESS is still short.
- calc_is_diagnostics() reports a tied tail as "tail ratios tied: k-hat undefined". calc_model_posterior_distributions() no longer counts unknown-scale rows as fit failures; calc_model_parameter_sensitivity() fills location-scale descriptions.
- Documented limitation: the kde (default) and binned marginal ESS do not detect importance-weight collapse (a point mass still gives roughly n/5 to n/25); read them with calc_is_diagnostics().

## Ensemble, R_eff and fit diagnostics

- run_fit_sandbox() scores the way run_MOSAIC() does: each day's total sums only location-days with an observation, and the default 30-day burn-in plus the 2-step cases warm-up are dropped (always the package default; there is no `control` argument). Scorecards are not comparable with earlier versions. On a pre-v0.96 config a CFR_target override takes effect and a mu_jt override is refused; predictions gain predicted_central, predicted_mean, central_method and n_locations_observed.
- calc_model_R2() and calc_model_cor() share one weights contract (scalar, full-length subset with the validity mask, or pre-filtered), and calc_bias_ratio(na_rm = FALSE) returns NA when an NA is present.
- calc_fit_diagnostics() computes cv_ratio and residual autocorrelation on paired, gap-aware days; an all-zero or flat prediction grades FAIL instead of NA.
- add_reproductive_numbers(recompute_ci = TRUE) weights members by the run's final (optimized) posterior mapped by seed, skips members that failed at calibration, uses the run's resolved central method (warning and falling back to the mean when unresolvable), no longer requires trajectories_ensemble.rds, and reports the true ensemble_source.
- calc_model_ensemble(): the worker no longer forces a per-simulation gc(), trajectory thinning no longer needs withr, the reconstructed epidemic_frac flag matches the engine, and PSOCK task errors are warned with a count.
- calc_spatial_hazard(): docs, example and dimnames match the J x T orientation, and names no longer error when J != T.

## CFR and rolling CV

- process_CFR_data() drops in-progress snapshot years, names its output for the last complete year and removes superseded later-dated artifacts, aggregates by ISO code (CIV no longer split), keeps only country-years with both counts, and fits each Beta with a logit-scale quantile fit. propvacc::get_beta_params() had returned a degenerate Beta(~0.009, ~1.9) for every ~2% CFR, so the shipped case_fatality_ratio table needs regenerating.
- est_CFR_hierarchical() labels the point row of param_mu_disease_mortality.csv `median`, and plot_CFR_hierarchical() marks population-average locations from the `pooled` flag. update_mosaic_data() fits the GAM with the package defaults (min_cases = 1, k_year = 12), matching the shipped artifacts; the old 3/15 settings moved some mu_jt centres by 10-14%.
- run_rolling_cv() drops peaks whose scoring window passes the cutoff, wipes each cutoff's run directory, fits psi in a scratch MODEL_INPUT (the canonical psi CSV is no longer overwritten), checks that the psi cache covers the scored window, rewrites manifest.json after every cutoff, rejects unknown model names, and emits `ensemble_opt` only when the subset optimizer actually selected a subset. It warns that psi from the canonical panel is not leak-free and that priors other than mu_jt are not rebuilt per cutoff.
- prefit_rolling_cv_psi() fits in an isolated scratch directory, keeps earlier cutoffs in its manifest, includes the prediction window in the cache-hit test, and no longer overwrites MOSAIC-docs figures. The cache key now includes a psi-algorithm version, so **existing psi caches (e.g. the OCV-4 cutoffs) are refitted, not reused**, after this release's drought_prob and lstm_v2 changes. Each fit's psi_suitability_config.json is kept next to its CSV (`psi_<T>_config.json`, also for run_rolling_cv() without a cache), and manifest entries record `n_seeds_ok`, `seeds_failed`, `target_anchor_end` and `source_csv_md5`, so a cutoff pooled over fewer seeds than requested is visible.
- evaluate_rolling_cv() measures horizons from the end of the harness embargo and never scores embargo rows; partial named `embargo_weeks` work and cells carry `anchor_date`. Scored sets shift by one day, so published OCV-4 scores will not reproduce exactly.
- Exported defaults changed: make_forecast_cv_table(train_start = NULL) takes the training length from each run's anchor date (summary rows respect the ESS gate, new `n_gated`), and plot_forecast_cv_skill() defaults to horizon_months = 3, model = "ensemble_opt". plot_rolling_cv() shades the window evaluate_rolling_cv() actually scores.

## Environmental suitability

- est_suitability() (lstm_v2): an auto-detected fit_date_stop is the last week with both observed cases and complete ENSO/IOD data, as documented, not the end of ENSO coverage (on the canonical panel 2027-04-29 -> 2026-08-13). The fold grid, the best epoch, the final refit and therefore the **default production psi change**; about 8 months per country move from 'training' to 'prediction'.
- est_suitability() (lstm_v2): the final full in-sample refit includes the week at fit_date_stop; per-country smoothing and bias correction stop at each country's last covariate-supported prediction; a model trained with `lead > 0` predicts `lead` weeks past the last covariate week.
- Under response_var = "transmission_intensity", every week without a case observation has an NA target instead of being trained as zero incidence; compile_suitability_data() writes that column as NA on such weeks (17,576 rows on the canonical panel). Observed weeks are unchanged.
- est_suitability() warns when a pre-computed target_* response is fit at a cutoff before the panel's target-anchor end (target-side leakage, detected not removed); compile_suitability_data() records it in a new `target_anchor_stop` column. The lstm_v1_legacy path warns similarly for a retrospective cutoff.
- psi_suitability_config.json records n_seeds_ok, seeds_ok, seeds_failed, seed_aggregation, target_anchor_end, the resolved arch_hp, loess settings and country_pool; failed seeds raise a warning, and under devtools::load_all() seeds fit serially (PSOCK workers would run the installed package).
- compile_suitability_data() reads the World Bank GDP and population-density files written by the process_WB_* functions. The lagged, observed-climate drought_prob GAM (see Data pipeline) is signed off for psi; only feature_set v7.4 uses the drought channels.
- calc_psi_star() no longer errors on a length-1 series or a single observed value under linear fill. est_suitability() documents logit-median seed pooling, feature_set = "v7.4" and a Leakage section.

## Priors and parameter estimation

- fit_beta_from_ci() keeps the mode exact and chooses the concentration by logit-scale quantile matching (an absolute +/-0.01 clamp discarded CIs below ~0.02); fit_lognormal_from_ci() matches the CI exactly on the log scale; fit_gompertz_from_ci() matches the interval and reports the true mode. Posterior JSON and staged priors from calc_model_posterior_distributions(), inflate_priors() and update_priors_from_posteriors() change accordingly.
- The zeta_ratio prior is a lognormal truncated below at 1 (`parameters$lower`), so zeta_2 = zeta_1 / zeta_ratio can no longer exceed zeta_1 (about 16% of draws did); the median moves from ~75 to ~185 at the next rebuild. sample_from_prior() honours lognormal lower/upper bounds; calc_model_posterior_distributions() fits a bounded prior's posterior in the same truncated family and writes the bounds to posteriors.json; update_priors_from_posteriors() refits an unbounded posterior to the truncated family before attaching the bound, so staged updates no longer drift (a non-identifiable zeta_ratio's median had moved ~5x per stage). check_sampled_parameter() and the prior/posterior density plots use the truncated mean, quantiles and density. make_config_default.R takes the pinned zeta_ratio (and zeta_2) from the truncated prior median.
- est_seasonal_dynamics() gains envelope_floor (default 0.1), shrinking case-fit coefficients so 1 + f(t) stays positive (new envelope_scale column), and aggregates precipitation on ISO weeks. param_seasonal_dynamics.csv is regenerated (30 of 40 locations scaled); both builders stop on a non-positive envelope. disagg_annual_cases_to_daily() weights days by 1 + f(t) with period 365, as the engine does.
- est_initial_E_I() back-calculates through the engine's reporting chain (the rho, chi_endemic and delta_reporting_cases priors instead of branch-specific placeholders), no longer puts reported cases into E, keeps zero draws, and fits the Beta to the Monte Carlo mean. A window with zero reported cases or no usable surveillance gets the near-zero Beta(0.01, 99999.99); only a failed estimate with data uses Beta(1, 9999) / Beta(0.5, 9999.5).
- est_initial_R() draws rho and chi_endemic from the global priors (it always used chi/rho = 5) and the per-location seasonal priors; est_initial_R() and est_initial_S() refit by method of moments, keeping the sample mean with the SD scaled by variance_inflation. fit_beta_with_variance_inflation_R() floors shape1 at 1 so a large factor cannot collapse prop_R onto 0. prop_R_initial means fall ~10-50x at the next rebuild.
- est_initial_V1_V2() gains phi_1/phi_2 and converts pre-t0 doses to effective immunisations. It reads the ees-cholera-mapping GTFCC request log, not data_vaccinations_GTFCC_WHO.csv. est_initial_S() no longer errors with verbose = TRUE.
- make_priors_default.R (at the next rebuild): no hand-tuned per-country E/I factors; a uniform E/I variance inflation of 10; variance_inflation_S = 1 (the 0.01-0.10 table shrank the SD); an assert that no prop_R prior's median is below 0.1x its mean; mobility inputs ported so a rebuild reproduces the shipped tau_i (overland lognormal) and blend gravity parameters; changelog-date guards.
- The estimated_parameters inventory is rebuilt (v1.3.0): alpha_1 is location-scale, tau_i lognormal, kappa's units corrected. Posterior quantiles previously skipped the alpha_1_<ISO> columns.
- est_zeta_ratio_prior()$fit, param_zeta_ratio_prior.csv and zeta_ratio_prior.png carry the shipped direct channel truncated at 1 (the figure had shaded the diagnostic combined channel's interval and median); est_immune_decay_vaccine() plots the est_vaccine_effectiveness() fit; est_WASH_coverage() pairs weights with the right countries; est_mobility() handles partial-coverage OD sources.
- get_symptomatic_prop_data(): the Harris et al (2008) row is the published 127/202 (0.629, 95% CI 0.558-0.695) and intervals are validated; get_suspected_cases() fits chi to the true 2.5% quantile. print.mosaic_priors and print.mosaic_initial_conditions_S are registered S3 methods.

## Data pipeline

- combine_vaccination_data() matches WHO shipments to GTFCC campaigns by ICG request number (GTFCC records one total per request, WHO one row per shipment) and drops repeated WHO-only listings beyond a request's approved total. match_confidence labels are correct (38 were NA) and an all-matched WHO table no longer errors. The shipped combined file has 173 campaigns and 182.83M doses (was 186 and 200.35M; MOZ 2017 and CMR 2019 second shipments and a duplicated MWI 2018 row were double-counted), with its redistributed file and param_nu_vaccination_rate_GTFCC_WHO.csv regenerated. config_default nu_jt picks this up at the next rebuild.
- process_WHO_weekly_data() dates weeks on WHO's epi-week calendar: 2025-W53 is no longer summed into W52 (doubling that week for ~20 countries), 2026 weeks are no longer 7 days early, and rows with a missing count are kept. process_JHU_weekly_data() keeps missing counts as NA instead of fabricating zeros (~1,870 weeks).
- process_JHU_weekly_data() drops the 4,144 country-weeks the OSF archive flags `phantom` (zero-filled weeks with no report: cases 0, no deaths, no observation_collection_id). They had entered the combined series as reported zeros at full trust and outranked real AI counts (COD 2012-01-09: 0 instead of 1,600). In the combined weekly file, 4,113 former JHU weeks are now filled by AI (2,257 fourier with 214,355 cases at confidence ~0.5, 132 observed with 16,346, 100 documented_zero) or SUPP (46), and 1,578 are empty. In the 2023+ fit window, 88 of these weeks change: 26 become empty, 48 take AI observed counts and 14 take fourier.
- process_cholera_surveillance_data() picks each country-week's row so that an observed row (WHO/JHU/SUPP, AI observed/documented_zero) always beats an imputed one (AI fourier_*, assumed_zero), whatever fields each carries, then by source priority WHO > JHU > AI > SUPP. The old rule put rows with both cases and deaths ahead of priority, so once JHU deaths became NA every JHU week without deaths lost to any AI row with both fields. On the phantom-free inputs that is 48 JHU weeks (SLE 19, SOM 23 fourier weeks in 2017 whose 92,603 cases replaced JHU's 45,542, and 6 others) plus 81 SUPP weeks that lost to fourier rows (UGA 2020-2021); none is in 2023+. If the chosen row has no death count, deaths come from another observed row with the same case count after half-up rounding (the same report), and the week keeps the lower confidence_weight of the two rows; rows from different reports are never mixed. A new source_deaths column in the weekly and daily combined files names the source of each death count.
- Consumers that drop AI rows see these source changes as added or removed weeks: est_seasonal_dynamics() and the epidemic_threshold derivation in make_priors_default.R, and compile_suitability_data()'s target anchors (trusted rows only). The phantom zeros had counted as observed zero weeks in all three.
- est_epidemic_peaks() detects peaks on observed weeks only: fourier_*/assumed_zero weeks are treated as missing, a flagged day on a flat stretch of the smoothed curve moves to the stretch's centre, and the peak day must be observed with cases > 0. The old rule took the last day of the stretch, which put an isolated observed week's peak ~11 days late on a zero day (ZAF 2023-09-11, SEN 2005-07-11, SOM 1997-12-08 reported 0 peak cases). A detected peak whose window is at least half imputed days is dropped. The hand-curated peaks the function appends are documented outbreaks and are exempt from that filter. plot_epidemic_peaks() draws the same series. Run on the corrected combined series, model/input/param_epidemic_peaks.csv and data/epidemic_peaks.rda (kept identical) have 159 peaks in 32 countries (was 139 in 28, built at v0.32.10 from data through 2026-04). By era: 11 before 2010 (JHU and AI observed back-history), 80 in 2010-2022, 68 from 2023 (59 before). In 2023+ the 2026 peaks move one week with the WHO re-dating and later 2026 outbreaks are added. The csv also feeds get_cases_binary_from_peaks(), which sets cases_binary in the suitability panel when compile_suitability_data() runs with use_epidemic_peaks = TRUE (update_mosaic_data(), prefit_rolling_cv_psi()), so the next psi fit trains on the new labels. config_default ships its own filtered copy, which changes at the next rebuild; until then only calc_model_likelihood()'s fallback for configs without epidemic_peaks reads the new object.
- impute_drought_probability() lags local-climate predictors past the 12-week SPEI label window (the old fit was a circular nowcast) and gains climate_obs_stop, which compile_suitability_data() sets per country from the observed climate/ENSO horizon. drought_prob changes a lot (deviance explained 69% -> 28.5%), so the next est_suitability rebuild moves psi. All hazard imputers return predictions in input row order.
- process_IDMC_data() counts only IDU 'Recommended figure' rows (Triangulation rows inflated displacement ~25%). process_WHO_annual_data() validates, deduplicates and logs dashboard snapshots, keeps the newest at equal coverage, and no longer splits Cote d'Ivoire.
- Raw writers (download_WB_data, download_UN_WPP_data, EM-DAT, download_IDMC_data, download_mobility_od_sources, the friction cache, get_WHO_vaccination_data) write dated snapshots atomically and log provenance; resolvers skip partial snapshots; an HDX outage is a failed IDMC download; download_country_DEM() gains overwrite = FALSE.
- get_cases_binary() handles countries with fewer than four weeks; refresh_data_repos() labels the JHU input as a static OSF archive.

## Plotting

- Prediction-figure captions score the same masked series as summary.json (warm-up and burn-in excluded).
- render_MOSAIC_figures() reads the run's resolved per-channel central_method (summary.json, subset_opt.rds, then control.json, including unnamed arrays) instead of aborting or swapping channels; renders sensitivity from the existing parameter_sensitivity.csv (which now records `weighting` and `n_used` for the subtitle), recomputing with a fixed seed only when absent; and reads only the needed samples.parquet columns.
- Spatial figures use posterior medians of tau_i, mobility_omega and mobility_gamma (fixed parameters stay fixed), and departure_tau shows the tau_i posterior interval. get_ggplot_legend() works with ggplot2 >= 3.5 and returns an empty grob for a legend-less plot, so mobility_flux_network.png is produced for single-location runs.
- plot_psi_star_diagnostic() plots the psi_jt the run was calibrated on, works post-hoc without a MOSAIC root (PATHS optional), and no longer repeats one location's psi under every name.
- plot_model_ppc() no longer double-counts multi-location runs, and its coverage statistic is labelled 'P(central > obs)'. plot_suitability_and_cases() and plot_suitability_by_country() plot the psi_jt the model receives.
- plot_model_likelihood() counts -Inf draws as failed; plot_model_subset_optimization() places the optimal-N marker correctly; plot_epidemic_peaks() shares est_epidemic_peaks()' 28-day smoother; TruncNorm labels stay readable for small-scale parameters.
- plot_seasonal_clustering() defaults to clustering_method = "ward.D2" (the old default errored) and its "knn" method works.

## Packaging, documentation and tests

- check_dependencies() reports missing TensorFlow/Keras as 'Limited', no longer creates a global `suitability_working`, and stops cleanly after an attach failure. install_dependencies() docs describe the suitability-only environment and the separate keras3 install.
- The Running-MOSAIC vignette and vm launchers no longer call attach_mosaic_env(); the Running-simulations vignette shows regenerated figures; the startup banner, README and Installation vignette use the DESCRIPTION name and no longer mention LASER.
- NEWS entries reconstructed for 0.74.0-0.91.x, including the 0.89.0 engine corrections and the change that pins kappa by default.
- CI runs `R CMD check` (no tests or vignettes; fails on WARNING), and the nightly tier runs the run_MOSAIC() integration test. The samples.parquet schema and sample_parameters tests run without `~/MOSAIC`; vacuous and mirror tests now call production functions; flaky moment checks are seeded.
- The pkgdown site no longer publishes internal planning/agent notes (every root *.md except README, NEWS and LICENSE is excluded), and inst/bin/setup_mosaic.sh is no longer installed.
- A duplicate internal .mosaic_best_subset_weights() from the review-branch merge is removed (no behaviour change). calc_model_ess()'s example is corrected (~1.74). inst/examples/simulate_outbreak_settings.R warns when a setting produces zero cases; the sporadic and rare settings need re-tuning for the v0.89.0 engine.

# MOSAIC 0.99.10

- The pkgdown site carries the Gates Foundation standard footer (legal notice, privacy and terms links) (#121).

# MOSAIC 0.99.9

## Carried-forward CFR years are exactly flat on every BLAS (v0.99.9)

Under `forecast_method = "carry_forward"`, `est_CFR_hierarchical()` gives every year after the last data year the same design row, but OpenBLAS can sum identical rows in different orders, so their `logit_mean` differed in the last bit on Linux and the as-of `mu_jt` was not exactly flat (a CI-only test failure). Those years now copy the first occurrence. On macOS the values were already identical, so no data object changes.

# MOSAIC 0.99.8

Merges #127's 0.93.2 fix. `calc_Reff.R` keeps the v0.96.0 mortality caveat, which never had the escaped percents.

# MOSAIC 0.99.7

## Pre-merge review fixes (v0.99.7)

- `calc_convergence_diagnostics()` documented `is_diagnostics` twice after the v0.99.2 merge, which moved `verbose`'s default under the wrong parameter and dropped the link to `calc_is_diagnostics()`.
- NEWS now records the removal of `calc_cases_from_infections()` and `calc_deaths_from_infections()`.
- `write_trajectory_csv()` no longer says the summary is the weighted median throughout: `disease_deaths` follows the deaths `central_method`.

# MOSAIC 0.99.6

## R CMD check hygiene (v0.99.6)

- `test-psi-manifest-provenance.R` read `R/` source before checking it exists, so it errored under R CMD check (where `R/` is absent) instead of skipping. CI never saw it because CI runs `testthat::test_local()` from source.
- `cfr_pred`, `deviation` (`plot_CFR_hierarchical()`) and `.dp` (`impute_drought_probability()`) are declared in `globals.R`.

# MOSAIC 0.99.5

## Merge the R_eff post-merge fixes (v0.99.5)

Brings in #127 (0.93.1): run inputs written at 17 significant digits, the `peak_window` argument to `calc_Reff()`, and the review's caveat and test fixes. `calc_Reff()` keeps the "mean" cases default and the CFR v2.1 mortality caveat, because deaths now leave at onset rather than at a rate from Isym.

# MOSAIC 0.99.4

## Declare the data pipeline's optional packages (v0.99.4)

`get_travel_time_matrix()` uses gdistance and malariaAtlas, and `rake_mobility_od_to_tau()` uses mipfp, each behind `requireNamespace()`. They were never declared, so R CMD check raised a WARNING on every run; they are now in Suggests. `mosaic_run_suffix()` calls `utils::str()` explicitly, clearing the matching NOTE.

# MOSAIC 0.99.3

## The human-R recovery test pools seeds (v0.99.3)

`test-reproductive_numbers.R` compared one realization's median R_hum ratio with
a 5% tolerance. That ratio scatters by about +/-5% seed to seed (0.91-1.08 at
the epidemic fixture), and the merged engine's fatal-onset draws change the
realization, so seed 3 fell outside it (0.909). The check now pools 8 seeds
(median ratio 0.98). The route kernel ignores the fatal onsets that never enter
Isym; at this config (p_fatal 3.3%) that moves the ratio by about -0.5%.

# MOSAIC 0.99.2

## The CFR v2.1 line merges into main (v0.99.2)

This release merges the CFR v2.1 development line into main. That line was
numbered 0.92.0-0.99.1 in parallel with main's 0.92.1-0.93.0 (route-split R_eff,
the psi_evolve close-out, the automated data refresh), so both sets of changes
are listed: the CFR line's under this heading (its own 0.92.0 and 0.93.0 entries
are labelled), main's under their versions below. What users will notice:

- Deaths come from the fate-at-onset reported-CFR model: `mu_jt` is the reported
  CFR, integrated out of the deaths likelihood per simulated path, and a config
  carrying the retired mortality fields (`mu_j_baseline`, `mu_j_epidemic_factor`,
  `CFR_target`, `delta_reporting_deaths`, `mu_j_slope`) is converted (a
  `CFR_target` becomes a constant `mu_jt`, with a warning) or refused. Rebuild
  such configs with `make_config_default()`.
- The ensemble central line is the weighted mean (`central_method = "median"`
  restores the previous behaviour), and forecast years carry the ensemble's
  latest-year CFR shift.
- The integrated deaths likelihood adds a median 13% (11-25%) to each scored
  iteration on the 40-location config.

## Pre-merge audit fixes: figures follow the run's central line; stale deaths text (v0.99.1)

- **render_MOSAIC_figures() drew the median for every mean run.** It read
  `central_method` from the top of `1_inputs/control.json`, but run_MOSAIC()
  nests the control under `$control`, so the lookup always fell back to the
  median; since the v0.98.0 default flip every re-rendered prediction figure
  (and the in-run `plots = TRUE` figures) showed the median while the CSVs and
  summary metrics used the mean. The lookup is now `.mosaic_run_central_method()`,
  which reads the nested shape and still treats a control.json without the
  setting (pre-v0.38.0) as median; the test fixture writes the real shape.
- **The final deaths step is no longer masked.** `mask_final_deaths_step`
  defaulted to TRUE for a laser-cholera off-by-one (issue #82) that the R engine
  does not have: since v0.96.0 deaths are reported on the cases' row and the
  post-hoc redraw fills the last column, so the mask blanked a real day in the
  CSVs, plots and R^2/bias. The default is now FALSE in calc_model_ensemble(),
  plot_model_ensemble() and the prediction table; an ensemble saved without an
  `artifact_mask` (laser-cholera era) still masks. A regression test checks the
  engine's final deaths column is populated.
- **Plot labels.** The PPC names the plotted central series from the CSV's
  `central_method` ("Predicted Mean"/"Median"); plot_forecast_cv_grid() draws
  `pred_central` (falling back to `pred_median`); the ensemble caption names the
  nested intervals correctly ("95% and 50%"); the trajectory true-deaths panel
  follows the deaths channel's central method and is labelled as fatal onsets
  dated at onset; plot_model_posteriors_detail() draws truncated-normal priors
  (delta_reporting_cases, epidemic_threshold, decay_*, psi_star_*).
- **plot_CFR_hierarchical()** takes its years from the model outputs (it had
  1970-2024 and 2024 hard-coded), shades and dashes the years each trend is
  held at its last fitted year, drops in-progress years as the model does,
  replaces the random-effects page (the country intercept is weakly identified,
  about 1e-5) with each fitted country curve's deviation from the population
  trend, keys its summary on iso_code (Cote d'Ivoire was split in two), and says
  "Case Fatality Ratio".
- **Docs:** the fatal share of symptomatic onsets is a few percent, not "a
  fraction of a percent"; the NB deaths dispersion is diagnostic only and the
  retired-setting warning no longer suggests `nb_k_deaths`; the Deployment
  vignette's install/run chunks are `eval = FALSE`; skill and agent notes updated
  (central_method default, the integrated deaths core, retired `nb_k_min_*`).
- Removed the orphaned `model/input/parameters_inventory.csv` (no reader; its
  `mu_j` row was the retired mortality model) and `local/calibration/
  calibration_test_43.R` (it set removed `sample_mu_j_*` flags).

## Forecast years carry the ensemble's shared CFR shift, not each member's own (v0.99.0)

v0.98.0 centred each ensemble member's forecast-year CFR on that member's own
deviation for the latest observed year. That deviation also absorbs the member's
case error that year: a member that under-shoots the year's cases gets a high
CFR, and members like that tend to have larger waves later, so the carried
error amplified the forecast. In the calibration test SSD's 2026 deaths went
from 1.05x to 2.50x observed while NGA's improved.

Each forecast year is now centred on one shift per location: the weighted mean,
over the posterior ensemble's members, of their posterior-mode deviations for
the latest observed year. The members' shared CFR change carries forward; each
member's own case error does not, and each member keeps its own location offset.
`run_MOSAIC()` estimates the shift after calibration from one run per ensemble
member, logs it, and stores it in the deaths integration, so the ensemble, the
medoid, `cfr_posterior.csv`, `config_medoid.json` and a post-hoc re-run from
`deaths_integration.rds` all use it. The forecast-year prior is a normal centred
on the shift (the v0.98.0 prior coupled forecast years to each member's latest
year). The calibration likelihood is unchanged except where a scored day's
blend reaches a forecast year, where it now uses a zero shift.
`calc_model_ensemble()` returns the shift as `forecast_shift`, and
`calc_log_likelihood_deaths_integrated()` gains a `forecast_shift` argument
(default 0: forecast years revert to the prior level). The likelihood version is
`R/v0.99.0+deaths_forecastshift`. Six test assertions that compared small
quantities (CFRs near 0.01-0.08) with `expect_equal(tolerance =)` were vacuous,
because testthat switches to an absolute difference when the values are smaller
than the tolerance; they now use a relative-error check (`expect_rel_equal()`).

## The ensemble central line is the mean; forecast years continue the latest CFR (v0.98.0)

- **Ensemble central tendency defaults to the mean** for both cases and deaths
  (`control$predictions$central_method = "mean"`; it was `"median"` from
  v0.46.1). It drives the prediction line and `predicted_central`, the headline
  `*_ensemble` R^2/bias, the medoid target and the subset objective. The daily
  median of sparse deaths is zero on most days in low-count countries, so it
  read 0x in-sample deaths bias for MOZ and KEN in the CFR-v2.1 calibration test;
  across the 8 countries the median in-sample deaths bias moves from 0.65x to
  0.78x and out-of-sample from 0.51x to 1.30x. Cases move from slightly low to
  slightly high (0.94x to 1.08x in sample). WIS, coverage, the calibration and
  the posterior are unchanged. **Behaviour change:** the medoid (and so
  `config_medoid.json`, the medoid plots and the R_eff central line) is now the
  member closest to the mean cases series. `summary.json` keeps both
  `*_ensemble_mean` and `*_ensemble_median`, and
  `central_method = "median"` reproduces the previous behaviour. Completed runs
  whose `control.json` predates the setting are still read as median by
  `render_MOSAIC_figures()` and `add_reproductive_numbers()`.
- **Forecast years continue the latest calibrated CFR.** The integrated CFR's
  year deviations were independent, so a year past the data reverted to the
  prior's long-run country level; in the calibration test this under-forecast
  COD's 2026 deaths after its CFR rose from about 1% to 3%. Each forecast year's
  deviation is now centred on the latest observed year's, with the same
  year-to-year spread (`sd_year`). The latest observed year is the year of the
  last scored day minus 30 days, so data reaching only into a new year's
  1 January blend leave the year before as the anchor. The prior is a product of
  conditional densities with unit Jacobian, so the calibration likelihood is
  unchanged except where a scored day's blend reaches a forecast year (data
  ending within 30 days of a 1 January). The post-hoc death redraw,
  `cfr_posterior.csv` and `config_medoid.json` follow. The likelihood version is
  `R/v0.98.0+deaths_carryforward`. A `deaths_integration.rds` saved by an earlier
  version re-runs with independent year deviations, as it was calibrated.

## est_CFR_hierarchical() documents the weak identification of tau (v0.97.3)

Documentation only. `?est_CFR_hierarchical` now states that the between-country
SD `tau` is weakly identified: the per-country factor smooth carries its own
intercept, so the fit can put the between-country spread in either term (about
0.003 on the full 1970-2025 data, 0.31 through 2024). Per-country estimates are
unaffected, and every MOSAIC location is in the WHO annual data, so no MOSAIC
prior depends on `tau`. The help page also describes the exclusion of
in-progress years and the per-country carry-forward of forecast years, both
added in v0.97.0.

## The CFR's year deviations are yearly levels, not an interpolated curve (v0.97.2)

v0.97.0 interpolated the integrated CFR's year deviations linearly between
1 July anchors. That extrapolates the within-year trend past the end of the
data. Every calibration ends partway through a year, so the forecast for the
rest of that year overshot. With the true CFR at 3% through 2024 and 1.5% in
Jan-May 2025, it forecast June-December 2025 at 1.3%, below the fitted Jan-May
level. The calibration test showed the effect on NGA (out-of-sample deaths 0.7x).

Each year's deviation is now a level for that calendar year, blended linearly
over the 60 days centred on each 1 January, so the CFR still has no step at a
year boundary. A year observed only in part is forecast at the level of its
observed months. The medoid config's posterior shift uses the same basis. The
likelihood version is `R/v0.97.2+deaths_yearlevel`.

## posteriors.json no longer copies the prior reported-CFR block (v0.97.1)

- `calc_model_posterior_distributions()` drops the top-level `mu_jt` block that
  it had copied verbatim from the priors. The reported CFR is integrated out,
  not sampled, so that block is a prior, and its calibrated value is
  `3_results/posterior/cfr_posterior.csv`. Staged estimation is unaffected,
  because `update_priors_from_posteriors()` starts from the priors.
- The `run_MOSAIC()` log line for the deaths likelihood now names the
  quasi-Poisson score and reports the median dispersion.

## CFR-v2.1 red-team fixes: a total-preserving deaths score and a corrected prior (v0.97.0)

A line-by-line red-team review of v0.96.1 (engine, likelihood, prior, completeness
and an end-to-end calibration), then fixes.

- **The integrated deaths score under-predicted deaths.** The negative-binomial
  score for the CFR level, Σ(D − m)/(k + m) = 0, weights low-count weeks about
  1/k and peak weeks about 1/m. So whenever a path's weekly shape differed from
  the data (always), the fitted CFR tracked the low weeks, not the totals.
  - On ZMB the redrawn deaths came out at 0.6-0.8× observed, and the posterior
    CFR at half the observed CFR.
  - The score is now **quasi-Poisson**. The Poisson score preserves totals, so
    given the path the fitted CFR reproduces observed deaths.
  - The Poisson log-likelihood is divided by a per-location dispersion φ ≥ 1,
    estimated from observed weekly deaths regressed on observed cases with
    year effects.
  - Implied/observed deaths moved: ZMB 0.74 → 1.00, MOZ 1.09 → 0.95; NGA, COD
    and ETH stayed at 1.00.
- **A week with no onsets but observed deaths** used to cost ~23 log-likelihood
  per death through an arbitrary 1e-10 floor. It now carries an additive
  background of `eps_rel_cases` × the location's mean scored weekly deaths, the
  same relative floor as a cases cell with zero prediction.
- **Smooth year deviations.** The CFR's year deviations interpolate between 1 July
  anchors (the rule `make_mu_jt()` uses for the prior), so the CFR and the
  redrawn deaths no longer step on 1 January. Forecast years ease back to the
  calibrated location level over half a year.
- **Deaths weighting and windows:**
  - deaths confidence weights are mass-preserving, like cases;
  - a reporting week cut by the edge of the scored window is scored on its own
    days (MWI lost 20% of its scored deaths to the leading partial week);
  - convergence is judged on the gradient when the line search stalls.
- **Deaths shape terms.** When the CFR is integrated out, the level-dependent
  deaths shape terms (peak magnitude, cumulative, WIS) are dropped with a
  warning: they scored engine deaths drawn at the prior `mu_jt`.
- **`config_medoid.json`** now carries the medoid's own posterior CFR, from the
  medoid ensemble. The ensemble's posterior is only the fallback. With the
  ensemble's, a re-simulation over-predicted the medoid's deaths by 7-12%.
- **`summary.json` `cfr_implied`** now uses the scored observed window for both
  predicted and observed totals, and weights the members. Before, predicted
  totals included the burn-in and the forecast tail, and members were unweighted.
- **The CFR prior (`est_CFR_hierarchical()`):**
  - a calendar year still in progress at its dashboard snapshot is excluded;
  - each country's trend is held at that country's own last WHO-annual year
    instead of being extrapolated (SOM, BFA, LBR, BEN end in 2022);
  - the out-of-sample coverage check now scores the observed count against its
    predictive distribution: 0.95, previously understated as 0.83-0.86;
  - `config_default` v5.1 and `priors_default` v16.1 rebuild `mu_jt` from it,
    and nothing else changes (34 of 40 locations move more than 5% in 2026);
  - `sd_product` is re-described as the centre's residual error against the
    observed CFR. It is not a product mismatch: the two products agree to
    sd(log) 0.03.
- **Displays:**
  - the trajectory CFR(t) and mass-balance panels use weighted mean series (a
    ratio of medians showed CFR 0 in sparse countries, and mass balance
    drifting by 1.6%);
  - prediction captions total the same days as their bias;
  - the true-deaths channel is labelled as reported / `rho_deaths`.
- **Data and docs:**
  - `estimated_parameters` drops the retired mortality rows (46 rows);
  - the `priors_default` roxygen matches v16;
  - the `Installation.Rmd` chunks carry `eval = FALSE` (`R CMD check` executed
    its installs);
  - `run_rolling_cv()` checks for mgcv and the WHO annual file before writing
    anything.
- **New tests:**
  - the calibration worker uses the integrated deaths score;
  - `config_medoid.json` gets the medoid's own CFR;
  - the fatality conversion uses `chi_epidemic` (threshold-forced arms);
  - totals are preserved under the redraw;
  - year boundaries are continuous;
  - in-progress-year exclusion and per-country carry-forward in the prior.
- **Resume.** The likelihood version is now `R/v0.97.0+deaths_quasipoisson`.

## Calibrated CFR outside the calibration loop; leak-free rolling CV (v0.96.1)

- **`config_medoid.json` carries the calibrated reported CFR.** Because the CFR
  is integrated out rather than sampled, the medoid config used to keep the
  prior `mu_jt`, so re-simulating it (rolling-CV medoid projections, scenarios)
  drew deaths at the prior level.
  - Its `mu_jt` is now shifted, per location and calendar year on the logit
    scale, to the run's posterior (`cfr_posterior`).
  - The within-year shape is kept.
  - A shift that needs a per-onset fatality probability >= 1 is refused, not
    clamped.
- **`2_calibration/deaths_integration.rds`** is saved, so a post-hoc
  `calc_model_ensemble(deaths_integration = readRDS(...))` redraws deaths from
  the calibrated CFR exactly as the run did.
- **The trajectory CFR reference line survives the subset optimizer.** It
  vanished when `optimize_subset = TRUE`, because the optimized ensemble carries
  no `cfr_posterior`. It now reads the candidate ensemble's posterior.
- **`run_rolling_cv()` no longer leaks post-cutoff CFR information.** Each
  cutoff T refits the WHO-annual GAM on years <= year(T) - 1 and rebuilds both
  the config's `mu_jt` and `priors$mu_jt` (centres, SEs, `sd_year`). Values are
  carried flat past that year's 1 July. The old freeze at T interpolated toward
  year(T) and year(T)+1 estimates from a fit on all years. It is removed, along
  with `make_mu_jt(freeze_after =)`.
- `est_CFR_hierarchical()` now wraps a non-writing core, `.cfr_estimate()`,
  which takes a `last_year`. Its outputs are unchanged, byte for byte.
- The priors `mu_jt` block is built by `.mosaic_mu_jt_prior()`, which both
  `data-raw/make_priors_default.R` and rolling CV use. Only the block's
  description text changed.

## CFR-v2.1: deaths decided at onset from a time-varying reported CFR (v0.96.0)

**Engine.** Each symptomatic onset is fatal with probability
`mu_jt * rho / (rho_deaths * chi_epidemic)`, drawn at onset (new rng-only draw
site `infectious/fatal_onsets`), and fatal onsets never enter Isym. Deaths are
reported on the case lag, so a death is reported in the same tick as its case.
`mu_jt` is the reported CFR by location and day, and it replaces
`mu_j_baseline`, `mu_j_epidemic_factor`, `CFR_target` and
`delta_reporting_deaths`. At epidemic PPV, expected reported deaths / expected
reported cases = `mu_jt` exactly. Replay mode keeps the laser-cholera daily
hazard verbatim for parity. `disease_deaths` now lands one results column after
the onsets that produced them.

**Deaths likelihood.** `run_MOSAIC()` integrates the reported CFR out of each
simulated path instead of sampling it. The CFR is `logit mu0_jt + a_j +
delta_{j,year}`, the deaths are scored with a weekly negative binomial, and a
Laplace step solves for the offsets
(`calc_log_likelihood_deaths_integrated()`). The `eps_rel_deaths` floor does not
apply to this score. The ensemble redraws each member's deaths from the CFR's
conditional posterior, and writes `3_results/posterior/cfr_posterior.csv`
(reported CFR by location and year). The `cfr_*` implied-CFR columns in
`samples.parquet` are removed.

**Prior.** `est_CFR_hierarchical()` is rewritten as a binomial GAM on all
WHO-annual years. It has a global trend, country intercepts, per-country drift
(`fs`, k = 10, m = 2) and a country-year random effect. Its widths are
predictive, and nothing is clamped. `make_mu_jt()` expands the estimates to the
daily matrix.

**Data objects.**
- `config_default` v5.0 carries `mu_jt`.
- `priors_default` v16.0 carries a top-level `mu_jt` block (per-year centres and
  SEs, `sd_year` 0.70, `sd_product` 0.3). The `CFR_target`,
  `mu_j_epidemic_factor` and `delta_reporting_deaths` priors are removed.
- The toy simulation configs use a constant 2% reported CFR.
- Both defaults were built as v0.95.0 plus these deltas only, not as a full
  rebuild.

**Legacy configs.** A config carrying `mu_j_baseline`, `mu_j_epidemic_factor`,
`CFR_target` or `mu_j` is handled the same way by the engine and the likelihood,
through one resolver:
- its `CFR_target` becomes a constant `mu_jt`, with a warning;
- its dead `mu_jt` matrix is ignored;
- without a `CFR_target` it is refused.

`make_simulation_config()` refuses such configs outright. The retired
`sample_*` flags and priors warn and are ignored.

**Resume.** The likelihood version is now `R/v0.96.0+deaths_integrated`, so a
resume refuses to pool shards across this change.

## mu_j_slope is removed (CFR restructure R3)

The per-location `N(0, 0.05)` prior on a linear-in-time trend in baseline IFR is
deleted, along with the engine term it fed: `run_simulation()` no longer
multiplies the mortality hazard by `(1 + mu_j_slope * tick/nticks)`, so `mu_jt`
is now two multiplicative components (per-patch baseline x epidemic escalation)
rather than three. 40 sampled dimensions go away. No other prior moves.

Four independent lines of evidence agree. It is **not estimable**: posterior
shrinkage 0.5 would need ~74,300 deaths in one country, the largest shipped
series is COD at 4,139, and all 40 locations pooled hold ~15,600; measured
posterior/prior SD on a 50,000-draw reference run is 0.969, i.e. the posterior
IS the prior. It is **not in the data**: 3 of 21 countries show a significant
weekly CFR trend and the signs are mixed, the between-country spread of the
implied trend is 7.3x wider than the prior, and in COD the weekly and annual
trends have opposite signs. It is **not in the literature**: WHO's own
Yemen-excluded global series is flat (1.7 / 1.4 / 1.5% for 2017 / 2019 / 2020).
And it is **double-counted**: `est_CFR_hierarchical()` already fits an `s(year)`
smooth, so the temporal component of country CFR is inside `CFR_target`.

**Shipped artifacts are bit-identical.** `config_default` has carried
`mu_j_slope = 0` for every location since the field existed, so `(1 + 0*t) == 1`
exactly. Verified over 26 scenarios / 722 result-channel digests in both engine
modes (`rng` and `replay`), at 40 and 1 patches, including the 1,398-tick
full-length oracle fixture: 25/26 bit-identical, the one difference being a
deliberate non-zero-slope sentinel that confirms the harness was not blind. The
golden fixtures therefore did NOT need regenerating -- which matters, because
they are frozen recordings of the read-only Python engine and could not have
been regenerated here. `make_simulation_config()` keeps a deprecated, ignored
`mu_j_slope` formal so pre-v0.95.0 configs on disk still replay.

**A correction to the evidence base.** The pre-registered claim that this term
"injects +/-30% of uncontrolled deaths level per draw" is NOT reproduced. That
figure came from a sweep running the slope out to about +/-1.2, which is 24
prior SD. Measured inside the actual `N(0, 0.05)` 95% interval (+/-0.098), total
deaths move only +/-4% (`log(deaths ratio) = 0.403 * slope`; the death-weighted
mean `t_factor` is 0.40). The identifiability report's further prediction that
the deaths-bias IQR would narrow by >=20% is also falsified (measured -2.6% to
+1.6%, i.e. noise). Removal is justified as deleting dead weight -- 40 sampled
dimensions carrying ~0.01 nats -- not as removing a large level injection.

Pinning was verified inert before removal (5 national medoids x 24 parameter
draws x 8 seeds/arm, arms paired at the PARAMETER level because `rbinom`
rejection sampling desynchronises the RNG stream): deaths ratio geomean 1.0034,
95% CI [0.9991, 1.0077], sd(log) 0.0239 -- smaller than the 0.0331 Monte-Carlo
noise floor of the same comparison. Cases pooled ratio 1.0000.

## R CMD check regressions from R6/R1/R2 are fixed

A paired `devtools::check()` at the branch base and at v0.94.0 showed the latter
had added 5 warnings and 1 note. All traced to two causes, both now fixed: a
malformed roxygen block in `calc_model_likelihood.R` (bare `\item`s outside any
container, cascading into the install / Rd files / Rd cross-references warnings
and the Rd contents note, plus 9 undocumented arguments), and non-ASCII
characters in shipped description strings. Also fixed: the spurious
`sample_parameters.Rd` "missing link `1, 14`" from `[1, 14]` parsing as an Rd
link, and a genuinely missing `@param is_diagnostics`.

Note for contributors: the CLAUDE.md check baseline of `0E/3W/2N` is wrong --
the real base is `0E/2W/4N` -- and `R CMD check .` cannot pass on this package
at all, because `Authors@R` is only expanded at build time, so a source-directory
check always reports `1 ERROR: Required fields missing or empty 'Author'
'Maintainer'`. Use `devtools::check()`.


## The epsilon floor is sized per channel (CFR restructure R1)

0.93.0 put the eps-floored density in place but applied **one constant, 0.02, to
both channels**. That is the right fraction for cases and roughly 12x too small
for deaths. Because production scores a **single stochastic realisation** per
draw, a low-count deaths series is mostly structural zeros; each zero-against-a-
positive-observation cell is scored at `log NB(y | eps)`, so too small a floor
makes those cells ruinous and the likelihood optimum moves onto draws that
**over-predict the deaths level by ~2.5x**. This is a scoring-rule (Jensen)
artifact -- `E_seed[LL(est)]` peaks far above `LL(E_seed[est])` -- not a CFR
misspecification: the NB scale-MLE on the mean path is unbiased for every `k`.

`calc_log_likelihood_negbin()` and `calc_log_likelihood_poisson()` gain an
`eps_rel` argument (default `0.02`, so every existing call is unchanged), and
`calc_model_likelihood()` gains `eps_rel_cases` (0.02) and `eps_rel_deaths`
(**0.25**), exposed as `control$likelihood$eps_rel_cases` /
`eps_rel_deaths`.

**How 0.25 was chosen.** A profile over a multiplicative scale on
`mu_j_baseline`, on four contrasting countries (ETH, COD high-burden, MOZ,
KEN degenerate-sparse), 4 accepted base draws x 3 seed groups x 12 stochastic
replicates each, with a **real 365-day holdout** masked out of the likelihood.
Every eps arm re-scores the identical cached simulation pool, so the arms are
exactly paired. Per-block median deaths bias at the likelihood optimum:

| `eps_rel_deaths` | 0.02 | 0.05 | 0.10 | 0.15 | 0.20 | **0.25** | 0.30 | 0.40 | 0.50 |
|---|---|---|---|---|---|---|---|---|---|
| in-sample bias  | 2.34 | 2.12 | 1.74 | 1.33 | 1.12 | **0.97** | 0.87 | 0.50 | 0.35 |
| held-out bias   | 1.43 | 1.40 | 1.39 | 1.29 | 1.20 | **1.11** | 1.10 | 0.98 | 0.80 |

0.25 minimises `|log bias_in| + |log bias_out|` both pooled over all four
countries and pooled over the three where the mechanism operates. **0.5 is past
the crossing** (in-sample bias 0.35, a 3x under-prediction) and is not used.

**Cross-check.** Replicate-averaging -- scoring the mean of `n` realisations at
the *unchanged* 0.02 floor -- moves the same pooled in-sample bias 2.51 (n=1) ->
1.20 (n=6) -> 1.10 (n=24), landing where the eps route lands. The two
independent routes agree, as the Jensen diagnosis requires.

**Known limit.** On a very sparse deaths channel (KEN: 0.07 deaths/day, every
non-zero day equal to 1) `eps_rel` is inert -- the floor never binds -- and the
bias there is not eps-mediated. Replicate-averaging does move KEN. The eps fix
is the cheap 90% of the problem, not all of it.

The pinned values in `test-calc_model_likelihood_reference.R` shift by
0.57-1.12 nats; each is re-derived from an independent hand computation
carrying the per-channel eps. `.mosaic_likelihood_impl_version()` is bumped so
resume refuses to pool shards scored under the old floor (it was **not** bumped
at 0.93.0, which also changed likelihood values).

## The likelihood scores every cell by its density (arm A1b; CFR line 0.93.0)

`calc_log_likelihood_negbin()` and `calc_log_likelihood_poisson()` special-cased
a zero prediction against a positive observation with
`ll <- -observed[i] * log(1e6)` -- a loss **linear in the observed count**, not a
log-density, and a zero-vs-zero cell with a flat `ll <- 0`. The linear term
carried essentially all of the log-likelihood's between-draw variance, so the
ensemble's delta-AIC ranking described that constant rather than model fit.

Every cell now goes through the density with the mean floored at
`eps = max(1e-4, 0.02 * mean(observed))`. The floor is **channel-relative**: a
fixed absolute floor calibrated on cases (mean ~30/day) sits far above the
typical deaths rate (ETH ~0.4/day), which would flatten the predicted rate above
the observations and destroy discrimination exactly where the deaths signal is.

Validated across ETH, MOZ and COD at a 6-month holdout: held-out cases MAE falls
from 26.4 to 19.7 pooled, and deaths MAE from 1.07 to 0.59, with bias moving
toward 1 on both channels.

## Note on the two changes in the CFR line's 0.92.0-0.93.0

The conditional dispersion estimator (0.92.0) and the epsilon-floored density
(0.93.0) were measured together in a 2x2 factorial at a 6-month holdout. A1b
improves held-out skill on both channels. The estimated dispersion is
consistently *below* the retired floor of 3 (ETH 1.78 cases, MOZ 0.43, COD 0.98),
which flattens the likelihood; on that experiment it degraded held-out MAE, most
sharply for MOZ. Both are retained: the estimator is the statistically correct
observation model, and the sharpness it removes is a separate concern that
belongs in an explicit temperature rather than in the dispersion. Set
`control$likelihood$nb_k_cases` / `nb_k_deaths` to override the estimate if a
sharper kernel is wanted for a given run.

## NB dispersion is now estimated, not floored (CFR line 0.92.0)

`calc_model_likelihood()` previously estimated the negative-binomial dispersion
`k` with a marginal method-of-moments form, `k = m^2/(v - m)`, computed across
the whole observation series. By the law of total variance that estimates
`Var(mu)` -- the epidemic signal -- rather than the observation dispersion the
surrounding comment claimed it measured. Consequently the `nb_k_min_*` floor
bound in **27 of 28** estimable locations for cases and 17 of 20 for deaths, so
the shipped "estimator" returned the constant 3 almost everywhere. On synthetic
data with a known `k = 4`, the old form returns 1.10; the new one returns 3.88.

**New:** `est_nb_dispersion()` estimates `k` per location by conditional maximum
likelihood (`MASS::glm.nb`) at the data's native **weekly** reporting cadence,
honouring the per-observation `reported_*_weight` confidence weights. The mean is
modelled with a spline trend plus seasonal harmonics, following the
Farrington/Noufaily convention used by the `surveillance` package.

* **Weekly aggregation** with the reporting-week boundary **detected** per
  location, not assumed. All 40 current locations report Monday-Sunday; the
  estimate is invariant to the config's start weekday.
* **Computed once per calibration**, not inside the likelihood. `k` depends only
  on the observations, so the previous code recomputed an identical value roughly
  7.2 million times per 40-location, 30,000-simulation run.
* **Edge cases are explicit.** All-zero and otherwise uninformative series, and
  a dispersion running to the Poisson boundary, resolve to `k = Inf` (Poisson).
  A six-rung mean-model ladder handles IRLS failures on series with long zero
  runs. Every location resolves to a finite `k` or Poisson -- never `NA`.
* **Cross-location shrinkage** toward a mean-dispersion trend (DESeq2-style, with
  a no-shrink escape) stabilises sparse locations.
* Diagnostics are written to `2_calibration/diagnostics/nb_dispersion.csv` and
  summarised in `summary.json`, including the **bound-bind rate** -- in a
  well-specified fit the hard bounds should rarely bind.

## Breaking changes (CFR line 0.92.0)

* `control$likelihood$nb_k_min_cases` / `nb_k_min_deaths` are **retired**. Setting
  either now warns and is ignored. To set the dispersion explicitly use
  `control$likelihood$nb_k_cases` / `nb_k_deaths`, which **replace** the estimate
  (scalar or one value per location) rather than silently flooring it.
* `calc_model_likelihood()` gains `nb_k_cases` / `nb_k_deaths` (scalar or
  length-`n_locations`) in place of `nb_k_min_cases` / `nb_k_min_deaths`. Passing
  a vector previously either collapsed to `max()` without warning or errored.
* `calc_log_likelihood_negbin()`'s `k_min` is deprecated and ignored.
* `check_overdispersion()` and the internal `.nb_size_from_obs_weighted()` are
  removed; both are superseded by `est_nb_dispersion()`.
* `calc_cases_from_infections()` and `calc_deaths_from_infections()` are
  removed. Neither had a caller, and the deaths one was a third, divergent
  copy of the CFR algebra.
* **All calibration results change.** Every likelihood value moves, so previous
  runs are not comparable. The likelihood-provenance string used by the resume
  guard is bumped accordingly, so resuming a pre-0.92.0 run stops with an
  actionable error rather than silently mixing two scoring rules.
* New dependencies: `MASS`, `splines`.
# MOSAIC 0.93.2

- The `calc_Reff()` kernel caveat no longer hand-escapes its percent signs, which main's roxygen guard (`test-mobility-od.R`) rejects.

# MOSAIC 0.93.1

## Post-merge review of the route-decomposed R_eff

Four independent post-merge reviews of 0.92.1 (maintainer, statistician, swe, disease-modeler) found the estimator correct. This release fixes what they flagged.

- **Run inputs are written at 17 significant digits.** `run_MOSAIC()` wrote `1_inputs/config.json`, `priors.json`, `control.json`, `environment.json` and `summary.json` with jsonlite's `digits = NA`, which keeps 15 significant digits and does not round-trip a double (0.1 + 0.2 comes back as 0.3). A member rebuilt from those files then differs by ~1e-15 relative, and the engine's integer rounding and binomial draws amplify that into a different trajectory. On a 4-location config, 31 of 80 re-simulated members were not bit-identical and the 95th-percentile total-case error was 10%, twice the tolerance of the `add_reproductive_numbers(recompute_ci = TRUE)` faithfulness gate, which would refuse the run. Single-location configs were unaffected. The resume integrity check accepts `1_inputs` written at either precision, so runs started before this release still resume. Run directories written before this release keep 15-digit inputs, and `recompute_ci` can still refuse multi-location runs there.
- **`peak_Rt` is the time-max of a 7-day Cori window.** It was the time-max of the daily ratio, which lands on low-count days: in 15 of 18 test location-runs the peak fell where route infectiousness was 1-3, and raising the floor from 1 to 10 halved it. Each member's peak is now the maximum of its trailing 7-day R (sum of infections over sum of infectiousness), computed on the worker. The window is recorded in `attr(, "peak_Rt_window")` and shown in the `plot_Reff()` annotation.
- **The re-simulation survives a dead worker.** It used `parLapplyLB()`, which blocks forever on Linux when a worker is OOM-killed or segfaults. It now uses the same worker-death-robust gather as `calc_model_ensemble()`; a dead worker fails the run with a count.
- **Documentation:**
  - The comment claiming that ignoring disease mortality moves the kernel means by under 0.2 day was wrong. With `config_default` rates the human kernel mean is 9.1 d at no mortality, 8.0 d at 0.017/day and 6.5 d at 0.058/day, and on engine runs at those rates R_env reads 2.5% and 6% low. At the median rate (~0.002/day) it is negligible. The kernel still ignores mortality.
  - The `calc_Reff()` caveat now says that suitability enters R_env twice (transmission rate and reservoir lifetime), so R_env > 1 in a high-suitability season is not a growth threshold, and that R here is not comparable to literature R estimated with a ~5-day serial interval.
  - `plot_Reff()` describes the stacking rule it uses since 0.92.1.
- **Tests now pin the timing.** A one-day lag error in the human infectiousness, dropping the initial latent stock, and a one-day shift in the reservoir decay each fail at least one test (checked by mutation); before, the lag errors passed the engine-truth tests and dropping the latent stock passed every test.

# MOSAIC 0.93.0

## New: automated data refresh and the overland mobility OD pipeline

- `update_mosaic_data()` / `list_mosaic_data_steps()` run a registry of every data build that can be refreshed automatically (CLI: `inst/scripts/update_mosaic_data.R`); `check_mosaic_data_freshness()` and `check_mosaic_manual_inputs()` report what is stale and what must be refreshed by hand.
- Dated-snapshot downloaders `download_EMDAT_data()`, `download_IDMC_data()`, `download_UN_WPP_data()`, `download_WB_data()` and `download_mobility_od_sources()` write atomic, never-overwritten snapshots into `MOSAIC-data/raw/<source>/` with a provenance row, per the root CLAUDE.md exception for automated snapshots.
- Overland mobility: `get_travel_time_matrix()` builds a least-cost travel-time matrix; `process_mobility_od_data()` fuses four bilateral sources into one OD structure; `rake_mobility_od_to_tau()` rakes it to per-country departure margins; `est_overland_tau_prior()` turns that into a per-country overland departure-rate prior; `plot_mobility_fused()` draws the figures.
- The shipped `config_default` / `priors_default` are **not** rebuilt in this release. A rebuild that sources `tau_i` from `est_overland_tau_prior()` is held back until it is reconciled with the CFR v2.1 schema change.

## Fixed: hand-escaped percent signs truncated manual pages

Under Roxygen markdown a hand-written `\%` renders as `\\%`, which Rd reads as a comment start, so the rest of the line was silently dropped from ~20 manual pages (e.g. the 95% CIs in `get_rho_care_seeking_params()`). Now plain `%` throughout; `test-mobility-od.R` guards against reintroduction.

## `update_mosaic_data()` builds data and no longer fits models

`est_suitability` (group 4B) is **removed from the registry**. Fitting the suitability LSTM is model fitting, not data building, and it does not belong in the data-update driver: it needs the TensorFlow/keras Python environment, budgets ~6 GB per seed worker, and runs for hours, so it needs its own schedule and its own failure handling. Call `est_suitability()` directly, or run it through the calibration workflow.

`compile_suitability_data` (group 4A) **stays, and is now in the default plan.** It is a data compile — it assembles the LSTM training panel from its 13 upstream producers (climate, ENSO, demographics, the multi-source surveillance combine, mobility, epidemic peaks, EM-DAT, all four World Bank indicators, WASH, elevation) — so an ordinary run now keeps the suitability *data* in step with its inputs. Previously it was held back along with the fit and could silently fall behind.

With no model fit left to hold back, `include_suitability` is **removed** from both `update_mosaic_data()` and `list_mosaic_data_steps()`, the `--suitability` CLI flag is removed from `inst/scripts/update_mosaic_data.R`, and `.mosaic_select_steps()` loses its fourth argument. Nothing is filtered from the default plan any more: every one of the 54 registry steps is a data build. `steps=` / `skip=` are unchanged and still accept `"4"`, `"4A"` or the step id.

That gate had already failed once — it compared `s$group` to `"4"` when the ids are `"4A"`/`"4B"`, matched nothing, and left the multi-hour fit in the *default* plan (fixed in v0.90.6, in two sibling sites). Deleting the step retires the whole class of failure rather than the instance. `test-update_mosaic_data.R` now asserts `est_suitability` appears in no step id **and in no step body**, so it cannot be reintroduced by either route.

## Fixed: the CLI wrapper silently overrode the `date_stop` default

`inst/scripts/update_mosaic_data.R` passed a bare `Sys.Date()` whenever `--date-stop` was omitted, defeating `update_mosaic_data()`'s own `Sys.Date() + 540` default. That is precisely the shortfall the +540 default exists to prevent: it produces a vaccination matrix 139 days shorter than the psi forecast horizon, and `make_config_default.R` then fails validation with `nu_1_jt must be a matrix with ... columns equal to the daily sequence from date_start to date_stop` — an error that names the wrong culprit. The wrapper now matches the function default, and `--help` no longer advertises "default: today".

## `alpha_1` is now PINNED by default

`sample_alpha_1` flips `TRUE` -> `FALSE` in both places that carry the default: `mosaic_control_defaults()$sampling` (`R/run_MOSAIC.R`) and `default_sample_args` (`R/sample_parameters.R`). A 40-country run now draws 640 location-specific values instead of 680 — exactly the 40 per-location `alpha_1` — and `alpha_1` comes through as the shipped `config_default$alpha_1`, seed-invariant. `sample_alpha_1 = TRUE` still works as an explicit override for a deliberate mixing-exponent experiment.

**Why.** `alpha_1` is collinear with `log(beta_j0_tot)` in the endemic regime (`beta * X^alpha_1`) and with any coupling multiplier at invasion, where the bracket collapses to the imported term and `log Lambda = log beta_j + alpha_1(log c + log M_j) - alpha_2 log N_j`. The 250,000-draw continental posterior (`stage3_continental_b21_a1loc`) moved `alpha_1` by a median **0.057 prior SD** against a random-subset null of **0.146** — it was learning nothing while costing 40 free dimensions and destroying cross-country comparability of the `beta_j0_tot` posteriors. The disease-modeler memory `reference_alpha_mixing_exponents.md` recommended pinning both alphas in the 40-country spatial fit; production had been doing the opposite because only one of the two default sites was ever consulted. `alpha_2` was already pinned and is unchanged.

**The value 0.27 is deliberately unchanged.** Pinning is about *freedom*, not level. MOSAIC's patches are whole countries — weakly-coupled aggregates of many sub-populations — so strong sub-linear mixing is the intended national-scale behaviour. The published 0.90–0.98 values (Xia 2004; Giles 2020; `tsiR`) come from community- and city-scale measles models, which are far closer to well-mixed and are not the right comparison class.

**Known consequence, documented not fixed.** Because the exponent applies to the pooled bracket, `alpha_1 < 1` damps imports when local prevalence dominates (~3.7x at 0.27) but *amplifies* them when the bracket is the imported term alone, which is the invasion regime: `x^0.27 > x` for `x < 1`, so an imported pressure of 0.019/day is treated as 0.342 (18x). Invasion probability therefore does not scale linearly with travel volume. This is a property of the FOI's structure, not of pinning, and pinning does not change it — but it now holds at a fixed exponent rather than a sampled one. See `MOSAIC-notes/2026-09-18 spatial FOI coupling research.md`.

`test-alpha1-pinned-default.R` pins the default at both sites and asserts they agree, so the two cannot drift apart again; it also re-checks the engine invariant `alpha_1 in (0, 1]` for the shipped scalar-or-length-nL form.

# MOSAIC 0.92.3

## psi_evolve closed out: the correctness fixes ship, the experimental architectures do not

The psi_evolve programme (waves 0-34 plus a 72-cell downstream A/B on dugong) found no psi variant that beats the production LSTM once psi is pushed through calibration. DLinear, D9b (static-covariate country embedding), N8 (per-country loss balancing) and the D9b+N8 branch defaults all fit in-sample WORSE than production (12/12 cells for DLinear and D9b+N8) and none improved out-of-sample WIS beyond the psi-seed noise floor. None of that experimental code is merged; the full history is kept under the git tag `archive/psi-evolve`.

What does ship are the defects the programme found in the production psi path:

- **The deployed model was trained past its best epoch.** The inner CV recorded the epoch training *stopped* at (best + `patience`), and the full-data refit ran for that many epochs with no early stopping, overshooting the optimum by up to 10 epochs (40-60% on the production schedule). New `.psi_epoch_from_history()` returns `argmin(val_loss)` when best weights were restored.
- **The leak-free v7.4 panel had a leaking target.** Response variables were normalised by p99 anchors computed over rows after the cutoff. New `compile_suitability_data(target_anchor_stop=)` bounds the anchor rows; `prefit_rolling_cv_psi()` passes the cutoff and folds it into the psi cache key, so a panel built under the old anchor is never silently reused. Default `NULL` leaves the canonical panel unchanged.
- **ISO-8601 week labelling** was wrong in the surveillance/climate processors, and the RW-CV grid now accepts day-based geometry (a day-based stride is no longer multiplied by `rw_subsample`).
- **psi RW-CV:** optional forecast `lead` and validation input context; per-fold held-out predictions are retained; the drop-filled-tail guard fails loudly; the psi manifest records fit provenance (and no longer references an undefined `backend`).

# MOSAIC 0.92.1

## R_eff is now decomposed by route: R_eff = R_hum + R_env

`calc_Reff()` used to divide total infection incidence by one generation-interval kernel: latent plus infectious period, moment-matched to a Gamma. That timing describes the human route only. Environmental transmission also passes through shedding and 16-200 days of survival in the reservoir, and it carries 99.4-99.9% of infections in every post-v0.89.0 calibration checked (MOZ, COD, ETH). With a ~3-5 day kernel applied to a ~30-200 day process, the old estimator compressed R strongly toward 1. The kernel's human part also used the mean infectious duration where the renewal needs the transmission-weighted mean infectious age.

Each route now has its own numerator (`incidence_human`, `incidence_env`) and its own infectiousness. Both are driven by total incidence, because every infection is infectious through both routes, and R_eff is their sum. The kernels are derived from the engine's own daily transition probabilities and phase order.

The environmental term is **instantaneous** (Cori: "if conditions stayed as they are at t"). The reservoir is rebuilt from the actual past decay path, and one infection's lifetime reservoir contribution is valued at today's `delta_jt`. Nothing after t enters, so truncating a series (e.g. at a forecast cut-off) leaves earlier values unchanged. People latent or infectious on the first day are included in both infectiousness terms, so no initial-condition mask is needed.

- `calc_Reff()`:
  - returns rows for `estimand` `"R_eff"`, `"R_hum"` and `"R_env"`;
  - needs the `incidence_human`/`incidence_env` channels (plus `E`/`Isym`/`Iasym` for the initial stocks) and the config's `zeta_*`, `psi_jt` and `decay_*`;
  - checks that the config's locations and start date match the trajectories;
  - caps decay rates above 1, which occur when `decay_days_short < 1` day;
  - `max_days` is removed;
  - the caveat now states that the renewal is per location, so in multi-location runs imported human-route spread is credited to the destination.
- `add_reproductive_numbers()`:
  - builds the kernel from `2_calibration/best_model/config_medoid.json`, not the input config of prior centres, falling back with a warning; attribute `config_source` records which;
  - applies the burn-in on both paths;
  - re-simulated members use their own kernel, engine `delta_jt` and initial stocks;
  - `peak_Rt` gains an `estimand` column, and cell quantiles need half the member weight defined;
  - `overwrite = FALSE` no longer keeps an older total-only table;
  - an explicit `burn_in_days = 0` now disables the burn-in (it used to become 30), and a negative value is an error.
- `plot_Reff()`:
  - stacks R_hum on top of an R_env area, and both are smoothed over the same days;
  - the stack is drawn on every day the total is defined; a silent route counts as 0, as it does in `calc_Reff()`;
  - the new `routes` argument is last, so existing positional calls are unchanged.
- The internal `.mosaic_generation_time_pmf()` is removed. `get_generation_time_distribution()` is unchanged.

Tests check against the engine rather than against the code's own algebra:
- R_hum and R_env recover the engine's true instantaneous R in single-route linear runs (median ratios 0.99 and 0.94 on the test seed). The 0.94 is not a bias: that seed's realized symptomatic share was 0.21 against sigma = 0.24, which a mean-field reconstruction cannot see. Across seeds, R_env against a mortality-aware reference built from the engine's reservoir has a median ratio of about 1.00 (range 0.92-1.05).
- I and W rebuilt from incidence track the simulated stocks and align best at zero lag.
- Truncation invariance, and a brute-force check of the frozen-at-t definition.
- The re-simulation path is exercised end to end through the real engine.

On the post-v0.89.0 MOZ medoid, the 14-day-mean R_eff has an interquartile range of 0.54-1.71 and a p95 of 3.1; the old estimator gave 0.96-1.09 and 1.19. Old and new R_eff files are not comparable.

# MOSAIC 0.84.0 - 0.91.x (changelog reconstructed from the commit history)

These releases shipped without NEWS entries; the summaries below are taken from their commit messages. Two of them change model behaviour.

## 0.89.0: two engine corrections (results are not comparable across this boundary)

Both are places where the R port faithfully reproduced laser-cholera 0.16.1 while laser-cholera diverged from `MOSAIC-docs/04-model-description.Rmd`, so neither could be found by comparing R to Python.

- **The symptomatic split is stochastic.** E -> I is now split binomially, as the spec's stochastic-transitions table states, instead of `round(sigma * progressing)`. `round()` is not linear, so the old form was wrong in the mean: at `sigma = 0.2` it gave zero symptomatic for every `n <= 2`, which suppressed the observed arm on 15.4% of patch-days with `E >= 1` (28-41% in GNB/COG/NAM) -- exactly where outbreak onset is decided.
- **The environmental dose-response is per capita.** `Psi = beta_jt_env * (1 - theta_j) * D / (kappa + D)` with `D = W / N`. The reservoir `W` is extensive while `kappa` is a concentration; comparing them pinned the response at 1 (0.9994 from a single symptomatic person; 95.7% of patch-days above 0.99), so `kappa`, `zeta_1`, `zeta_2`, `zeta_ratio` and the decay parameters were flat directions whose posteriors returned their priors. At `kappa = 1e6` the response now half-saturates near 0.3% symptomatic prevalence and the realised human share near onset moves from 0.13% to ~25%, without touching `p_beta` or any other prior. Replay mode keeps the raw-`W` form for port parity (`.SIM_RNG_ONLY_CORRECTIONS`).
- **`kappa` is fixed by default in calibration.** `mosaic_control_defaults()` now sets `sampling$sample_kappa = FALSE`, so `run_MOSAIC()` holds `kappa` at its config value (`1e6`) instead of sampling it. The per-capita dose-response gives `kappa` a meaning, but the data still cannot estimate it. Pass `sample_kappa = TRUE` in `control$sampling` to sample it as before. A direct `sample_parameters()` call still defaults to `sample_kappa = TRUE`.

Calibrations, posteriors and R_eff estimates from before 0.89.0 should not be compared with later ones.

## 0.90.x: exact importance-sampling diagnostics

- 0.90.0: the best-subset posterior **saturates** delta-AIC at 4 (`pmin(delta, 4)`) rather than applying the Delta <= 6 cut-off the spec described, so subset weights lie in `[exp(-2), 1]` and `ESS_B` is inflated by construction. This is now documented, and the honest numbers are reported alongside: new `calc_is_diagnostics()` (exact untruncated IS ESS + Pareto k-hat) and `summary.json` fields `ess_is_best`, `ess_is_all`, `ess_is_all_prop`, `khat_all`, `khat_all_status`, `n_positive_ratios_all` -- reported, never gated. New `control$targets$best_subset_weighting` (`"saturated"`, the unchanged default, or `"tempered"`). `codetools` declared in Suggests.
- 0.90.1: `alpha_2` validation accepts the documented `[0, 1]` and rejects values above 1.
- 0.90.2: the subset diagnostics score the subset the posterior actually uses. 0.90.4: `khat_status` reports usability, not just convergence. 0.90.5: corrected `tempered` weighting description; degenerate subsets guarded.
- 0.90.6-0.90.11 (suitability RW-CV): ISO-8601 week labelling fixed and day-based RW geometry; forecast lead and validation input context; per-fold held-out predictions retained; the psi drop-tail guard fails loudly and the manifest records fit provenance; a day-based stride is no longer multiplied by `rw_subsample`; an undefined `backend` reference removed from the manifest.

## 0.91.0

- `write_trajectory_csv()` exports the ensemble trajectory channels (incidence, compartments, derived channels) as CSV, so they reach the results archive in a readable form.

## 0.84.0 - 0.88.x: calibration pipeline performance and robustness

- 0.84.0: the ensemble RAM projection counts the config broadcast.
- 0.85.0: `add_reproductive_numbers()` gains `n_cores`; the R_eff re-simulation (`recompute_ci = TRUE`) runs on a PSOCK cluster.
- 0.86.0: reverted the `open_dataset()` shard combine from 0.79.0 (2.1x slower on production hardware).
- 0.87.0: `control$io$shard_batch_size` default 1 -> 100 (57x faster combine, 116x faster resume scan, 34.6x smaller on disk).
- 0.88.0: the combine's small-file branch unifies shard schemas instead of silently dropping columns a later shard adds. 0.88.1: the implied-CFR identity uses the post-#67 form.

# MOSAIC 0.83.0

## Every PSOCK cluster now clamps to the connection budget

R allocates its connection table at startup: 128 slots, three already held by `stdin`/`stdout`/`stderr`. Every PSOCK worker holds one slot for its lifetime, so a default R build cannot exceed ~125 workers no matter how many cores the host has. `make_mosaic_cluster()` has clamped to that budget for some time; the package's two other cluster sites did not.

`calc_model_ensemble()` called `parallel::makeCluster()` raw, on an `n_cores` that `run_MOSAIC()` derives straight from `control$parallel$n_cores` — the *unclamped* value, whenever `run_MOSAIC()` builds its own cluster (the `cluster = NULL` path, which is every ordinary run; the `ens_n_cores` derivation added in v0.73.1 only reads the clamped length when a cluster is *supplied*). On dugong at `n_cores = 170` that call threw `all 128 connections are in use`, the `tryCatch` around both ensemble calls turned the throw into a `log_warn`, and the run finished reporting success with **no posterior ensemble, no medoid ensemble, no predictions and no figures** — after paying for the entire calibration. Verified on dugong: the raw call fails at 170 while the clamped one succeeds at 123.

New internal `.mosaic_clamp_psock_workers()` (`R/make_mosaic_cluster.R`) is now the single place that decision is made, wired into all three PSOCK sites — `make_mosaic_cluster()`, `calc_model_ensemble()` and `ensemble_suitability()`. An over-budget request now runs narrower and explains itself, instead of throwing. `run_MOSAIC()` additionally reports the budget once at startup, so a shortfall appears in the log at minute 0 rather than being inferred from a missing artifact hours later. `test-psock-connection-clamp.R` pins the behaviour and asserts that *every* `parallel::makeCluster()` site in `R/` routes through the clamp — guarding the asymmetry, not just this instance of it.

## Raising the ceiling: `--max-connections` in the VM wrappers

R >= 4.4.0 accepts `--max-connections=N` (128 to 4096) to enlarge the table. It is a **startup** option: there is no environment variable, and nothing in-session can change it, so it cannot be fixed from R. The new tracked `vm/make_wrappers.sh` generates `~/bin/r-mosaic-{R,Rscript}` for both compute VMs with `--max-connections=512`, superseding the gitignored `claude/dugong_setup/make_wrappers_dugong.sh`. Every `LD_PRELOAD` in it is `[ -f ]`-guarded, so one generator serves hedgehog's GLIBCXX problem and dugong's libexpat/libssl ones; the generated wrapper reproduces the previous `LD_PRELOAD` chain byte for byte.

Measured on dugong at `n_cores = 170`: **170 workers granted (was 123)**, cluster startup 16.6 s (was 13.6 s), 176 of 1024 file descriptors, 121 GB of 1511 GB resident. The connection table — not memory, not descriptors — was the binding constraint, and ~28% of the machine was being left idle. The flag precedes `"$@"` so a caller's own later value still wins (R takes the last occurrence); a flag placed *after* the script filename is ignored by R entirely. hedgehog (120 cores, `n_cores = 118`) sits under the default ceiling and does not need this; it gets the flag so both VMs behave identically.

`inst/examples/forecast_cv_experiment.R` now derives its calibration cap from the live connection budget instead of a hard-coded `FORECAST_CV_PSOCK_CAP=120`; an explicit env var still overrides.

# MOSAIC 0.74.0 - 0.82.x (changelog reconstructed from the commit history)

These releases shipped without NEWS entries; the summaries below are taken from their commit messages.

- 0.74.0: `inst/bench/`, a multi-version calibration benchmark suite.
- 0.75.0: `process_IDMC_data()` builds displacement panels from IDMC IDU records.
- 0.76.0: **`alpha_2` is pinned by default** (`sampling$sample_alpha_2 = FALSE`); it is weakly identified and suitability absorbs its signal. Set it `TRUE` to restore the old behaviour.
- 0.77.0: **`config_default` rebuilt** on the 2018-01-01..2027-02-04 window (3,322 ticks, was 1,398) with refreshed psi and a newer UN WPP vintage. The demographic-trend test tolerance is now expressed per simulated year (0.8%/yr). 0.77.1-0.77.2: remaining laser-cholera relics and the dead `data-raw/mosaic_python_env.R` removed.
- 0.78.0: the Python environment is optional and suitability-only; `library(MOSAIC)` no longer initialises Python (~5.2 s saved per interactive session).
- 0.79.x: shard combine via `open_dataset()` (reverted in 0.86.0); four R CMD check warnings cleared and the build tarball shrunk 105x.
- 0.80.0: `render_MOSAIC_figures()` renders per-location figure families across PSOCK workers (`cl` / `n_cores`); the vignettes ship in the package. 0.80.1: the resume scan reads simulation ids from the `sim` column, not the filename.
- 0.81.0: parallel rendering actually engages (the cluster previously failed to start and fell back to serial); shard batching added. 0.81.2: the optimised ensemble keeps its scored-cell mask. 0.81.4: `results_all` restored.

# MOSAIC 0.73.1

## Leaked PSOCK workers are now reaped in production, not just in tests

v0.73.0 fixed this for the test suite only, and said so. The production path had the same defect: `parallel::stopCluster()` shuts a worker down by writing to its socket, so a worker that is not *reading* that socket — an interrupted run, a task stalled inside `run_simulation()` and abandoned by the gather — survives its own cluster. It then holds the parent's stdout open, so a **finished** `run_MOSAIC()` looks like it is hanging: no output from `tail`, no R master in the process table. It also holds ~1 GB of RSS and one of R's 128 connection slots, which is enough to make a later cluster creation in the same session fail.

New internal `.mosaic_stop_cluster()` (`R/cluster_teardown.R`) calls `stopCluster()` and then SIGKILLs any recorded worker PID whose `/proc/<pid>/cmdline` still contains `RSOCK`. Wired into all four production teardown sites: both in `run_MOSAIC()` (the `on.exit` handler and the explicit post-calibration stop), `calc_model_ensemble()`, and `ensemble_suitability()`. `make_mosaic_cluster()` records the worker PIDs on the cluster object as the `"mosaic_worker_pids"` attribute **at creation**, because they cannot be asked for at teardown time in the one case that needs them — `clusterEvalQ(cl, Sys.getpid())` would queue behind the very task that is stuck. For a cluster built elsewhere it falls back to querying, which is never worse than the old behaviour. Linux-only and `RSOCK`-gated by design; elsewhere, and for `FORK` workers, it degrades to plain `stopCluster()`.

`tests/testthat/helper-cluster.R` is now a set of thin wrappers over the package functions rather than a second copy of the logic, so `test-cluster_teardown.R` (which is the only thing that can catch this becoming a no-op — it did once, see lesson 18) now tests the production code.

## Post-calibration ensembles honour a caller-supplied cluster

`run_MOSAIC(cluster = cl)` ran the calibration on the supplied workers and then ran both post-calibration ensembles **serially**, because `calc_model_ensemble(parallel = )` was read from `control$parallel$enable` alone — a key a caller who has already handed over a cluster has no reason to have set. The dugong recipe does set it, so production was unaffected, but the inconsistency is real. A supplied cluster is now treated as consent to parallelise and sizes the ensemble cluster (`ens_parallel` / `ens_n_cores`). The ensembles still build their own cluster: `cl` has just been stopped, and a caller-provided one is the caller's to manage.

## R is now faster than the Python engine it replaced (measured)

`migrate-laser-r.md` recorded R as 1.69x **slower** than laser-cholera and left the figure flagged as un-repriced after the v0.73.0 win. It has now been measured in the same interleaved paired harness as the rest of this work (6 blocks x 3 reps, both engines reading the same `config_default.json`, both thread-pinned, numba warm-up untimed):

| arm | min (s) | mean (s) | R faster by, per block |
|---|---|---|---|
| `py_full` | 0.6765 | 0.7107 | — |
| `r071_json` (`config = <path>`) | 0.5810 | 0.7233 | 1.15-1.23x |
| `r071_rda` (`config = <list>`) | 0.4000 | 0.5488 | 1.65-1.72x |

So the migration's headline cost has gone to zero and turned slightly positive. Two things worth carrying forward: the within-arm spread across blocks exceeds the between-arm gap, which is why these are quoted per-block and paired; and the two R arms differ by ~0.18 s of cold config parsing against Python's 0.066 s setup, making **config parsing, not the tick loop, the largest remaining single cost on the cold path**. Recorded as "Addendum 2" in `migrate-laser-r.md`, which also marks the old 1.69x decision paragraph as superseded.

## Documentation

- `perf-next-steps.md` (new) records the two deliberately deferred items — the C++ engine plus the two-engine architecture question, and the step-2 sampling-efficiency findings (objective noise before draw count; the ΔAIC-4 truncation question) — with the reasoning for each and the sequencing if they are picked up.
- The `~2 GB/worker` figure, which was the *Python* engine's footprint, is corrected to the measured ~1.0 GB in `.claude/skills/dugong-run/SKILL.md` and `.claude/skills/run-mosaic/SKILL.md`. `CLAUDE.md` already carried the right number.

# MOSAIC 0.73.0

## Engine runtime: 2.25x more, from one state write that was copying the whole run

v0.72.0 took the engine 1.156x and reported that the remaining profile was flat. It was not. The single largest cost in the engine was a **copy of the entire per-channel pointer vector on every state write**, and it had been hiding in plain sight for the same reason it hid at the port: the profiler charges it to the phase bodies and to `<GC>`, not to anything that looks like a state write.

Every change here is **bit-identical**: verified across 5 configurations x 20 seeds against the pre-change engine (full `params` + 28 result channels + seed payload, plus the draw-coverage counter), with all 420 assertions in the Tier B oracle-replay suite and all 143 in the results contract passing untouched.

Measured on the default 40-location, 1,398-tick config by an **interleaved paired benchmark** (arms alternate within each block, so drift in machine load cancels rather than being attributed to one arm — see lesson 17):

| | min per run | per-block speedup |
|---|---|---|
| v0.72.0 (007c779) | 1.012 s | — |
| v0.73.0 | **0.452 s** | **2.25x** (range 1.88-2.33 over 6 blocks; 2.04-2.29 over a separate 8) |
| pre-optimization baseline (1956037) | 1.140 s | **2.41x cumulative** (range 2.01-2.77) |

### The mechanism

Each channel was a list of `nticks + 1` per-tick vectors, and those lists lived on the state environment. So `state$S[[i]] <- v` is a **subassignment into an environment-held list**: R's `*tmp*` fetch raises the list's reference count, and `[[<-` therefore duplicates the whole 1,399-element pointer vector before storing one element. `tracemem()` reports a copy on every write.

Three independent measurements agree:

- **Per-write cost is linear in `nticks`** — 1.5 / 3.3 / 5.9 / 11.6 microseconds at 200 / 700 / 1,399 / 2,800 rows. A write that copies nothing would be flat. This is why the cost was invisible at fixture scale and worst in production.
- **Padding the channel lists to 4x their length, without touching the dynamics**, moved a 1.02 s run to 2.08 s. The extra rows are never read or written, so the difference is pure copy cost: **0.354 s per 1,399 rows**, about 35% of the run.
- **Per-write cost is 6.15 microseconds against 0.20** for the replacement.

The engine performs roughly 60 state writes per tick over 1,398 ticks, so this was ~615 MB of garbage per run for ~0.5 MB of useful stores.

### What replaced it

State is now **one environment per tick** (`state$rows[[row]]$S`), holding every channel for that tick. Each phase binds the one or two rows it needs once — `rh` for `here`, `rn` for `nxt` — and then addresses channels by name; the lagged reads spell out `state$rows[[probe]]$Isym`. An environment binding is a pointer store with no copy, and there is no longer any long vector to duplicate: `state$rows` is written once at allocation and never again.

Re-running the padding diagnostic on the new representation confirms the mechanism is gone: the `nticks` slope falls from **0.354 s to 0.025 s** per extra 1,399 rows, a 14x reduction in how much the engine cares how long the run is.

The 2.25x exceeds what the padding diagnostic predicted (1.53x), because that diagnostic measures only the length-proportional part of each copy. It misses the fixed per-subassignment overhead and the collector's share of the churn.

### Costs, and what did not improve

- **Peak RSS rose 5%**, from 945 MB to 992 MB per process over 5 sequential runs (`VmHWM`). 1,399 hashed environments of 23 bindings cost more than 23 lists of 1,399 pointers. CLAUDE.md's ~0.9 GB/worker planning figure still holds, but it is now ~1.0 GB and should be read as such.
- **Time in the collector fell in absolute terms and rose as a share** — 0.69 s to 0.47 s over 5 runs, but 13.3% to 18.1% of a much smaller wall. Allocation is still where the remaining engine time goes.
- Allocation at startup rose ~2 ms (1,399 environments instead of 23 lists) and results assembly ~9 ms. Both are once per run against ~560 ms saved.

### `.sim_gather()` is `rbind`, not `vapply`, on purpose

The obvious way to assemble a whole-series `[rows, npatches]` array from row environments is `vapply(rows, ..., state$.proto[[nm]])`, which is faster and self-documenting about storage mode. It is also **wrong here**, in a way the 100-cell bit-identity harness cannot see, because that harness only exercises `rng` mode. `vapply` enforces its prototype's type; `do.call(rbind, ...)` promotes. In `replay` mode the draws come back from a recorded fixture rather than from `rbinom()`, so an integer-allocated channel legitimately holds doubles — and eight Tier B replay tests fail on the `vapply` form. Whether replay ought to preserve storage mode is a real question, but it is a separate one from making the engine faster, so the promotion behaviour is reproduced rather than tightened. `rbind` also gets the `npatches == 1` orientation right for free, where transposing a `vapply` result would have silently returned a single-patch series with time in the columns.

### Correction to the migration record

`migrate-laser-r.md` concluded, after the matrix-to-list fix: *"After that the profile is flat — the hottest single line is 6.9% — so there is no second structural win of that size."* There was: this one, worth more than the fix that prompted that sentence.

The same document already contained the mechanism. It records that holding the channels in an environment *"changed nothing: `env$M[i, ] <- v` still copies, because fetching `M` bumps its reference count before the subassignment."* That is exactly right, and it was then reintroduced one level down — the fix moved the matrices into lists but left the lists on the environment, so the reference-count bump that had just been diagnosed still fired on every write. The diagnosis was correct and was not carried through to the structure that replaced it.

## Test suite: a leaked worker that made a finished run look hung, and a skip condition that could not fire

Two pre-existing defects in the parallel test infrastructure, found while running the suite for the engine work above.

**A wedged PSOCK worker outlived its cluster and held the suite's stdout.** `stopCluster()` asks each worker to shut down by writing to its socket; a worker that is not *reading* that socket never gets the message. `test-ensemble_cluster_robust.R` has a task that calls `Sys.sleep(600)` deliberately -- that is the point of the test, which asserts the gather stops rather than hanging -- so `stopCluster()` returned cleanly while the worker slept on. The orphan inherited the test process's stdout, so the pipe never reached EOF and a suite that had **already finished** looked like it was hanging: `devtools::test() | tail` produced nothing while the R master was already gone from the process table. New `tests/testthat/helper-cluster.R` records worker PIDs at cluster creation and kills any that outlive the shutdown request, wired into all five cluster-creating tests in the two robustness files.

The first version of that helper was a no-op, and every test still passed. It guarded the kill with `grepl("RSOCK", readLines("/proc/<pid>/cmdline"))`, and `/proc/<pid>/cmdline` is NUL-separated: `readLines()` truncates at the first NUL and returns `/usr/lib/R/bin/exec/R` with none of the arguments, so the guard could never match. `tests/testthat/test-cluster_teardown.R` therefore asserts the **leak** first -- that plain `stopCluster()` does leave the worker running -- and only then that the helper reaps it, because without the negative case a helper that kills nothing passes the positive one. It also asserts the PID-reuse guard does not fire on the test runner's own PID.

**`test-optimize_ensemble_subset.R` errored under `devtools::test()`.** Its PSOCK test guards itself with

    skip_if_not(is.function(get(".optimize_eval_cell_block", envir = asNamespace("MOSAIC"))))

whose comment says "skip if the installed package predates this refactor (e.g. running via load_all against a stale install)". Under `devtools::load_all()`, `asNamespace("MOSAIC")` *is* the load_all namespace in the master process, so the symbol is always found, the skip never fires, and the test then died on the worker's `library(MOSAIC)` with "there is no package called 'MOSAIC'". It is a skip condition that cannot detect the thing it names -- the same shape as lesson 13 -- and it asked the master a question only a worker can answer. It now asks a worker via `clusterEvalQ()`. Verified to skip cleanly under `load_all` and to run and pass (88 assertions) against a real 0.73.0 install with `NOT_CRAN=true`.

Note that this test still does not run under a bare `R CMD check`, where `skip_on_cran()` skips it regardless; it needs `NOT_CRAN=true` *and* an install. That is a real coverage gap, but closing it by `load_all`-ing on the workers would stop testing the production path, which is `library(MOSAIC)` (`make_mosaic_cluster()`).

### Also

- `sim_alloc_state()` no longer gives the final row environment the two dose channels. They are `nticks`-shaped in the Python engine and the phases only ever write them at `here` (1..nticks), so a stray read of row `nticks + 1` now returns `NULL` rather than a plausible-looking zero.
- New `tests/testthat/test-sim_alloc_state.R` (70 assertions) pins the shapes, the per-channel storage modes, the dose contract, the `npatches == 1` orientation, and that the zero prototypes shared across rows are never mutated in place.

# MOSAIC 0.72.0

## Engine and worker runtime: 1.15x on the engine, and the per-simulation `gc()` is gone

The pure-R engine ran about 1.69x slower per simulation than the retired Python engine, a regression accepted knowingly at the port (v0.68.0) on memory and startup grounds and never investigated. This release investigates it. Every change here is **bit-identical**: verified across 5 configurations x 20 seeds against the pre-change engine, comparing the full `params` + 28 result channels + seed payload and the draw-coverage counter, with the Tier B replay fixtures and the results contract passing untouched.

Measured on the default 40-location, 1,398-tick config by an **interleaved paired benchmark** — 8 blocks alternating between this version and a worktree of the previous commit, 4 runs each, so slow drift in machine load cancels instead of being attributed to whichever version happened to run during it:

| | median | min | per-block speedup |
|---|---|---|---|
| before (v0.71.1) | 1.144 s | 1.060 s | — |
| after | **0.994 s** | **0.909 s** | **1.156x** (range 1.07-1.23 over 8 blocks) |

Interleaving is not a formality here. Sequential measurements of the same two versions returned speedups from 1.28x to 1.45x and, on one run, claimed the engine was *faster* with an assertion enabled than disabled. Any unpaired A/B on this class of machine drifts by more than the effect being measured, which is the same defect that made the A-3a scaling curve worth re-running. **Per-change attribution below is therefore reported as indicative only:** the individual figures come from sequential ablation and are inflated by the same drift, in the direction that favours whichever arm ran later. Only the 1.156x total is paired.

Per-simulation worker time falls considerably further than 1.156x, because `n_iterations` defaults to 3 and the `gc()` removal below is per simulation rather than per run.

### Where the time actually was

The premise that motivated this work was wrong in an instructive way. The patch-scaling curve implies ~69% of runtime is fixed per-tick cost and ~31% scales with patch count, and that split holds up on a re-measured, nested-subset curve (intercept 0.758 s, slope 0.0088 s/patch, five points, linear-fit R^2 0.989). But the patch-scaling term was attributed to random variate generation, at an estimated ~325 ns per variate. Measured directly, R's samplers cost **54.5 ns** per variate and **67 ms** per run in total — about 5% of runtime, not 31%. A C harness calling `Rf_rbinom` with one `GetRNGstate()` for the whole loop puts the variate-only floor at **40 ms**.

So variate generation was never the cost. Profiling by function rather than by line put 53% of self time in the phase bodies themselves and 11% in `<GC>`, and the three changes below came out of that.

### 1. `sim_check_invariants()` was the largest single cost, and now costs ~1.7%

The engine's only oracle-independent correctness check — compartments non-negative and NA-free, `N` equal to their sum, `Lambda`/`Psi`/`W` finite and non-negative — runs on every tick of every run (`config$check_invariants` defaults to `TRUE`). It was **20.6%** of engine runtime.

Almost none of that was the checking. It was allocation churn: `Reduce(`+`, lapply(compartments, ...))` built a list of nine vectors plus eight intermediate sums per tick; `intersect(c("Lambda","Psi","W"), names(state))` rebuilt and matched against the whole state environment's name vector per tick to rediscover three names `sim_alloc_state()` always creates; and `anyNA(v)` + `any(v < 0L)` walked each compartment twice, allocating a logical vector each time.

The rewrite keeps every assertion and every error message: one `min()` pass per compartment (an NA anywhere makes `min()` NA, so both checks fall out of one traversal that allocates nothing), an accumulation loop for the compartment sum, `min()`/`max()` for the finiteness checks, and a `NULL` skip for absent channels. `which()` is computed only on failure. The residual cost is now within measurement noise, so **the check stays on by default** — there was no speed-versus-safety trade to make.

It had no direct tests, which is why a rewrite could have silently turned any of these assertions into a no-op with the whole suite still green. `tests/testthat/test-sim_check_invariants.R` now asserts each one *fires*, plus that the engine still calls it and that the default is `TRUE`.

### 2. The draw wrapper

Every stochastic draw passed through four layers. In `"rng"` mode — every production run — the replay machinery is dead weight, and it cost 3.2 microseconds per call against a 2.2 microsecond `rbinom()`: a closure allocated and discarded on each of ~30,750 draws, a `rep()` materialising a length-40 `p` that `rbinom()` recycles for free, a `stats::` namespace resolution per call, and two extra frames.

`sim_draws()` now branches on mode once (`ctl$fast`) and `.sim_binom`/`.sim_pois` carry a thin production path. Parity is exact because recycling happens inside the sampler, so a scalar `p` yields the same variates in the same order as a materialised one.

Coverage counting is **kept** — it is part of the engine's return contract and three tests read `attr(out, "sim_coverage")` off an ordinary run — but the counter moved from a named integer vector to a hashed environment. `coverage[site] <- n` on a named vector copies the whole vector and its name attribute on every draw; that alone was ~9% of runtime. The "unknown draw site" error still fires on the fast path.

`.sim_at()` is now a no-op in `"rng"` mode. It stamps the tick and phase so a *replay* mismatch can say where it happened; nothing in production reads those fields, and two environment writes x 10 phases x 1,398 ticks was 8% of runtime. **Consequence for anyone instrumenting the engine:** `ctl$tick` and `ctl$phase` stay `NA` in `"rng"` mode. Force `mode = "replay"` if you need them.

### 3. No per-simulation `gc()` in the calibration worker

`.mosaic_run_simulation_worker()` called `gc(verbose = FALSE)` twice per simulation — once at the end, once inside the iteration loop at `j == n_iterations`, the latter still commented as preventing "Python object buildup". The Python full GC went with the Python engine in v0.68.0; there is no reticulate finalizer queue or NumPy heap left to sweep, and the rationale left at the same time the code did not.

A forced full collection on a warm worker heap measured **292 ms**, so the pair was **14.8% of the entire per-simulation worker budget** — against 2.1% for `calc_model_likelihood()` and 0.1% for the parquet write. It also defeats R's generational collector. Both calls are removed.

Peak worker RSS does rise, but not enough to matter: 913 MB with the `gc()` against **957 MB** without it, over 30 simulations of the default config, read from the kernel's `VmHWM`. That is 44 MB on a worker the A-3a gate already sized at 926 MB, so the worker-count budget in CLAUDE.md is unaffected.

### What was measured and left alone

- **Parameter matrix orientation.** `sim_params()` transposes the `_jt` matrices out of the patch-major orientation the config delivers and into one that needs a strided read on each of 15,378 row extracts per run. Real, and self-inflicted — but measured at 0.6 ms/run for a contiguous read and 5.2 ms/run for per-tick vector lists, i.e. 0.05% to 0.4%. Not worth the blast radius of flipping 11 call sites and every `_jt` consumer. Left as is.
- **Worker time outside the engine.** The engine is **83%** of the per-simulation worker budget (3.285 s of 3.956 s at `n_iterations = 3`). Likelihood is 2.1%, the parquet write 0.1%. Sharding the one-row-per-simulation parquet files would not measurably help the worker; its real cost is on the load-and-combine side.

## Config reading: `read_json_to_list()` reads the path, and config paths are cached

`config_default.json` is **5.76 MB**, and 10 of its 79 fields are dense 40x1398 numeric matrices (`b_jt`, `d_jt`, `psi_jt`, `mu_jt`, `nu_1_jt`, `nu_2_jt`, `reported_cases`, `reported_deaths`, and the two weight matrices) -- about 390,000 doubles serialized as decimal text.

`read_json_to_list()` was doing `readLines()` -> `paste(collapse = "\n")` -> `fromJSON(string)`, materialising a 5.76 MB intermediate string on top of the line vector. Handing the path straight to `fromJSON()` gives the identical result (verified by test) in **0.167 s -> 0.125 s**.

The larger cost was re-parsing. `run_simulation(config = "path.json")` is a documented input, and the parse landed inside `sim_params()`, so a loop over a config path re-read 5.76 MB on **every simulation** -- 0.167 s against a 0.94 s simulation, an 18% tax with nothing to indicate it. The same shape appeared in `run_fit_sandbox()` (which the `diagnose-fit` workflow drives repeatedly) and in the rolling-CV per-window config reader.

Those three call sites now go through an internal reader cached on path + size + mtime, so a warm repeat read is **~0 s**. All three previously passed `simplifyVector`/`simplifyMatrix` arguments that are the `fromJSON` defaults, which is what makes one shared reader safe; the test suite asserts that equivalence rather than assuming it.

**Calibration is unaffected either way** -- `run_MOSAIC()`'s worker hands `run_simulation()` an in-memory list and never re-reads a file.

The exported `read_json_to_list()` is deliberately **not** cached: callers of an exported reader should get the file as it is on disk now. Invalidation keys on mtime as well as size, so rewriting a config with different content of the same byte length is still picked up -- there is a regression test for exactly that case.

Not changed: JSON remains the canonical config format. An RDS sidecar would read in 0.005 s at 0.33 MB (33x faster, 17x smaller, bit-identical round trip, no new dependency), but `1_inputs/config.json` and `best_model/config_medoid.json` are documented, human-inspectable interchange artifacts and that is worth more than 0.16 s paid once per run.

## `make_mosaic_cluster()` is capped by available connections

`n_cores` defaulted to `parallel::detectCores() - 1L` with no cap. Every PSOCK worker holds one R connection and a default R build permits 128 in total, three already taken by stdin/stdout/stderr, so on any host with more than ~126 usable cores `parallel::makeCluster()` failed outright. This is not hypothetical: **dugong has 176 cores.**

`n_cores` is now clamped to `parallelly::freeConnections() - 2` (two held back for worker parquet I/O) with a message naming the clamp and R 4.4.0's `--max-connections=N`. New dependency: `parallelly` (Imports).

# MOSAIC 0.71.1

## Bug fix: `weighted_quantiles()` biased every weighted quantile downward

`weighted_quantiles()` and `weighted_quantiles_presorted()` interpolated against each observation's **upper** weight-block edge, `cumsum(w)/sum(w)`, instead of its **midpoint**, `(cumsum(w) - w/2)/sum(w)`. This credits each observation with the whole of its own weight before interpolating to it, so every quantile was pulled toward lower values. The bias is negligible when weights are equal and spread thin, and grows with weight concentration — which is precisely the BFRS posterior regime these functions are used in.

Two cases pin the defect. The function's own documented example, `x = 1:5` with `w = c(.1, .2, .4, .2, .1)`, is symmetric about 3 and returned **2.5**. With `x = c(1, 2)` and 99% of the weight on `x = 2`, it returned **1.49** rather than approaching 2. Equal weights did not reproduce `stats::quantile()`, and splitting one observation's weight across two copies of the same value changed the answer — an invariance any weighted quantile must satisfy.

With the fix, equal weights reduce exactly to the standard Hazen (type-5) quantile.

### What this changes

Every quantile-derived output moves **upward**. Affected paths are `calc_model_ensemble()` (weighted-median central estimate, CI bounds), `optimize_ensemble_subset()`, `calc_Reff()`, `calc_model_posterior_quantiles()` and `add_reproductive_numbers()`. Weighted *means* are untouched.

Size depends entirely on how concentrated the posterior is. On a realistic calibration posterior (487 parameter sets, ESS 65) the ensemble median shifted in 13.7% of cells and totals rose 0.38%. On the small, heavily-concentrated `parity_tier2` test fixture the medians moved 2–63%, and for the `mae` objective the *selected subset* changed (`optimal_n` 7 → 12) because `optimize_ensemble_subset()` scores candidates using these medians. Anyone comparing new ensemble output against runs produced before this release should expect a small upward shift in the central estimate and CI bounds.

### Tie handling

Weights spanning many orders of magnitude (the Gibbs weight floor is 1e-15) make consecutive plotting positions collide in double precision. These are now collapsed explicitly by their **weighted** mean, rather than left to `approx()`'s unweighted tie averaging, which also emitted one warning per call — 46,672 in a single ensemble reduce.

### Verification

The golden fixture `tests/testthat/fixtures/parity_tier2.rds` was re-baselined, inputs asserted byte-identical, by `claude/parity/rebaseline_parity_tier2.R`. Three independent checks distinguish a re-baseline from a covered-up break: the fixture-free oracles in `test-tier2_parity.R` (#2a, #1) pass untouched at `tolerance = 0`; weighted means came back bit-identical at ~4e-16, and the re-baseline script aborts if one moves; and every changed cell moved up with none moving down (`cases_median` 8 up / 0 down, `ci_bounds` 47 up / 0 down), the only direction correcting a downward bias can produce.

Found while running the phase A-5 calibration acceptance; it affected neither arm's comparison, since both were reduced by the same function.

# MOSAIC 0.71.0

## The R engine is accepted

Phase A-5 of `migrate-laser-r.md` is complete: the pure-R transmission engine has passed the acceptance criteria prespecified before the port began. No package code changed in this release — this version marks the acceptance itself, which is A-5's stated exit.

### Tier C — free-running distributional parity

Four configurations (default 40-location, single-location, high-vaccination, epidemic-threshold-crossing), 200 seeds per arm, each config's noise floor measured from a Python-vs-Python replicate pair before the R arm was compared to it. **All 36 configuration × quantity bands pass.**

The three configurations beyond the default were built for this release and each was verified to reach the code path it targets before engine time was spent on it. That check earned its keep: the default configuration's `nu_2_jt` is all zeros and only 17 of 40 patches receive any `nu_1_jt`, so the entire second-dose block had never executed under Tier C until the high-vaccination configuration turned it on.

### Calibration acceptance

500 parameter draws from `sample_parameters()` pushed through both engines from the same config files and scored by the same R `calc_model_likelihood()`, with a Python replicate over the same draws setting the noise floor. **All 32 bands pass** — R², bias ratio, ESS (Kish and perplexity), mean log-likelihood, and the weighted posterior marginals of all 25 sampled scalar parameters.

### One behavioural difference, quantified

NumPy's Poisson sampler raises `ValueError('lam value too large')` above λ ≈ 9.2e18 because it returns `int64`; R's `rpois()` returns a double and samples correctly there. Consequently **13 of 500 prior draws (2.6%) run under the R engine and are rejected by the Python engine** — the same 13 under both Python replicates despite a 100,000 seed offset, so the rejection is deterministic in the draw. All 13 carry an extreme `zeta_2` and overflow at the environmental shedding draw. Their R results are ordinary (no non-finite values; 0.5–6.4M cases against 2–3.8M for the best-fitting ordinary draws), they would carry 3.2% of posterior mass, and one ranks 9th best of 500.

The practical consequence is that the R engine explores a thin band of prior tail that the Python engine silently discarded. R is the correct arm; no change was made.

# MOSAIC 0.70.0

## `LASER` is gone from the names too

The engine has been pure R since v0.68.0 and the `laser-cholera` dependency went in v0.69.0. `LASER` named the Python package MOSAIC used to shell out to, so every `LASER` in the API was pointing at something that no longer exists. This release renames them and clears out what the migration left behind.

### Breaking changes

* **`run_LASER()` is now `run_simulation()`**, **`make_LASER_config()` is now `make_simulation_config()`**, and **`get_default_LASER_config()` is gone** in favour of the identical `get_default_config()` (the two were byte-for-byte duplicates and neither had a caller). The old names are kept as stubs that raise an error naming the new one -- not as silent aliases, which is how a dead name survives for years. Arguments and behaviour are unchanged. The lowercase alias `run_laser()` is likewise a stub.
* **The engine's internals are `sim_*`, not `laser_*`.** `laser_params()` -> `sim_params()`, `laser_results()` -> `sim_results()`, `LASER_CHANNELS` -> `SIM_CHANNELS`, `LASER_PIPELINE` -> `SIM_PIPELINE`, and so on for every engine symbol; the files follow (`R/laser_engine.R` -> `R/sim_engine.R`). The two attributes on a `run_simulation()` return are now `sim_provenance` and `sim_coverage`.
* **`check_coiled_workspace()` and `mosaic_dask_presets()` are deleted.** v0.67.0 replaced them with `stop()` stubs "for one minor version after the engine cutover"; the cutover was v0.68.0, so this is when that expires.
* **The `Running LASER` vignette is now `Running simulations`** (`vignettes/Running-simulations.Rmd`).

### Removed

* **The Docker worker image and its CI.** `.github/workflows/docker-image-update.yaml` built and published `mosaic-worker:latest` and refreshed the Coiled software environment; `.github/workflows/smoke-test.yml` pulled that image on every push. Both existed to serve the Dask/Coiled backend, which went in v0.67.0. The `azure/` tree (the Dask/Coiled scripts, Dockerfile and runbooks) goes with them. The ACR image and the Coiled environment themselves are external and still need deleting by hand.
* **Dead local helpers,** each defined and never called: `draw_loc_or_default()` in `est_initial_E_I()`, `.get_ci()` in `plot_model_ppc()`, `.lookup_prior_family()` in `calc_model_posterior_quantiles()`, `get_column_names()` in `get_WHO_vaccine_data()`, `log_sum_exp()` in `calc_model_ess_parameter()`, and a 62-line `calc_kl_analytical()` in `plot_model_distributions()` that duplicated the exported `calc_kl_divergence()`.
* `.Rbuildignore` entries for `deprecated/` and `src/`, neither of which exists.

### Changed

* **CI no longer installs a conda environment on the PR path.** The Miniforge setup plus a ~2-3 GB TensorFlow solve ran on every push to check an environment that only the suitability model uses; it now runs on the nightly schedule and on manual dispatch, where "does `environment.yml` still solve" is the actual question. The `Install MOSAIC from GitHub` step is gone -- it reinstalled the *default branch* over the tarball just built from the PR, after the tests had already run. The macOS Homebrew Python step is gone too: macOS never installed the r-mosaic environment, so it only ever handed reticulate an interpreter with none of MOSAIC's Python packages in it.
* `install_dependencies()` no longer claims to install "the LASER disease transmission model simulation tool"; its documentation now says what the environment is actually for.
* The startup banner no longer advertises LASER.

# MOSAIC 0.69.0

## The `laser-cholera` dependency is gone

v0.68.0 made the R engine the only engine. This release removes the Python
package it replaced. Nothing on the simulation or calibration path touches
Python any more; `reticulate` survives solely for the keras3 environmental-
suitability model, which is unchanged.

### Breaking changes

* **Resuming a run directory created before v0.68.0 is now a hard error.** Its
  shards came from the Python engine, and the two engines agree statistically
  but not draw-for-draw, so pooling them would produce a posterior from neither
  simulator. The check reads the MOSAIC version recorded in
  `1_inputs/environment.json`. Run directories created by v0.68.0 or later
  resume exactly as before.
* **`1_inputs/environment.json` no longer records `python$pkg_laser_cholera`**
  (nor `pkg_laser_core`), and now records `pkg_tensorflow` and `pkg_keras`
  instead. Readers of the old key get `NULL`; the resume path no longer reads it.
* **`inst/python/environment.yml` loses `laser-cholera`, `laser-core`, `numba`,
  `llvmlite` and `pyarrow`,** keeping `python`, `pip`, `numpy`, `packaging` and
  the pinned `tensorflow`. A fresh `install_dependencies()` builds a
  TensorFlow-only environment. Nothing in the package imports the removed
  packages.

### Changed

* **`check_dependencies()` validates a TensorFlow environment, not a LASER
  one.** Its "core" capability category is gone along with the packages that
  populated it -- there is no longer a Python capability whose loss breaks
  simulation -- so it reports one capability, suitability estimation, and says
  plainly that a broken Python environment costs you `est_suitability()` and
  not `run_MOSAIC()`.
* **`lock_python_env()` verifies the environment by importing `tensorflow`**
  rather than `laser.cholera.metapop.model`.
* The `psi_manifest.json` written by `prefit_rolling_cv_psi()` no longer
  carries a `laser_version` field. It was write-only provenance -- cache hits
  key on `spec_hash` -- and psi is upstream of the transmission engine, so the
  engine version never bore on whether a frozen psi CSV was reusable.

### Removed

* `.onLoad()` no longer sets `NUMBA_THREADING_LAYER=workqueue`. That workaround
  stopped numba loading Intel's OpenMP runtime alongside data.table's; numba
  came in with the engine and is no longer installed, so the setting named a
  package that is not there. The `KMP_*` and `OMP_NUM_THREADS` settings stay --
  TensorFlow can still bring its own OpenMP runtime.
* `.mosaic_lc_pre013()` and `.mosaic_lc_deaths_scale()`, which classified two
  laser-cholera versions against the v0.13 deaths-likelihood-scale boundary.
  Their "current" operand was read from the installed wheel, which after the
  v0.68.0 cutover no longer described what had simulated anything -- and once
  the wheel left `environment.yml` the guard would have reported itself
  SKIPPED on every single resume. Replaced by `.mosaic_run_engine()`, which
  answers the larger question the boundary was a proxy for: which engine
  produced these shards.
* `.mosaic_likelihood_provenance()`'s `lc_version` argument. v0.67.0 had
  already reduced the body to a constant, leaving a parameter every caller
  filled and nothing read.
* `skip_if_no_python_likelihood()` and the eager Python probe in
  `tests/testthat/setup-python.R` that fed it. The helper had no callers left
  once the R-vs-Python likelihood parity tests went, but the probe still paid a
  reticulate interpreter init plus two module imports (~6 s) in every test
  process to cache three flags nothing read. The CI step that installed the
  wheel so those tests would not skip is gone with them.

# MOSAIC 0.68.0

## The R engine is now the engine

`run_LASER()` runs the pure-R transmission model. It was a `reticulate` bridge
to `laser.cholera.metapop.model`; it is now the R engine itself, and for the
first time it is the package's **only** engine entry point. It was not one
before: `run_MOSAIC()`'s simulation worker, `calc_model_ensemble()`'s per-task
worker and `calc_Reff()`'s re-simulation each imported the Python module and
called `run_model()` directly, so "the engine call site" was four places that
had to be kept in step. All four now go through `run_LASER()`.

### Breaking changes

* **`run_LASER()` returns an R list, not a Python object.** The shape is
  unchanged -- `$params`, `$results`, `$seed`, with `$results` holding
  `[location, time]` matrices -- so `model$results$reported_cases` still works,
  but `reticulate::py_to_r()` around it does not and is no longer needed. The
  28 channels, their orientation, their per-field storage mode and the absence
  of dimnames are asserted by `test-laser_results_contract.R`.
* **Single-location runs return a `1 x nticks` matrix**, where the Python
  engine returned a bare length-`nticks` vector. Callers that already handled
  both are unaffected.
* **`visualize`, `pdf`, `outdir` and `py_module` are gone** from `run_LASER()`.
  They drove the Python engine's matplotlib Analyzer or passed in a
  pre-imported module; supplying one now raises an error naming it.
* `make_mosaic_cluster()` no longer imports `laser.cholera` into each worker,
  and no longer loads `reticulate` there. This was scheduled for the dependency
  removal, but keeping it would have meant every calibration worker still paid
  the 3.3 s import and held the Python heap for a module nothing calls. For the
  same reason the calibration worker's every-100th-sim `reticulate::import("gc")$collect()`
  is gone: there is no Python heap left to sweep, and the call would have
  initialised Python in each worker to collect nothing.

### Fixed

* **`run_fit_sandbox()` was broken and no test could see it.** It called its
  runner with `visualize`/`pdf`/`outdir`, which `run_LASER()` had already
  started rejecting -- but every test in the file stubs the runner, and the
  stubs accepted those arguments. The call is fixed, and a new test asserts the
  sandbox only ever passes arguments that are formals of the real `run_LASER()`.
* **`test-lasik_calculations.R` now runs, and three of its assertions were
  wrong.** The file validates engine output against this package's analytic
  helpers, and it had been silently inert: gated to the slow tier, and even
  there its config path was cwd-relative and never resolved. With the R engine
  the whole file takes ~2 s, so it is un-gated. Running it surfaced that (a)
  the `pi_ij` comparison applied a `t()` that made it wrong by up to 0.29,
  where the untransposed comparison agrees to 4e-16; (b) the spatial-hazard
  check read `V1sus`/`V2sus`, compartments the engine collapsed into `V1`/`V2`
  in v0.16.1, i.e. it passed `NULL`; (c) the population check's 1% tolerance
  was never achievable -- the measured drift against UN WPP is 2.23% for the R
  engine and 2.24% for the pinned Python oracle, so it is engine demography
  rather than a port artefact, and the tolerance now says so.
* The **coupling** comparison in the same file no longer needs its `1e-2`
  fudge. The engine correlates the untrimmed prevalence series (`nticks + 1`
  observations) while the result channels have the seed row trimmed;
  reconstructing that observation from `I_j_initial`/`N_j_initial` makes
  `calc_spatial_correlation_matrix()` reproduce the engine's matrix *exactly*,
  which proves the trim was the entire difference rather than assuming it.
* **`expected_cases` no longer exists** in the engine's return and has been
  removed from `calc_model_ensemble()`'s default trajectory channels and from
  `plot_model_trajectories()`'s panel spec. It was degrading silently to an
  absent panel.

### Removed

* `.mosaic_prepare_config_for_python()` and `.mosaic_strip_laser_file_handler()`.
  The first wrapped length-1 config fields so `reticulate` would pass them as
  Python lists; the second deleted the log file `laser-cholera` created on
  import. Neither has anything left to do.

The `laser-cholera` dependency itself is still declared -- `check_dependencies()`,
`lock_python_env()`, `environment.yml`, the run-provenance keys and CI still
reference it. Removing those is the next step.

# MOSAIC 0.67.0

## Pure-R transmission engine (ported; cut over in 0.68.0)

The Python `laser-cholera` transmission engine is replaced by a pure-R
implementation. This release lands the deterministic precomputation, the full
tick loop and the result contract behind an internal entry point;
0.68.0 makes it the engine `run_MOSAIC()` actually calls. See
`migrate-laser-r.md`.

**All ten pipeline components are ported** (`Susceptible`, `Exposed`,
`Recovered`, `Infectious`, `Vaccinated`, `Census`, `HumanToHuman`,
`EnvToHuman`, `Environmental`, `DerivedValues`). Correctness is established by
replaying the Python engine's recorded PRNG draws: **all 22 stochastic draw
sites, 30,751 draws matched draw-for-draw, and all 19 integer result channels
bit-identical over a full 1,398-tick 40-patch run**, with the 9 float channels
inside a measured scale-aware tolerance. The return contract -- 28 channels,
`[patch, time]` orientation, per-field storage mode, no dimnames -- is asserted
against an ordinary (non-replayed) run.

`DerivedValues` contributes the two end-of-run diagnostics `spatial_hazard` and
`coupling`, which `calc_model_ensemble()` and the spatial plots consume. Both
are computed once, on the final tick, from the whole run. `coupling` is a
Pearson correlation matrix of per-patch prevalence and is `NaN` for any patch
whose prevalence never varied (correlation with a constant series is
undefined); the R and Python engines agree on exactly which patches those are.

Two findings worth flagging:

* **Single precision is observable.** The Python engine stores most parameters
  and the `W`/`Lambda`/`Psi` state as float32. Where that reaches an integer --
  `round(sigma * progressing)`, the vaccination pro-rata split, the reported-case
  divisor, the epidemic-threshold comparisons -- the R port reproduces the stored
  precision, because an integer differing by one decorrelates the draw sequence.
  Where it reaches only a float, the R port stays in double and is the more
  accurate of the two. Details in `tests/testthat/fixtures/ORACLE.md`.
* **`spatial_hazard` can be negative, in both engines.** The unconstrained
  two-harmonic seasonal envelope dips below zero for some patches in the low
  season, so `beta_jt_human` goes negative and the hazard follows it.
  `HumanToHuman` clamps its own rate with `pmax(..., 0)`; `derivedvalues.py`
  has no such clamp, and the R port reproduces that rather than quietly
  changing the model. The same near-zero envelope is why `spatial_hazard`
  needs a looser parity tolerance than any other float channel.
* **The engine is 1.69x slower than the Python original**, not faster as the
  migration plan projected: 1.183 s against 0.698 s for a 1,398-tick 40-patch
  run. An earlier version was 2.91 s; storing each channel as a list of per-tick
  vectors rather than a matrix removed ~55% of the runtime, because a matrix row
  write copies the whole matrix. Whether the remaining gap is an acceptable price
  for dropping reticulate, the 2 GB-per-worker Python heap and the per-worker
  import tax is a judgement call, flagged in the plan rather than assumed.

## Dask/Coiled distributed backend removed

The distributed-compute layer existed to make the Python transmission engine
affordable — that engine costs ~2 GB of RAM per worker and 3.3 s of import time
per worker process, which on a 20-core fan-out is 66 s of startup tax on every
batch. With the engine moving to pure R (see `migrate-laser-r.md`), the reason
for it goes away. It was also already scientifically invalid on Coiled: the
worker image lagged `laser-cholera`, so hybrid runs completed but produced low
R² / unconverged results (issue #113).

`run_MOSAIC()` now has exactly one execution path: the local PSOCK/sequential
cluster, sized by `control$parallel$n_cores`. This is the single largest
simplification the package has had — ~2,200 lines of production code and ~1,900
lines of tests removed, and `run_MOSAIC.R` alone dropped from 3,999 to 3,388
lines.

### Breaking changes

Removed, and **loud** about it — every one raises an error naming what to use
instead, rather than being silently accepted and ignored:

* `dask_spec` argument to `run_MOSAIC()` and `run_rolling_cv()`. Use
  `control$parallel$n_cores`.
* `check_coiled_workspace()`, `mosaic_dask_presets()`.
* `control$parallel$strict_worker_version` (guarded orchestrator/worker engine
  version skew, which cannot exist with one process).
* `precomputed_results` argument to `calc_model_ensemble()` — its only
  production callers were the Dask gather and the Dask medoid dispatch. It was
  also the seam four test files used to inject synthetic engine output, so the
  per-task simulation worker has been hoisted out of `calc_model_ensemble()`'s
  body into `.mosaic_ensemble_sim_task()` (`R/calc_model_ensemble_task.R`) and
  the tests now mock that instead. They assert strictly more than before: the
  real task list, dispatch, gather and worker-side spill-to-scratch all run,
  where the old argument bypassed them. `optimize_ensemble_subset()` and
  `calc_model_ensemble()` still reproduce `fixtures/parity_tier2.rds`
  bit-for-bit through the new seam.
* The `param_seed` field on a result record, which sat ahead of config `$seed`
  and positional `parameter_seeds` in `calc_model_ensemble()`'s per-member seed
  fallback. It existed only because a Dask worker held a config the master did
  not; the two surviving tiers are unchanged.
* `run_LASER()`'s `py_module`, `visualize`, `pdf` and `outdir` arguments. The
  first let a caller hand in a pre-imported module; the other three drove the
  Python engine's matplotlib `Analyzer`, which is not part of the R contract.
  All four had zero callers.

`run_MOSAIC()` and `run_LASER()` now also reject **unknown** arguments rather
than absorbing them into `...`. This is the "unknown key validator" whose
absence let renamed `control` parameters be silently dropped for fifteen minor
versions (see CLAUDE.md lesson #13).

`make_mosaic_cluster()` is **not** removed. Despite its Dask-era documentation it
builds the local PSOCK cluster that the surviving backend runs on.

### Also removed

* `inst/python/mosaic_dask_worker.py` (727 lines), and `dask[distributed]` /
  `coiled` from `inst/python/environment.yml`. `laser-cholera` is untouched —
  it is still the engine until the R port lands.
* `.mosaic_inject_likelihood_settings()` and `.extract_base_config()`, which
  flattened likelihood settings onto the config for on-worker Python scoring.
  This also retires the bug in CLAUDE.md lesson #12(a), where the injector
  overwrote `get_location_config()`'s filtered `epidemic_peaks` with the full
  unfiltered SSA dataset.
* A dead allow-list in `.mosaic_resume_check_inputs()` that permitted resuming
  across two `laser-cholera` versions whose on-worker Python likelihood values
  were verified byte-identical. With scoring now always R-side its
  `engine == "python"` condition can never be true. Shards scored by the old
  Python path now fail the provenance check outright, which is correct — those
  likelihoods are not reproducible here.

### Tests

Ten Dask test files were retired, but three carried assertions about the
surviving code and were re-homed rather than deleted:

* `test-samples_parquet_schema.R` keeps the **ISO-suffix parquet column
  contract** (`beta_j0_tot_ETH`, never `beta_j0_tot_1`) that every downstream
  posterior join depends on.
* `test-calc_model_likelihood_regression.R` replaces the R-vs-Python likelihood
  parity suite with **frozen R baselines** plus monotonicity and orientation
  properties, guarding the shape-term scaling bugs of lessons #4 and #5.
* `test-presets.R` keeps `mosaic_io_presets()`.

New `test-removed_dask_api.R` asserts the removed surface errors rather than
being silently absorbed.

# MOSAIC 0.59.1

## plot_Reff readability

* The medoid `central` is a daily series and is very noisy (the instantaneous
  Cori R_t on one trajectory swings sharply day-to-day); at full line weight it
  rendered as a solid mass that buried the band. `plot_Reff()` now draws a
  centered rolling-mean trend (`smooth_days`, default 14) as a thin headline line
  with the raw daily series kept faint behind it, and the cross-member envelope
  ribbon at higher alpha so it is visible. The per-member daily peak is unchanged
  (still reported in the annotation). `smooth_days = 1` restores the raw line.

# MOSAIC 0.59.0

## Cori R_eff: phase-coherent headline + per-member peak (underestimation fix)

Investigation of "R_t sits near 1 everywhere" found the headline `central` was a
per-calendar-day cross-member weighted MEDIAN of per-member R_t. Because members'
R_t peaks are phase-misaligned (peak-day SD ~hundreds of days), that statistic
regresses to ~1 even when individual trajectories are explosive — it was an
aggregation artifact, not the biology and not a renewal-math bug (Euler-Lotka
cross-checks pass; posterior weighting is near-uniform so not a factor).

* **`central` is now the MEDOID trajectory's R_t** — a single coherent member,
  selected with run_MOSAIC's exact medoid criterion (per-channel `central_method`,
  default median) computed on the saved `cases_array`. Phase-coherent, so it shows
  real peaks (e.g. MOZ 1.46 -> 4.58, COD 1.50 -> 2.23).
* **New `peak_Rt` attribute** — per-location posterior-weighted q2.5/q50/q97.5 of
  each member's post-burn-in time-MAX R_t (burn-in masked before the max so the IC
  seeding transient cannot dominate). The explosivity statistic.
* The per-calendar-day cross-member quantiles (`q2.5/q50/q97.5`) are retained as a
  calendar-date *envelope* (no longer the headline) with attr
  `band_definition = "per_calendar_day_cross_member_weighted_quantiles"`; attr
  `central_definition = "medoid_trajectory"`; `medoid_member` records the selection.
* **`plot_Reff()`** now draws the purple medoid line, a faint envelope captioned
  to explain it is NOT the epidemic peak (phase-misaligned), and a per-member peak
  R_t annotation.
* `add_reproductive_numbers()` / `.mosaic_reff_resim_ci()` gain a `burn_in_days`
  arg (from `control$likelihood$burn_in_days`) and honor
  `control$predictions$central_method` for the medoid target (closes a lockstep
  gap; default median matches existing behavior).

# MOSAIC 0.58.1

## Bug fixes (suitability — Class-A psi flat-tail in the lstm_v2 path)

* **`est_suitability()` (lstm_v2 path) no longer emits a flat carry-forward psi
  tail.** `.psi_weekly_to_daily_smooth()` `zoo::na.locf`-fills the daily psi grid
  out to `pred_date_stop`, so days past a country's last covariate-supported
  weekly prediction carried the last genuine value forward as a flat constant
  (~99 days, e.g. 2026-10-29 -> 2027-02-04 for 29/40 ISOs). That flat psi tail
  flattens the environmental force of infection and produces an artificial
  end-of-series drop in downstream LASER predictions. The v0.44.14 fix
  (`.drop_filled_prediction_tail`) existed only in the legacy path; it is now
  applied in the lstm_v2 writer too. `.psi_run_seed_ensemble()` captures each
  country's last genuine weekly prediction date (`genuine_last_pred`) from the
  weekly grid **before** the daily na.locf fill, and the writer truncates daily
  and weekly rows beyond it per country. Keyed on the explicit genuine date (the
  na.locf fill is non-NA, so the NA-keyed contract of the helper alone would not
  catch it). `make_config_default()`'s common-coverage truncation propagates the
  per-country horizon into the simulation `date_stop`. Class-B floor-clamp
  (genuine constant-at-floor psi from the LSTM bias correction) is unchanged.

# MOSAIC 0.58.0

## New features (Cori R_eff — posterior credible interval)

* **`add_reproductive_numbers(..., recompute_ci = TRUE)`** computes a proper
  posterior credible interval for R_eff by faithfully re-simulating the saved
  posterior ensemble (`ensemble_candidate.rds`): each member config is rebuilt
  with the same recipe and seeds as the production ensemble worker
  (`sample_parameters()` + transmission clamp + deterministic LASER seed), the
  daily infection-`incidence` channel is captured, R_eff is computed per member
  with that member's own generation-interval kernel, and the members are reduced
  to weighted quantiles (median + 95%) via `weighted_quantiles()`. This recovers
  the CI that the time-strided trajectory `lines` cannot. A statistical-
  equivalence faithfulness gate validates the re-sim against the saved
  `cases_array` (numba RNG is not bitwise-reproducible across cold processes, so
  an exact gate is unachievable; the gate checks p95 per-member relative error,
  weighted-aggregate error, and median per-member correlation).
* **`burn_in_days`** argument (read from the run's `control.json`, overridable)
  excludes the initialization transient from the R_eff output and plot.
* **Thread safety:** the re-simulation path now pins BLAS/Numba threads to 1
  (it drives LASER outside `run_MOSAIC()`), so it is safe to run many models
  concurrently on a many-core host.
* **`plot_Reff()`** now draws the purple median + 95% ribbon only (`show_iqr`
  defaults FALSE) and excludes the burn-in period.
* **Trajectory-capture grid fix:** `.mosaic_build_trajectories()` now uses
  `seq.int(1L, n, by = line_stride)` instead of `which(seq_len(n) %% line_stride
  == 1L)`. Byte-identical at the default `line_stride = 7L`; fixes the
  previously-broken `line_stride = 1` (which produced an empty/non-daily grid).

# MOSAIC 0.57.0

## New features (Cori R_eff — Phase 3)

* **`plot_Reff()`** — visualizes the Cori effective reproductive number from a
  `reproductive_numbers` object: per-location R_eff(t) with a dashed R_eff = 1
  reference line, faceted for multiple locations and single-panel-titled for
  one. Draws the 95% (and, with `show_iqr`, the 50%) posterior ribbon only when
  the quantile columns are present; on strided-line artifacts where the CI is
  unavailable it draws just the central line and captions the limitation.
  Leading warm-up NAs are trimmed; interior floor-gated NAs break the line.
* **`add_reproductive_numbers()`** — post-hoc driver that applies R_eff to an
  existing MOSAIC output directory: reads `2_calibration/trajectories_ensemble.rds`
  + `1_inputs/config.json`, runs `calc_Reff()`, writes
  `3_results/posterior/reproductive_numbers.{csv,rds}`, and (when `plots=TRUE`)
  saves `3_results/figures/reproductive_number/*.{png,pdf}`. Every failure mode
  (missing artifacts, no `incidence` channel, write errors) returns a status row
  rather than crashing, so it is safe to map over many run directories.

# MOSAIC 0.56.1

## Bug fixes / refinements (Cori R_eff red-team remediation)

* **Mean-preserving generation-interval kernel.** `.mosaic_generation_time_pmf()`
  now discretizes via the EpiEstim `discr_si` (Cori 2013 / Cauchemez) formula
  instead of a naive CDF-difference, which had inflated the effective mean by
  ~0.5 day (+~10% for the low-shape cholera kernel) and biased R_eff upward
  during growth. Recovered discrete mean now matches the target (5.400 d). Docs
  `eq:gen-time-discr` updated to match.
* **Initial-condition warm-up gate.** `calc_Reff()` / `.cori_reff()` gained an
  `infectiousness_floor` argument (default 1 effective past infection): a step
  whose generation-weighted denominator falls below the floor returns NA. This
  suppresses the spurious R_eff spike at the start of a series (an IC seed gave
  R≈3600+ at t=2) and cleans deep inter-epidemic troughs. `infectiousness_floor = 0`
  recovers the pure Cori convention.
* **Posterior weights indexed by member id.** `.mosaic_reff_member_quantiles()`
  now looks up a supplied `weights` vector by member id, not per-location
  position, fixing silent cross-location CI misalignment when a member is dropped
  in one location. The `weights = NULL` default path is unchanged.
* **Honest CI/estimand documentation.** Removed the incorrect claim that
  re-capturing with `line_stride = 1` enables the posterior CI (the trajectory
  builder cannot currently produce a daily-consecutive series; the CI ribbon
  requires a Phase-2 builder change). Added an estimand caveat that the kernel is
  the two-clock (latent + infectious, human-route) generation interval and
  excludes the environmental-delay clock, so R_eff is a lower-mean-G approximation
  in waterborne-dominated locations.
* **Test rigor.** Added a continuous-truth Euler–Lotka regression (Gamma-MGF
  value, independent of the discretized kernel — catches kernel-mean bias the
  prior self-consistency test could not), plus internal-NA and t_min>1 CI-window
  tests. `test-reproductive_numbers.R` now 75 expectations, all passing.

# MOSAIC 0.56.0

## New features

* **Cori effective reproductive number, `calc_Reff()` (R_eff) — Phase 1.** New
  `calc_Reff(ensemble, config)` computes the time-varying Cori et al. (2013)
  instantaneous effective reproductive number per location, as an *infection*
  R_eff: `R[t] = I*[t] / Σ_Δt g(Δt) I*[t-Δt]`. The renewal series is **infection
  incidence** (the `incidence` channel = `incidence_human + incidence_env`, a
  flow), with symptomatic and asymptomatic infections counted with **equal
  weight** — not the infectious-compartment stocks and not shedding-weighted
  (see the corrected spec in `MOSAIC-docs/04-model-description.Rmd`). The
  generation-interval kernel is the moment-matched two-clock Gamma with the
  over-dispersed σ-mixture infectious-period variance, exposed as a new in-memory
  helper `.mosaic_generation_time_pmf()` (normalized so Σg = 1). Posterior
  median + credible interval via `weighted_quantiles()`, plus a medoid point
  estimate. The pure renewal core `.cori_reff(incidence, g)` is unit-tested
  against an Euler–Lotka exponential-growth fixture. `get_generation_time_distribution()`
  and its CSV outputs are unchanged. Not yet wired into `run_MOSAIC()` (Phase 2)
  and no `plot_Reff()` yet (Phase 3). See `claude/plan_r0_rt/PLAN.md`.

# MOSAIC 0.55.17

## Bug fixes

* **Dask post-cal: hoist `.traj_enabled` above the reconnect dispatch.** `run_MOSAIC()` referenced `.traj_enabled` in the post-calibration Dask reconnect dispatch (`capture_trajectories = .traj_enabled`) ~80 lines before it was defined, so the dispatch threw "object '.traj_enabled' not found" and fell back to LOCAL execution for the ensemble/stochastic sims. Silent + harmless for single-location runs (local ensemble is fast), but it HANGS a multi-location (regional) post-cal ensemble. Hoisted the `.traj_*`/`.optimize_*` flag definitions above the reconnect block so post-cal stays on the Coiled cluster. Found by the first regional (5-loc) smoke.

# MOSAIC 0.55.16

## Bug fixes

* **Dask path: `run_MOSAIC()` now nulls an empty re-injected `epidemic_peaks`** (`.mosaic_inject_likelihood_settings`). The Dask/Coiled path *overwrites* `config$epidemic_peaks` with a fresh `.filter_epidemic_peaks()` against `MOSAIC::epidemic_peaks` — so the v0.55.15 `get_location_config()` guard was insufficient (the input value is discarded). For a no-peak location the re-injected frame is 0-row, JSON-round-trips to the worker without its `iso_code` column, and crashes laser `params.py:303` (every sim erroring). This is the fix that actually resolves the BFA/CIV/GHA/LBR/NAM/ZAF Coiled failures. Regression test added (`test-epidemic_peaks_dask_inject.R`).

# MOSAIC 0.55.15

## Bug fixes

* **`get_location_config()` now nulls an empty `epidemic_peaks`.** A 0-row `epidemic_peaks` (a no-peak location, or a filter that matched nothing) JSON-round-trips to a Dask/Coiled worker without its `iso_code` column and crashes the laser engine at `params.py:303` (`.iso_code` on a column-less DataFrame → `'DataFrame' object has no attribute 'iso_code'`, every sim erroring → "No simulation results to process"). The fix drops the key when the filtered result is empty so the engine skips the `epidemic_peaks` block entirely. Surfaced by 6 no-peak countries (BFA/CIV/GHA/LBR/NAM/ZAF) failing a Coiled full-metapop batch where they had succeeded on the in-process (local) path. Regression test added.

# MOSAIC 0.55.11

## Deprecations

* **`plot_model_parameters()` is deprecated and unwired from `render_MOSAIC_figures()`.** The parameter-vs-likelihood scatter (the `"parameters"` figure group, wired in during the v0.52.0 visualization/modeling split) was a revived orphan that is redundant with the `"sensitivity"` group (`calc_model_parameter_sensitivity()` / HSIC importance) and the `"posterior"` group (prior/posterior densities per parameter), and it dominated render time — a per-facet LOESS over the full retained sample (tens of thousands of simulations across all parameters) could run for 10+ minutes on a single country. It is removed from the render pipeline and `valid_groups`; the exported function remains as a deprecation shim (`.Deprecated()`) and will be removed in a future release.

# MOSAIC 0.55.3

## Model-trajectory figures (new)

* **New `"trajectories"` figure group in `render_MOSAIC_figures()`** — a multi-panel "Model trajectories" figure per location reconstructed from a finished run directory: the comprehensive set of internal LASER channels over time (compartments S/E/Isym/Iasym/R/V1/V2/W/N, force of infection Λ/Ψ + per-time transmission rates β_jt, incidence & infection flows, burden channels) plus derived series (I_total, mass_balance, CFR(t), epidemic fraction). Each panel shows a uniform-thinned set of **actual** posterior member trajectories (the spaghetti) with the weighted central line overlaid; observed surveillance points are drawn only on the `reported_cases`/`reported_deaths` panels. Pure read-render (P5): consumes the persisted artifact only — never a LASER replay. Output to `3_results/figures/trajectories/` as `trajectories_<ISO>.pdf` + `trajectories_<ISO>_p1.png`.
* **New exported plotter `plot_model_trajectories(trajectories, location, output_dir)`** — renders one location from a `mosaic_trajectories` artifact. No LASER, no re-simulation, no re-weighting. Multi-location aware (the renderer loops one figure per location). `incidence` is correctly labelled the **S→E new-infection flow** (distinct from the `new_symptomatic` E→I progression); Λ/Ψ are labelled per-capita/day hazards; the deaths-observable overlay uses `reported_deaths` (not `disease_deaths`).
* **Capture-at-sim-time, STREAM-TO-DISK (capture-don't-replay; no second simulation).** A new `capture_trajectories` argument to `calc_model_ensemble()` (default `FALSE`; `run_MOSAIC()` defaults it **ON** for the posterior ensemble) harvests the comprehensive channels from each member where `model$results` is already in hand — at zero marginal simulation cost. Channels are **spilled to a per-sim scratch file during the run** (new args `trajectory_scratch_dir`, `reduce_trajectories`), then reduced **on the master, channel-by-channel** (one transient `[n_loc×n_time×n_param×n_stoch]` array at a time → freed before the next channel). Peak RAM is bounded to ONE dense array regardless of the channel count (the scratch *disk* footprint, not RAM, scales with `trajectory_channels`), so capture scales to 40-location runs on both laptops and large-memory VMs. Members are the **best subset** only; the full sample is never used. `epidemic_frac` reconstructs the engine's internal epidemic flag (`Isym[t-δ] > threshold·N_eff`, `N_eff` = inline people-sum) via a streaming weighted-mean pass.
* **Deviation-#1 (optimized-subset weighting), exact.** The reduction is deferred (`reduce_trajectories = FALSE`) and run by `run_MOSAIC()` over the FINAL displayed subset *after* `optimize_ensemble_subset()` — with **no re-simulation**: when `optimize_subset = TRUE` the optimized members are mapped back to their candidate scratch files by seed (with a duplicate-seed guard that falls back to the positional candidate path), and `reported_cases`/`reported_deaths` are taken from the optimized `cases_array`/`deaths_array`. The reported_* central line follows `control$predictions$central_method` **per channel** (weighted median or weighted mean), so it is **bit-identical** to the cases/deaths prediction plots under both modes; all other channels use the conventional weighted median. When `optimize_subset = FALSE`, the candidate subset (`is_best_subset`/`weight_best`) is used.
* **Comprehensive channel capture in lockstep across both backends** — the local PSOCK worker (`calc_model_ensemble.R`), the Dask Python worker (`mosaic_dask_worker.py`), and the Dask R harvest (`run_MOSAIC_helpers.R`) capture the same channel set. `capture_trajectories = FALSE` now gates the Dask Python worker itself, so it cuts the on-wire payload (not merely discarded R-side). Capture is post-calibration only (`run_laser_postca`); calibration is untouched. `trajectory_channels` is the documented RAM/disk lever; `.mosaic_ensemble_ram_projection_gb()` includes the capture term (and, on the Dask client path, the gathered-channel term) so the OOM warning fires honestly.
* **Capability check (no silent backend skew).** When `capture_trajectories = TRUE` but no worker returns the channels (e.g. a client running a MOSAIC build that predates this feature), the reduction emits a loud `warning()` and skips the artifact rather than silently producing trajectories on one backend and not the other. The Dask worker script (`mosaic_dask_worker.py`) is uploaded to the Coiled workers at runtime via `client$upload_file()`, so **no Coiled image rebuild is required** — reinstalling MOSAIC on the client is sufficient for the Dask backend to return the new channels.
* **New artifact `2_calibration/trajectories_ensemble.rds`** (`mosaic_trajectories`, schema-stamped) — per-channel weighted central series + uniform-thinned actual member lines + observed series + per-location endemic/epidemic CFR reference levels (`cfr_refs`). The medoid ensemble never captures trajectories. New directory key `res_fig_trajectories`. The transient scratch dir is removed on normal completion **and on any error/interrupt** (`on.exit`), so a failed run cannot orphan multi-GB of scratch.
* **CFR(t) panel** carries dashed endemic/epidemic regime reference lines (weighted-median `cfr_baseline`/`cfr_epidemic` from the `calc_implied_cfr()` sample columns over the best subset), with a caption noting the rolling window is `min(28, series length)` so a sparse panel on short series is not mistaken for a bug.
* **Robustness:** `.rec_mat` trims `tick+1` flow channels (recorded one time-column long) instead of silently dropping the FOI/incidence panels, and warns per-channel on an unrecoverable mismatch; display-line thinning uses `withr::with_seed()` (no global `.Random.seed` mutation); the reducer closes its per-channel connections on any exit; and the per-channel present-member count is surfaced so a compartment central computed over fewer members than its `reported_*` neighbour is visible rather than read as a model inconsistency.

# MOSAIC 0.53.0

* **New `"spatial"` figure group in `render_MOSAIC_figures()`** — six spatial-dynamics figures reconstructed from a finished run directory, in two families (mobility and transmission). Pure read-render (P5): config + persisted `.rds` arrays + a packaged basemap only — never a simulation or a GeoBoundaries / `get_country_shp()` API call. Figures: π_ij diffusion heatmap, τ_i departure (forest + N·τ daily travelers), modeled flux matrix M, mobility network, spatial importation hazard 𝓗_jt, and Keeling–Rohani coupling 𝓒_ij. Output to `3_results/figures/spatial/`.
    * **New pure helper `calc_mobility_flux(config)`** — single source of truth for the four mobility figures. Returns `{location_name, coords, N, tau, omega, gamma, D, pi, flux}` with the model-implied daily flux `M_ij = N_i·τ_i·π_ij` (diagonal `NA`), all in **config order** with axis labels aligned element-wise (no internal re-sort). Satisfies the identity `rowSums(M, na.rm=TRUE) == N·τ`. Uses only `get_distance_matrix()` + `calc_diffusion_matrix_pi()` — no disk, estimation, or API. This is the *model-implied* sibling of `plot_mobility()`'s *observed-OAG* panels (distinct quantities; not a fork).
    * **New exported plotters** `plot_diffusion_pi()`, `plot_departure_tau()` (CI bars only when supplied), `plot_mobility_flux_matrix()`, `plot_mobility_flux_network()` (centroid network over an optional `sf` basemap). All match docs aesthetics via `mosaic_colors`/`theme_mosaic`.
    * **Engine spatial arrays now extracted & persisted.** Both simulation workers previously `del`/`gc`'d the model object and returned only cases/deaths. The engine's `spatial_hazard` (J×T), `coupling` (J×J), and `pi_ij` (J×J) (filled by the engine `DerivedValues` component) are now extracted **inside each worker before discard, on both the local PSOCK path (`calc_model_ensemble.R`) and the Dask path (`mosaic_dask_worker.py`) in lockstep**, aggregated as the **element-wise median across the posterior-ensemble members**, and persisted to `2_calibration/{spatial_hazard,coupling,pi_ij}_ensemble.rds` (schema-stamped, config order, `location_name` attached). A Dask↔PSOCK serialization round-trip parity test asserts the `numpy.tolist()`→R reconstruction is bit-identical (incl. `NaN` cells). The coupling figure **masks `NaN`** (zero-variance/never-infected locations) explicitly. The renderer prefers the persisted engine `pi_ij` over an R recompute when present.
    * **τ-CI artifact (`1_inputs/mobility_tau_ci.csv`)** — `run_MOSAIC()` copies the per-location 95% CI from `MODEL_INPUT/mobility_travel_prob_params.csv` (the upstream `fit_prob_travel()` Beta posterior) into the run directory in config order; `plot_departure_tau()` draws interval bars when present, point-only otherwise. Unconditional (gated by `io`, not `plots`).
    * **Packaged basemap** `inst/extdata/africa_adm0_lowres.geojson` (~40 KB, derived from the per-country ADM0 shapefiles in `MOSAIC-data/processed/shapefiles`) provides the continental network backdrop without any API call. Subnational geographies fall back to a centroid-only network.
    * **F3 label-ordering fix.** `get_distance_matrix()` gains `sort = TRUE` (default, historical behavior preserved); `sort = FALSE` keeps input/config order so axes stay aligned to value vectors. `plot_spatial_hazard()` no longer re-sorts location labels alphabetically — it now follows `rownames(H)` (config order), preventing a silent row-mislabel when `H` is non-alphabetical. File `plot_spatial_correlation_matrix.R` renamed to `plot_spatial_correlation_heatmap.R` (exported function name unchanged).
    * **Engine-pool note (DM#3):** the rendered hazard is the engine array (single source of truth), which uses an S-only susceptible pool `S*_jt=(1-τ_j)S_jt`; the standalone R `calc_spatial_hazard()` uses a non-canonical `S+V1+V2` pool and is **not** reconciled to the engine figure.
    * Files: `R/calc_mobility_flux.R` (new), `R/plot_diffusion_pi.R` / `R/plot_departure_tau.R` / `R/plot_mobility_flux_matrix.R` / `R/plot_mobility_flux_network.R` (new), `R/plot_spatial_correlation_heatmap.R` (renamed + NaN-mask), `R/plot_spatial_hazard.R`, `R/get_distance_matrix.R`, `R/render_MOSAIC_figures.R`, `R/calc_model_ensemble.R`, `R/run_MOSAIC.R`, `R/run_MOSAIC_helpers.R`, `inst/python/mosaic_dask_worker.py`, `inst/extdata/africa_adm0_lowres.geojson` (new). Tests: `test-calc_mobility_flux.R`, `test-render_spatial_group.R`, `test-spatial_arrays_dask_psock_parity.R`.

# MOSAIC 0.52.0

* **Visualization separated from modeling in `run_MOSAIC()`** — the pipeline now produces a *complete, self-describing* `dir_output/` (every data artifact) **independent of plotting**, and all figures are rendered by a single standalone pass over the finished directory. Directly attacks the recurring "computation gated behind `plots=TRUE`" bug class (CLAUDE.md lessons #2/#9/#10).
    * **New exported `render_MOSAIC_figures(dir_output, which = NULL, plots = TRUE, verbose = TRUE)`** reconstructs every pipeline figure **from disk artifacts** (`.rds` ensembles, `samples.parquet`, posterior/diagnostic CSVs, prediction CSVs, `priors.json`). It is a **pure read-render**: it never calls `calc_model_ensemble()`, `run_LASER()`, or `sample_parameters()`; a missing / corrupt / schema-incompatible artifact is warned-and-skipped (never rebuilt, which would trigger local re-simulation). `which=` selects figure groups (`convergence` / `posterior` / `predictions` / `ppc` / `sensitivity` / `psi_star` / `parameters`); each figure is `tryCatch`-wrapped. Usable post-hoc, on another machine, or repeatedly.
    * **Data writes are now unconditional** (gated by `io`, not `plots`): prediction CSVs (`predictions_{ensemble,medoid}_*.csv`), `parameter_sensitivity.csv`, and `convergence_status.csv` are written by `run_MOSAIC()` regardless of `plots`. `run_MOSAIC(plots = FALSE)` and `run_MOSAIC(plots = TRUE)` now emit **identical data artifacts**; only the `*.png/*.pdf` figures differ.
    * **New pure helper `.mosaic_assemble_prediction_table()`** (extracted from `plot_model_ensemble()`) is the single source of truth for the masked prediction table, so the exported CSV and the plotted line are guaranteed identical. CSV schema is unchanged (`location, date, metric, observed, predicted_central, predicted_mean, predicted_median, central_method, ci_<k>_lower/upper`).
    * **New exported `calc_model_parameter_sensitivity()` and `calc_model_convergence_status()`** hold the HSIC computation and the convergence-status-table assembly (and their CSV writes); the corresponding `plot_*` functions are now pure consumers (accept a precomputed result, no longer own the CSV).
    * **In-memory objects persisted** for the renderer: `medoid_ensemble.rds` (G3 — the one ensemble previously unserialized), `subset_opt.rds` (G4). Every persisted `.rds` carries a `mosaic_schema_version` stamp so the renderer can warn-and-skip incompatible old run directories.
    * **`plot_model_parameters` wired in** as a renderer diagnostic (`parameters` group) — previously orphaned. `plot_model_ensemble(save_predictions=)` is **deprecated to a no-op with a warning** (not silently removed); the masking regression tests migrate onto `.mosaic_assemble_prediction_table()`.
    * **Tests:** engine-free P4 regression (`test-plots-false-still-writes-data.R`) asserts the data CSVs exist + match plotted content without any calibration; renderer round-trip (`test-render_MOSAIC_figures.R`) asserts figures appear and **no simulation is triggered** (P5, via mocked re-sim entry points), plus warn-and-skip on missing / schema-incompatible artifacts. Files: `R/render_MOSAIC_figures.R` (new), `R/calc_model_parameter_sensitivity.R` (new), `R/calc_model_convergence_status.R` (new), `R/plot_model_ensemble.R`, `R/plot_model_parameter_sensitivity.R`, `R/plot_model_convergence_status.R`, `R/run_MOSAIC.R`, `R/run_MOSAIC_helpers.R`.
# MOSAIC 0.51.0

* **Per-location `alpha_1`** (within-metapopulation population-mixing exponent). `alpha_1` is now driven **end-to-end as a per-location quantity** (length-`nL` vector) — sampling → priors → config validation → reticulate → engine, **including the Dask worker path**. The laser-cholera engine is already dual-mode (a scalar `alpha_1` is broadcast to all patches; a length-`(num_nodes,)` array is applied elementwise per patch in the force of infection), so **no engine change** was required. **`alpha_2` stays a single global scalar** (weakly identified given ψ absorbs the environmental signal — by design).
    * **Prior (`priors_default` v15.16):** `alpha_1` relocated from a single global `Beta` to a **per-location** prior carrying a **shared informative `Beta(28.4, 71.6)`** for every ISO (mean 0.284, sd ≈ 0.045, 95% CI ≈ [0.20, 0.38], cleanly within the engine `(0, 1]` invariant). The tight shared prior emulates hierarchical shrinkage (MOSAIC's independent-per-ISO sampler cannot express a true hierarchy) and starves the `alpha_1`↔`beta_j0_tot` degeneracy while still letting real per-location signal move it. `alpha_2` prior unchanged.
    * **Seed config (`config_default` v4.7):** `alpha_1` now stored as a length-`nL` vector (`rep(0.27, nL)`) so the `convert_matrix_to_config` round-trip preserves per-ISO calibrated values (a scalar seed silently dropped indices 2..`nL`). `alpha_2` remains a scalar (`0.50`).
    * **Three silent-corruption sites fixed in lockstep:** (a) `validate_sampled_config()` now treats `alpha_1` as **dual-mode** (scalar OR length-`nL`) rather than a rigid global scalar that hard-errored on a vector; (b) `get_param_names()` classifies a length-`nL` `alpha_1` as **per-location** (scalar `alpha_1` still routes to `$global` via the length-mismatch fallback); (c) the `convert_matrix_to_config` round-trip is now robust because the seed config carries a length-`nL` `alpha_1`.
    * **Dask parity:** `alpha_1` added to `mosaic_dask_worker.py::_VECTOR_FIELDS` (NOT `_AS_NDARRAY_FIELDS` — the engine recasts it to float32 via `np.asarray` regardless of input type, so it is parity-safe and does not trigger the float64-vs-float32 state-seeding divergence the `_AS_NDARRAY_FIELDS` header warns about).
* **Scalar back-compat preserved.** A scalar-`alpha_1` config (national `nL=1` and legacy configs) still validates and runs unchanged (engine broadcast); regression tests pin this.
* **Tests:** per-location `alpha_1` sampling invariants (length-`nL`, range `(0,1]`); `make_LASER_config` dual-mode validation (scalar / length-`nL` accepted, wrong-length / out-of-range rejected); `get_param_names` dual-mode classification; `convert_config_to_matrix`/`convert_config_to_dataframe` `alpha_1_<ISO>` expansion + round-trip; Dask schema-parity `alpha_1_<ISO>` assertion; `validate_sampled_config` dual-mode cases.
* **Note:** the logged sampled-parameter count rises by `nL` (e.g. +40 continental) now that `alpha_1` is per-location; no ESS/budget code change.

# MOSAIC 0.50.0

* **B2.1 — engine-correct `CFR_target` → `mu_j_baseline` chain factor** (`config_default` v4.6; `priors_default` **unchanged** at v15.15; statistician-validated). The B2 derivation (0.49.x) coupled `mu_j_baseline` to `CFR_target` via `gamma_1 * rho / (rho_deaths * chi_blend)` with `chi_blend = 0.5·(chi_endemic + chi_epidemic)`. A controlled-probe diagnosis of the laser-cholera deaths/`reported_cases` mechanism (statistician memory `b2-cfr-chain-factor-diagnosis`) showed the engine actually implies a **different chain factor**, fixed here in `sample_parameters()`:
    * **Dwell: `gamma_1` → `(1 - exp(-gamma_1))`.** Since laser-cholera v0.14.0 (#67) `reported_cases` is a thinning of daily *incidence*, not of symptomatic *prevalence-days*, so the recovery-tick factor relating the per-day mortality hazard `mu` to a per-incidence CFR is the survival complement `(1 - exp(-gamma_1))`, not `gamma_1`.
    * **PPV: `chi_blend` → `chi_epidemic`.** `reported_cases` is an `Isym` stock-read dominated by epidemic-regime ticks, so the *effective* PPV leans to `chi_epidemic` rather than the endemic/epidemic blend.
    * New derivation: `mu_j_baseline = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)`. The `[0,1]` engine clamp and the Lesson-#12 version-skew guard are unchanged.
* **Why:** the OLD-B2 chain over-attributed deaths. B2.1 **reduces realized deaths bias 16–28%** on the problem countries (statistician validation) with **no harm to the well-calibrated ones** (COG slightly over-corrects to realized/target ≈ 0.91, within the residual). It removes the *derivable* part of the deaths-scale error; an **irreducible ≈1.3–1.5× dynamics-dependent residual** — driven by the realized epidemic-regime fraction and spatial coupling, which no closed form can capture — remains, and is expected.
* **`config_default` v4.6** — the static `mu_j_baseline` anchor is re-derived to match: `cfr_to_mu_adjustment = (1 - exp(-0.10)) * rho_mean / (rho_deaths_mean * 0.75) ≈ 0.1303` (was `≈ 0.1467` under v4.5; every country's config-default `mu_j_baseline = CFR_target × anchor` scales ×0.888). `CFR_target`, `psi_jt`, the surveillance fit target, and the calibration window are all **identical to v4.5** — patched surgically from the stored per-country `CFR_target` (no source-data regen, so no unrelated current-source drift). Per-episode CFRs for the high-N countries remain in `[0.5%, 15%]`. `priors_default` is **untouched** (B2.1 changes only the *derivation*, not the `CFR_target` prior).
* **Docs:** `MOSAIC-docs/04-model-description.Rmd` `mu_{j,0}` derivation (eq. `mu-baseline-derivation`) corrected to the incidence-based `(1 - exp(-gamma_1))` dwell factor and epidemic-leaning `chi^{epi}`, with the residual noted.
* **Tests:** the B2 regression fixtures (`test-sample_parameters_B2_mu.R`), the implied-CFR consistency guard (`test-cfr-pipeline-consistency.R`), and the hand-computed Fixture-1 `mu` (0.00273 → 0.00220) updated to the B2.1 identity. The engine read-back diagnostic (`test-implied-cfr.R` / `calc_implied_cfr.R`) is unchanged — it recomputes from the *emitted* `mu`, which is the point.

# MOSAIC 0.49.4

* **Test hygiene (maintainer review of the 0.49.x burst):** the three new 0.49.1 regression files were green only because the author's session happened to satisfy their hidden environment assumptions — they false-passed elsewhere. Two fixes, no production-code change:
    * **`test-ensemble_cluster_robust.R` / `test-run_batch_robust.R` now skip under the parallel testthat harness.** Both spawn a PSOCK cluster and `SIGKILL` its workers; doing that *inside* a `Config/testthat/parallel` worker (a callr subprocess) collides with testthat's own result IPC and crashed the harness. They now call `skip_if_testthat_parallel()` (the established package guard, matching `test-dask-psock-orchestrator.R` / `test-optimize_ensemble_subset.R`), so they run serial-only — exactly as covered by the serial `devtools::test()` and the CI engine-install run. The worker-death-robust gather logic itself is unchanged and verified correct (the dead-worker→scalar-`FALSE` coercion keeps the calibration `sum(unlist(success_indicators))` tally valid).
    * **`test-sample_parameters_B2_mu.R` is now root-independent.** 6 of its 9 blocks call `sample_parameters()`, which calls `get_paths()` whenever `PATHS` is `NULL` — so in any process/CI without a MOSAIC root set they errored in `get_paths()` *before* reaching the B2 logic (including block 9, which left the B2 version-skew `stop()` guard with zero real coverage). The blocks now pass a `PATHS = list()` stub (honouring the file's own "self-contained, no MOSAIC root needed" contract), exercising the B2 derivation and the version-skew guard directly. All 9 blocks pass with no root set.

# MOSAIC 0.49.3

* **laser-cholera engine 0.16.0 → 0.16.1** — contract-neutral for MOSAIC: the only engine-source change (0.16.0→0.16.1) is a Python-side `compute_wis_parametric_row` NaN-poisoning fix (#91) that MOSAIC's R-side likelihood does not use; SEIR dynamics, parameters, and the result schema are unchanged, so simulated cases/deaths are identical. Pin updated in `inst/python/environment.yml`.
* **B2 — dynamic `mu_j_baseline` ← `CFR_target` coupling** (`priors_default` v15.15 / `config_default` v4.5): `mu_j_baseline` is now derived at sample time from `CFR_target * gamma_1 * rho / (rho_deaths * chi)`, so the implied CFR is invariant to `gamma_1` drift and the ETH dwell stop-gap is removed. (0.49.0)
* **Worker-death-robust parallel gather** (`.mosaic_cluster_lapply_robust`): a PSOCK worker *process* crash (laser/numba C-abort or OOM) during the calibration or ensemble gather previously hung the master forever on Linux (blocking `unserialize()`); it now degrades on the survivors with a warning, or `stop()`s with a diagnostic past an idle timeout. Wired into both `.mosaic_run_batch` (calibration) and `calc_model_ensemble`. (0.49.1–0.49.2)

# MOSAIC 0.48.3

* **`priors_default` v15.14 / `config_default` v4.4 — deaths-bias + cases-bias prior fixes from the Stage-1 metapop review** (validated on a 6-country dugong recalibration before shipping; cases:deaths weight 1.0:0.5, matching the Stage-1 fits).
    * **B1 — `mu_j_baseline` CFR→μ derivation re-anchored** to posterior-consistent chain factors (`gamma_1` 0.1133→0.10, `chi` 0.639→0.70), scaling every country's center **×0.805** (cases-neutral). Corrects the systematic deaths over-prediction traced (independent 4-agent review) to the derivation freezing `gamma_1` at its prior mean while calibration consistently pulls it below the anchor (implied CFR ∝ `1/gamma_1` inflated ~1.2–1.8×). `mu_j_baseline` magnitude (the v0.13 `rho_deaths` correction) and `rho_deaths` itself are **unchanged**. The ETH ×0.40 dwell stop-gap is **retained** (held ≈0.00088): B1's *uniform* ×0.805 under-corrects the low-`gamma_1` countries, so ETH keeps its residual until the deferred Phase-2 dynamic per-country `gamma_1`-coupled derivation (B2) subsumes it.
    * **`beta_j0_tot` per-country recenter** — geometric-mean shrinkage `new_meanlog = 0.5·log(prior) + 0.5·log(posterior_median)` (w=0.5, **width kept**) toward the Stage-1 posteriors for the 18-country data cohort, correcting the ~1.6× cases over-prediction (data halved `beta` in most). Genuinely per-country (5 countries — BDI/MWI/NGA/RWA/TZA — move *up*); COG's deliberate override **preserved**; LBR (unidentified) left at default. Width is intentionally not tightened (the warm-start ×2 `beta_j0_tot` inflation guard).
    * **Validation (6-country dugong recalibration, 2018 window):** deaths bias dropped in all 6 (NER 2.81→2.25, MOZ 1.76→1.23, SSD 2.27→2.10, …), cases R² unchanged (≤0.01) — cases-neutral as designed. Residual ~2× on COD/SSD is the by-design implied-CFR / median-ensemble floor (`project_central_method_v038`), not a prior issue; the low-`gamma_1` countries (e.g. MWI, ~unchanged under B1) are the Phase-2 B2 case. Full `devtools::test()` green (FAIL 0).

# MOSAIC 0.48.2

Pre-scale-up package-health audit (parallel `swe` + `maintainer` review of the v0.45–v0.48 burst). Two Dask/Coiled-path correctness fixes (local PSOCK was unaffected throughout) plus build/doc hygiene.

* **[CRITICAL] Dask path: per-cell weight matrices no longer leak into the per-sim JSON.** `.extract_sampled_params()` (`run_MOSAIC_helpers.R`) did not exclude `reported_cases_weight`/`reported_deaths_weight`. Those matrices contain `NA` cells; on the Dask path `jsonlite::toJSON(digits=NA)` serializes `NA_real_` as the **string `"NA"`**, and `config.update(sampled)` on the worker then overwrote the clean float64 base_config arrays with lists carrying `"NA"` — so `np.asarray(..., dtype=float)` in the v0.48.0 weighted scorer raised `could not convert string to float: 'NA'`, **failing every Dask sim**; it also re-serialized the ~1.5 MB matrices per sim (~30× JSON bloat at 40K-sim scale). The matrices are broadcast once (clean numpy) via `.extract_base_config()`, so they are now excluded from the per-sim set. Latent until v0.48.0 began *consuming* the weights; a textbook local/Dask divergence (Lesson #12) — local PSOCK has no JSON round-trip.
* **[HIGH] Dask path: dtype parity for `as_ndarray` engine fields.** The engine's `as_ndarray()` returns an incoming numpy array unchanged but casts an incoming *list* to the declared dtype (`uint32` for IC counts, `float32` for seasonality/transmission vectors). The local PSOCK path ships these as lists (via `reticulate::r_to_py`), so the engine casts them correctly; the Dask worker pre-converted them to float64 numpy, so `as_ndarray()` left them float64 and the engine seeded its stochastic state in float64 instead of uint32 — a silent ~1–2% log-likelihood divergence from local at identical values + seed. A new `_AS_NDARRAY_FIELDS` registry keeps these fields as lists on the worker (and converts any base_config numpy copies back to lists), restoring bit-level dtype parity with local.
* **Hygiene:** rewrote a dead/empty dedup test in `test-get_location_priors.R` (guarded on a field path that no longer exists → 0 assertions; now 7 real assertions); fixed two roxygen Rd cross-reference WARNINGs (`calc_model_ensemble.Rd` `\link{i}`→`\code{}`; `get_greek_unicode.Rd` `\link{ggplot2}`→`\pkg{}`); added `__pycache__/`/`*.pyc` (and `vignettes/cache|figures`) to `.gitignore`/`.Rbuildignore` (the parity test's `source_python` compiles a `.pyc` into `inst/python/`); converted non-ASCII glyphs in `plot_model_subset_optimization.R` to `\uXXXX` escapes.
* **Known follow-ups (not blockers):** (a) CI never pip-installs laser-cholera, so the engine-backed parity tests — including the v0.48.0 Dask loc_idx guard — only run locally/on the VM, never in CI; (b) `burn_in_days=30` (the default) silently clamps to `n_time` on short/outbreak windows, yielding degenerate 0-contribution scoring — set `burn_in_days=0L` for short-window sims (a guard-warning is a candidate change pending statistician/disease-modeler input); (c) upstream laser-cholera WIS `np.sum`→`np.nansum` gap (see `claude/wis_issue_draft.md`).

# MOSAIC 0.48.1

* **Test fix (post-v0.16.0 audit):** `test-calc_model_likelihood_python_parity.R` test #7 had *pinned* a known R↔Python divergence in the zero-prediction cumulative penalty (Python ~1.78× more negative than R, observed on laser-cholera 0.13.1) with an explicit instruction to convert to an exact-parity assertion once the engine aligned. laser-cholera v0.16.0's verbatim port of `calc_model_likelihood` **aligned** the penalty (R and Python now agree to ~1e-12), which tripped the pinned ratio — the single failure surfaced by the full-suite health audit. The test now asserts exact parity. No production-code change; this is the early-warning test doing its job. `devtools::test()`: 0 failures. (Note: the built-tarball `R CMD check` carries a known non-CRAN baseline of standing ERRORs/WARNINGs/NOTEs — CRAN-incoming/examples-need-data-root/relative-`source()` test paths — none from this change; MOSAIC is explicitly non-CRAN and CI gates on `testthat::test_local()`, which is green.)

# MOSAIC 0.48.0

* **laser-cholera engine bumped to v0.16.0** (`inst/python/environment.yml`; both the release tag and the wheel filename). Two engine features are now integrated:
    * **Dual-mode `alpha_1` / `alpha_2`.** The engine now accepts either a global scalar (existing behaviour, bit-identical) **or** a per-location (`length(location_name)`) vector for the FOI mixing exponents, which `humantohuman.py` broadcasts/indexes via `np.power`. `make_LASER_config()` validation now accepts both forms and enforces the engine's range invariants (`alpha_1 ∈ (0, 1]` strict-positive, `alpha_2 ∈ [0, 1]`); the previous validator hard-required a scalar and used the looser `[0, 1]` lower bound for `alpha_1`. **Default sampling/priors remain global-scalar** — the per-location form is accepted for direct `run_LASER()` configs; per-location *sampling* (per-iso priors) is intentionally deferred. The scalar path is unchanged end-to-end (serialization, local PSOCK, and Dask all already pass a scalar through untouched).
    * **Dask/Coiled likelihood now honors the per-cell confidence weights (parity fix).** laser-cholera v0.16.0's `calc_model_likelihood` gained the `weights_obs_cases` / `weights_obs_deaths` matrix arguments (ported verbatim from MOSAIC's R `calc_model_likelihood`). The Dask worker (`inst/python/mosaic_dask_worker.py`) previously could not apply them — it emulated only the deaths-prefix zero by NaN-ing the deaths obs and otherwise ran **unweighted**, diverging from the local PSOCK R score whenever `reported_cases_weight`/`reported_deaths_weight` were non-trivial. The worker now passes the real (sliced, deaths-prefix-zeroed) weight matrices to the Python `calc_model_likelihood`, mirroring `run_MOSAIC.R` exactly, and **recomputes on-worker whenever the weights are non-trivial OR the scored window starts after t=1** (the engine analyzer's `model.log_likelihood` never sees `weights_obs_*`, so it cannot be trusted in those cases). The pure-default path (trivial weights, no slice) still uses `model.log_likelihood`, bit-identical to the engine analyzer. A new `_weights_obs_matrix_trivial()` helper gates the decision (matrix-level analogue of R's `.weights_obs_row_trivial`).
    * **Worker peak-sourcing fix (shape-term parity).** The v0.16.0 Python `calc_model_likelihood` matches `epidemic_peaks` rows by an integer `loc_idx` column (`getattr(row, "loc_idx", None)`), silently dropping any row that lacks it. The raw `config["epidemic_peaks"]` scattered to workers carries only `iso_code`/`peak_date` (see `.filter_epidemic_peaks()`), so the rewritten `_score_window_likelihood()` would have zeroed **every** peak-timing/peak-magnitude/cumulative shape term whenever shape weights were on — diverging from the local PSOCK R scorer (which dispatches by `iso_code`). The worker now sources peaks from `model.params["epidemic_peaks"]`, which the engine's `params.py` enriches with `loc_idx` at model construction — identical to what the engine analyzer reads. Verified to numerical tolerance against the R local path for the full burn-in-slice + deaths-prefix + per-cell-weights + peak-timing/magnitude/cumulative case. (Shape weights default to 0, so this was latent.) Covered by the new `test-dask_worker_score_window_parity.R`. **Known engine-side gap (not fixed here):** v0.16.0's `compute_wis_parametric_row` uses `np.sum` rather than `np.nansum`, so a single `NA` observation poisons the entire WIS row to `NaN` and the term is dropped engine-side, while R's `na.rm = TRUE` retains it — local and Dask diverge on the WIS term when `weight_wis > 0` over gappy surveillance data. WIS defaults to 0; a fix belongs in the read-only laser-cholera engine.
* **Resume safety:** v0.16.0 is intentionally **not** added to the `.mosaic_lc_likelihood_compatible()` byte-identical allow-list `{0.14.0, 0.15.0}`. Under non-trivial obs-weights a v0.16.0 Dask shard differs from a 0.15.0 shard, and the version-keyed allow-list cannot tell whether a given shard used trivial weights — so a 0.15.0 → 0.16.0 resume re-runs rather than risk pooling weighted and unweighted likelihoods.

# MOSAIC 0.47.5

* **Hardening of the config/priors manipulation ops behind the staged metapop calibration** (independent `swe` + `maintainer` deep review on the full 40-location structure). The staged round-trip (`get_location_config` → `update_priors_from_posteriors` → `inflate_priors` → `make_LASER_config`/`sample_parameters`) was verified engineering-correct on the real 40-loc `config_default`/`priors_default` and real Stage-1 posteriors (39/39 non-target-location invariance, exact lognormal `beta_j0_tot` inflation, bit-perfect matrix round-trip). Two defensive gaps closed:
    * `.validate_updated_priors()` now checks per-group location **names**, not just counts — a name-drift that preserves cardinality (e.g. a relabelled iso) previously passed validation silently (the very misalignment class this validator exists to catch).
    * `get_location_priors()` now returns locations in **canonical source order** (matching `get_location_config()`, which selects via `which(location_name %in% iso)`) instead of requested-argument order — removing a position-vs-name foot-gun for multi-iso callers. Sampling is name-keyed, so this changes order only, not values (no result change).
* **New `test-staged_metapop_prior_ops.R`** exercises these on the real 40-loc objects (previously only 2-location toy fixtures): partial-fold non-target invariance, validator name-drift detection, `get_location_priors`/`get_location_config` order consistency, full-40 `inflate_priors` scope + lognormal mean/variance, and the `convert_config_to_matrix`/`convert_matrix_to_config` 40-loc round-trip. (Deferred, doc-only: `convert_config_to_dataframe` is not param-set-equivalent to the matrix path — review item LOW-3, unused by `run_MOSAIC`.)

# MOSAIC 0.47.3

* **Default `control$likelihood$burn_in_days` changed from `0` to `30`.** The per-channel scored window (v0.47.0) is now **on by default**: the first 30 daily steps are excluded from the likelihood and R²/bias scoring. Rationale from the optimal-window research: the seeded E/I discharge into **cases** settles by ~day 14–21, but the **deaths** IC transient lags (death event + `delta_reporting_deaths` ≈ 5 d) and is still decaying out to ~day 28–35 — so a single 30-day window clears both channels' transients. On KEN this took cases-R² 0.338 → 0.56 and kept dropping the deaths bias well past 21 (3.26 → 2.50 at 21 → 2.22 at 28). Cost is ~1 % of a 2018 window / ~2.5 % of the 2023 window, and it is a no-op where there is no start spike (high-burden countries unaffected). **Behavior change, not bit-identical:** like the `central_method` change, this alters the scored target, so `config_best`/subset/medoid selection differs and runs are **not comparable across this boundary**. Set `control$likelihood$burn_in_days = 0L` to restore pre-v0.47.3 scoring. **Note:** on very short windows 30 may clamp toward `n_time` (the `min_obs_for_likelihood` gate then yields a 0-contribution, fail-safe) — set `0L` for short-window/outbreak sims. The residual deaths-bias floor (KEN ~1.6, COD ~1.27) is model deaths-baseline / implied-CFR, NOT IC transient, and is unaffected by burn-in (that's the `deaths_score_start`/CFR lever).

# MOSAIC 0.47.2

* **`config_default` (v4.3) + `priors_default` (v15.13) rebuilt under the relaxed trust-tier gate, at the unchanged 2023-01-01 window.** Following the v0.47.1 fourier-keep policy, the shipped fit-target matrices now retain `fourier_*` reconstructions at their lower per-week `confidence_weight` instead of NA-blanking them. 811 fourier weeks fall in the 2023+ window, so the change is material: `reported_cases_weight` cells in `(0,1)` rise from 10,062 → 17,073 (range 0.40–0.95), with the complementary drop in `==1` cells — those weeks now inform the fit as down-weighted real observations rather than being dropped. The simulation window is **unchanged** (`date_start = 2023-01-01`, `ncol = 1398`); only the per-observation weighting of previously-discarded weeks differs. priors `build_date_start = 2023-01-01`, `ic_t0 = 2023-02-01` (IC seeding epoch unchanged). `test-config_default_weights` (21/21) and `test-cfr-pipeline-consistency` (93/93) pass.

# MOSAIC 0.47.1

* **Surveillance trust-tier gate relaxed: keep `fourier_*` reconstructions, down-weighted.** `process_cholera_surveillance_data()` now NA-blanks **only** `assumed_zero` weeks (a pure surveillance-silence assumption). `fourier_*` rows — synthetic reconstructions of *real* annual/quarterly totals — previously hard-dropped are now kept and reach the daily fit target carrying their lower per-week `confidence_weight` (~0.4–0.5 vs ~0.9 for `observed`), which `calc_model_likelihood()` consumes as a per-observation weight. So low-confidence-but-real-magnitude weeks (e.g. the 2019–2022 JHU→WHO handoff gap) inform the fit at reduced weight rather than being discarded. `observed`, `documented_zero`, and direct WHO/JHU/SUPP rows are unchanged. Code-only here; the shipped `config_default`/`priors_default` are rebuilt under this policy in a follow-up commit.

# MOSAIC 0.47.0

* **Per-channel scored time window (burn-in + deaths-era start).** New run-time-only knobs in `control$likelihood` exclude leading time steps from the likelihood and R²/bias scoring, without touching `config_default`, the builders, or `ic_t0`:
    * `burn_in_days` (integer ≥ 0, default `0L`) — drops the leading IC-transient steps from **both** channels. MOSAIC seeds E/I by moment-matching at `date_start`; the seed discharges into reported cases over the first ~1–2 weeks, producing a day-1 spike the forced steady state can't match (verified on KEN: medoid day-3 cases 2761 vs observed 15, ~184× over observed; decays to ~observed by day 21). `burn_in_days=21` removes it from the scored series entirely (scored-window max 45 vs observed max 125).
    * `deaths_score_start` (`NULL` or `Date`/`"YYYY-MM-DD"`, default `NULL`) — scores deaths only from a later date (non-stationary observed CFR makes deaths unfittable before ~2023 for several countries).
    * `score_start_cases` (`NULL` or date, default `NULL`) — optional explicit cases start overriding the burn-in.
* **Mechanism.** Resolved once to per-channel 1-based start indices (`.mosaic_resolve_score_window()`), the worker **slices** `obs`/`est` to `min(idx_cases, idx_deaths):n` (not merely down-weighting — the peak/cumulative shape terms don't honor `weights_time`), zeros the deaths residual prefix, and passes the **sliced start date** so the peak `date_seq` stays aligned. R²/bias sites drop the unscored head via a generalized `.mosaic_mask_central_for_scoring()`; `calc_model_ensemble()` records `score_idx_*` in `artifact_mask`; `plot_model_ensemble()` blanks the unscored head for display.
* **Bit-identical at defaults.** All knobs at their defaults yield `idx_cases = idx_deaths = 1` ⇒ no slicing ⇒ scoring identical to pre-feature runs (new `test-burn_in_scoring_parity.R` + the unchanged tier-2/reference/obs-weights parity suites). Carried in `control$likelihood` ⇒ byte-compared by the resume guard ⇒ changing the window across a resume correctly errors. Dask path re-injects the resolved `score_idx_*` and filters `epidemic_peaks` at the sliced start (schema-parity test extended).

# MOSAIC 0.46.4

* **Hardening from the adversarial red-team of the 0.46.1–0.46.3 changes.**
    * `calc_model_ensemble()` gains a **RAM-projection guard**: before allocating the dense `[loc×time×param×stoch]` ensemble arrays it projects the concurrent peak and emits a loud warning (naming `max_best_subset`/`n_iter_ensemble` to dial down) when it would exceed ~80% of system RAM — so a long-window run (e.g. a 2015, ~4320-column config → ~51 GB at production defaults) fails loud with guidance instead of OOMing the orchestrator. Warn-only; a no-op at normal (2023, ~1400-col) widths.
    * `make_config_default.R` now **asserts its resolved `date_start` matches the installed `priors_default$metadata$build_date_start`** (newly recorded by `make_priors_default.R`) and `stop()`s on mismatch — closing the silent config/priors window-desync reachable if the documented rebuild order was skipped (back-compat warning for older priors lacking the field).
    * `ic_t0` selection **relabeled to its actual criterion — broadest-data-coverage** month (maximizes how many countries are seeded from real surveillance rather than Beta defaults), with an explicit note that case-volume weighting was considered and rejected (it biases the epoch toward an outbreak peak and over-seeds E/I). Factored into a pure, unit-tested `.ic_select_epoch()` helper (previously untested).
    * Doc/cosmetic nits: `optimize_ensemble_subset()` `@param` no longer claims the package default is `"mean"`; `plot_model_ensemble()` uses an adaptive date-break ladder (~10–15 ticks) instead of a fixed 3-month interval (~44 ticks over an 11-yr window).

# MOSAIC 0.46.3

* **Build start date (`date_start`) generalized across the config + priors builders.** `make_config_default.R` and `make_priors_default.R` both honor a `MOSAIC_BUILD_DATE_START` env override (single source of truth; fallback 2023-01-01) so the same start date flows into both, enabling back-history rebuilds (e.g. a 2015-01-01 start — all covariates cover ≥2015). The required rebuild order is documented in `make_config_default.R`. The `ic_t0` tie-break is now explicit (earliest month), with a loud guard if the 2023 anchor ever drifts off 2023-02-01 and a cold-start warning for far-future starts.

# MOSAIC 0.46.2

* **Data-driven initial-condition seeding epoch (`ic_t0`) that tracks `date_start`.** Replaced the hard `max(date_start, 2023-02-01)` floor — whose "2015 has no active-case countries" premise predated the multi-source JHU/AI back-history — with a data-driven epoch (broadest-data-coverage month) within `[date_start, date_start + 12mo]`. Inert for the 2023 default (resolves to 2023-02-01; shipped priors numerically unchanged), and for a back-history build it seeds ICs from that era's data instead of an 8-year-mismatched 2023 state.

# MOSAIC 0.46.1

* **Default ensemble `central_method` changed from `"mean"` to `"median"`.** The posterior-weighted ensemble now summarizes each per-cell predictive with the **median** by default — the conventional epidemiological-forecast point estimate, robust to stochastic right-tail outliers (this is the rationale; note that on a right-skewed predictive a lower bias-to-1.0 ratio mostly reflects the skew, so "lower bias" alone is *not* the justification). **Behavior change, not reporting-only:** `central_method` feeds the best-subset optimizer (`optimize_objective = "mae"`), so this changes the selected subset, the medoid, and `config_best.json` — runs are **not directly comparable across this boundary**. Both `*_ensemble_mean` and `*_ensemble_median` R²/bias remain in `summary.json`, so the mean-based implied-CFR/skew diagnostic is preserved as a cross-walk field. All default sites updated in lockstep (resolver, `mosaic_control_defaults`, `plot_model_ensemble`, `run_rolling_cv`).

# MOSAIC 0.46.0

* **Engine-artifact mask for post-calibration R²/bias scoring (metrics-only).** Two laser-cholera (0.15.0) array artifacts were silently contaminating the post-calibration fit metrics: (1) the first ~2 `reported_cases` timesteps are an initial-condition warm-up transient (seeded E flushing into `new_symptomatic` before the SEIR dynamics settle), and (2) the final `reported_deaths` timestep is a structural zero (written at `[tick]` then `[1:]`-trimmed; laser issue #82). `plot_model_ensemble()` already masked these for DISPLAY ONLY; the R²/bias metrics in `run_MOSAIC()` still scored them.
    * `calc_model_ensemble()` now carries the mask spec via two new params (`n_cases_warmup_mask = 2L`, `mask_final_deaths_step = TRUE`, defaults matching `plot_model_ensemble()`) and records the resolved spec in the returned `mosaic_ensemble` object as `artifact_mask = list(cases_warmup, deaths_final)`. The central/quantile/array fields stay RAW (unmutated).
    * A new internal helper `.mosaic_mask_central_for_scoring()` sets the artifact time-positions to `NA` on the EST central matrix only; observed series stay unmasked and `calc_model_R2()`/`calc_bias_ratio()` drop the NA pairs pairwise. Wired into every R²/bias scoring site in `run_MOSAIC()` (canonical ensemble, dual mean/median, tier subset, medoid; windowed metrics inherit the masked canonical series). The medoid SELECTION distance is deliberately left unmasked. CSV/prediction export and plot series are unchanged.

# MOSAIC 0.45.5

* **Test suite sped up ~2x and re-tiered.** The default fast-tier `devtools::test()` now runs in parallel (`Config/testthat/parallel: true`) at ~70–90s wall-clock on a 10-core laptop versus the ~153s single-threaded baseline, with no loss of assertion coverage. Changes:
    * **Parallel testthat** with `Config/testthat/start-first` ordering the slowest cluster-spawning files first. A new `setup-python.R` pins all thread env vars (OMP/MKL/OPENBLAS/NUMEXPR/TBB/NUMBA + ARROW = `1`) and `BLAS` threads to 1 in every worker *before* Python starts, then probes the interpreter once and caches the result in `options()`. The 5 tests that themselves fork (`mclapply`) or spawn PSOCK clusters self-skip under a testthat callr worker (`skip_if_testthat_parallel()`) to avoid result-IPC corruption; CI runs serial (`TESTTHAT_PARALLEL=FALSE`) so they retain full coverage.
    * **Removed a gratuitous TensorFlow load** from `test-est_suitability_dispatch.R`: the deprecation/unknown-arg tests now mock `.est_suitability_lstm_v2` (the message fires before dispatch) instead of falling through into the real lstm_v2 path (~14s saved where TF is installed).
    * **Trimmed Monte-Carlo / whole-config work** in the prior and sampler tests: `est_zeta_*` `n_sim` 10000/5000 → 500; `sample_parameters()` distributional loops 200/100 → 40; a shared `local()`-memoized `.cached_sampled_config()` fixture (`helper-fixtures.R`) replaces repeated full 301-parameter draws in structural-assertion tests.
    * **Centralized skip helpers** into `helper-skips.R` (single source of truth for `skip_if_no_python_likelihood`, `skip_without_tensorflow`, `skip_if_no_data`, `skip_if_few_cores`, `skip_if_no_rho_deaths_prior`), removing duplicated inline copies across 5 files, and added `skip_if_slow()` (gated on `MOSAIC_RUN_SLOW_TESTS`) to tier genuinely-slow non-engine tests.
    * **Shrank the flood-GAM test fixture** (`test-impute_flood_probability.R`) from 5→2 synthetic ISOs (28s→19s warm); the AUC signal-recovery assertion is preserved always-on (AUC 0.82–0.88 across seeds, clearing the 0.75 threshold with margin).
    * **CI workflow** (`.github/workflows/R-CMD-check.yaml`): push/PR runs the fast tier; a scheduled + `workflow_dispatch` job runs the slow tier with `MOSAIC_RUN_SLOW_TESTS=1`.

# MOSAIC 0.44.13

* **Upgraded the laser-cholera engine pin to v0.15.0** (`inst/python/environment.yml`). v0.15.0 is a docs-migration + validation-hygiene release: it converts `assert`-style checks to always-on `if/raise ValueError/AttributeError` (so parameter validation runs even under `python -O`), tightens the `metapop` CLI `--over` validation, and modernizes type hints — but it leaves the **parameter contract and the `model.results` schema (`reported_cases`/`reported_deaths`) unchanged**. Verified contract-neutral: a deterministic `run_LASER(config_default)` smoke run and the full test suite pass against v0.15.0 with no changes to the R observation-model code, priors, or config. Unlike the v0.13 (deaths-scale) and v0.14 (reported-cases-incidence) upgrades, no prior/derivation changes are required.
* **Resume now pools across verified-likelihood-compatible engine versions on the Dask backend.** Added `.mosaic_lc_likelihood_compatible()` (an explicit, conservative allow-list) and wired it into `.mosaic_resume_check_inputs()`: when persisted and current shards were both Python(Dask)-scored by laser-cholera versions whose engine `calc_model_likelihood` and output schema are byte-identical (currently `{0.14.0, 0.15.0}` — v0.15.0 changed only docs/type-hints), resume proceeds with a warning instead of hard-failing the likelihood-provenance guard. Non-allow-listed pairs (e.g. `0.13.0` ↔ `0.15.0`) still hard-error, and the local (R-scored) path is unaffected. Note: the **Coiled worker image / Coiled software env must be rebuilt to v0.15.0 to keep client↔worker version parity** (hybrid runs with mismatched engine versions remain invalid).

# MOSAIC 0.44.11

* Integrated the Phase 3 (#101) Dask worker-schema rewrite into main: workers now compute the likelihood on-worker and return a per-iter `{iter, seed_iter, likelihood}` schema, the Dask gather adapter collapses to a single gather-and-write loop, `.mosaic_inject_likelihood_settings()` re-injects `location_name` + `N_j_initial` and casts the observed surveillance matrices to double before scatter, and the `test-dask_worker_schema_parity.R` regression suite is added. Merged on top of the post-0.40.0 main evolution.

# MOSAIC 0.40.0

* **New `backfill_weekly_case_gaps()` + `compile_suitability_data(backfill_case_gaps = TRUE)` repair holiday-week false zeros in the suitability target.** Genuinely-missing weekly surveillance weeks (most often the Christmas/New-Year reporting lapse) were turned into `0` cases by the suitability pipeline's `NA→0` keras-target sanitiser, fabricating a "no transmission" week inside an active outbreak. The new helper linearly interpolates **short interior** gaps (`≤ max_interp_weeks`, default 2) that are bounded by reported weeks, on the per-group weekly series, inside `compile_suitability_data()` before the target is built. It is **suitability-local by design** — the canonical surveillance files and the calibration likelihood (which correctly NA-skips missing weeks) are untouched.
* Interpolation is **date-weighted** (handles unevenly-spaced rows, e.g. a dropped ISO W53 leaving a 14-day step); **reported anchors are never modified** (rounding/clamping apply to filled cells only, so fractional disaggregated counts are preserved); gaps **shouldered by a reported zero are left unfilled** (`min_anchor`, default 0 — a near-zero shoulder is more likely a genuine non-outbreak week); phantom duplicate `(iso, date)` rows are not fabricated; and a `cases_interpolated` flag marks the filled weeks.

# MOSAIC 0.39.0

* **`run_MOSAIC()` now produces only the posterior ensemble and the medoid representative model — the single best-likelihood model is no longer produced.** Removed: the best stochastic ensemble, its prediction plot/CSV (`predictions_best_*`), its Dask dispatch, the `config_best.json` artifact, and the best-seed sampling fatal gate. The best-**subset** calibration machinery (`is_best_subset`/`weight_best`, and the `is_best_model` flag in `samples.parquet`) is unchanged — it still drives the posterior ensemble and parameter diagnostics.
* `summary.json` now reports ensemble (+ tier) fit metrics only: the single-model fields `r2_cases`/`r2_deaths`/`bias_ratio_cases`/`bias_ratio_deaths` are removed. Run-success (`outputs_ok`/`[RUN_SUMMARY]`) keys off `r2_cases_ensemble`; the `r2_cases_best`/`r2_deaths_best` log fields are dropped.
* `run_rolling_cv()` drops `"best"` from its default `models` (now `c("ensemble","ensemble_opt","medoid")`). `"best"` is still accepted for back-compat and re-simulated when an older run dir carries `config_best.json`, otherwise skipped with a warning.
* Migration: scenario scripts that read `config_best.json` should repoint to `config_medoid.json` (the representative single-config model).

# MOSAIC 0.38.0

* **New `central_method` control selects the ensemble central tendency (default `"mean"`).** A single setting — `control$predictions$central_method`, `"mean"` (default) or `"median"`, scalar or per-channel `c(cases=, deaths=)` — now consistently governs the ensemble central trajectory used for (i) the prediction trajectory + plots, (ii) the canonical `*_ensemble` R²/bias metrics, (iii) the medoid representative member ("mean-medoid"), and (iv) the subset-selection objective (`optimize_ensemble_subset()`). The weighted **mean** is the unbiased estimator of expected counts (`E[Σ]=ΣE`) and never collapses to zero on sparse deaths (~92% zero-days), where the per-tick weighted median otherwise reads as a zero-collapse. Cases (dense) are insensitive (mean≈median).
* **Behaviour change:** because the default flips from the historical median to the mean, all `*_ensemble` metrics, the medoid seed, and the prediction plots differ from pre-0.38 runs and are not directly comparable. Set `central_method = "median"` to reproduce historical runs bit-for-bit (the regression-anchored path — ensemble, tier, and medoid metrics, the medoid seed, and the optimizer-selected subset N are all unchanged under `"median"`). The WIS objective is quantile-based and unaffected by `central_method`.
* **`summary.json` cross-walk:** during the transition both `r2_*_ensemble_mean`/`r2_*_ensemble_median` (and the matching `bias_ratio_*`) are emitted alongside the canonical `r2_*_ensemble`, plus `central_method_cases`/`central_method_deaths` provenance fields.
* **Prediction CSVs** now carry `predicted_central` (the plotted/scored series), `predicted_mean`, `predicted_median`, and a `central_method` column — `predicted_median` always holds the true median (never mislabeled). `plot_model_ppc()` prefers `predicted_central`.
* **Rolling-CV** (`run_rolling_cv()`, `compile_rolling_cv_predictions()`, `evaluate_rolling_cv()`, `plot_rolling_cv()`) threads `central_method` (default `"mean"`) so in-sample and out-of-sample point metrics use the same summary. The predictions table gains `pred_central`/`pred_mean`/`central_method` (keeping `pred_median`); R²/bias/MAE/RMSE/skill score on `pred_central` while WIS/coverage stay quantile-based on the median.
* **Reading the new numbers:** under the default mean, the canonical `r2_*_ensemble`/`bias_ratio_*` fields differ from pre-0.38 same-named fields (mean vs median); read `central_method_cases`/`central_method_deaths` and compare across versions via `r2_*_ensemble_mean`/`r2_*_ensemble_median`. In particular the **deaths bias ratio will rise to ~2× on existing runs** — this is the expected unmasking of the posterior implied-CFR property (accepted as admissible; see the plan §0), not a regression. The implied-CFR diagnostic (`summary.json:cfr_implied`) is computed from raw member arrays and is **unchanged** by `central_method`.
* **Medoid retarget:** `config_medoid.json` is now selected against the chosen central case series; scenario owners that pin the medoid base member should re-pin. The medoid distance is **cases-anchored** (a representative case trajectory), not deaths-calibrated — a single-member base under-states the across-member mean deaths, so deaths-sensitive counterfactuals should propagate the full retained ensemble.
* **Rolling-CV note:** OOS point metrics (R²/bias/MAE/RMSE/skill) default to the mean for in-sample/out-of-sample consistency; these are not directly comparable to externally-reported *median*-point forecast errors on right-skewed counts. WIS and interval coverage remain median/quantile-based (the externally-comparable scores).
* **Known cosmetic (sparse deaths):** the plotted central line is the weighted mean while the ribbon shows member quantiles, so on near-all-zero death series the mean line can sit slightly *above* its own 97.5% ribbon (a moment vs order-statistics of a zero-inflated count — mathematically correct, not a plotting bug). The proper-scoring "predictive fan" in plan §7 is the planned follow-up.

# MOSAIC 0.36.12

* Fixed a medoid-model mis-mapping: the medoid metric correctly selected the central ensemble member, but the seed it was mapped to came from a separate vector that could drift out of positional alignment with `cases_array`, so `config_medoid` was sampled from the wrong parameter set and its prediction could collapse to near-zero. `calc_model_ensemble()` now carries a per-member `seeds` vector aligned with `cases_array` (sourced from the parameter set that produced each member), `optimize_ensemble_subset()` propagates it through its sort/slice, and `run_MOSAIC()`'s medoid uses `ensemble$seeds[medoid_idx]`. Bit-identical for correctly-aligned runs (Tier-2 parity unchanged).

# MOSAIC 0.36.4

* Test integrity: replaced the two false-green parity-test `skip()`s with active divergence monitors so the R/Python likelihood-drift suite no longer reports green over known disagreements (review B2-6).

# MOSAIC 0.36.3

* Hygiene: declared previously-undeclared package dependencies, fixed markdown-link Rd errors, and updated `.Rbuildignore` (Batch 5a).

# MOSAIC 0.36.2

* Fixed Dask ensemble weight misalignment plus guardrail/renormalization correctness in the posterior-weighted ensemble (deep-review Batch 1).

# MOSAIC 0.36.1

* Fixed Dask worker-count staleness; added `plot_rolling_cv()` and lstm_v2 model artifacts.

# MOSAIC 0.36.0

* Run the orchestrator sampling/parquet-write loops on PSOCK rather than fork for cross-platform stability (B2).

# MOSAIC 0.35.2

* Separated local (`n_cores`) from remote (`dask_spec`) parallelism and fixed the Dask worker-count derivation.

# MOSAIC 0.35.1

* Fixed ensemble weight misalignment and applied a bit-identical Tier-2 performance refactor (guarded by parity fixtures).

# MOSAIC 0.34.0

## est_suitability(): new default lstm_v2 hierarchical-FiLM architecture (rolling-origin CV)

`est_suitability()` is now an architecture dispatcher. The v0.34 default,
`architecture = "lstm_v2_hierarchical_film"` ("gauge_A"/"B4"), replaces the
v0.33 shared LSTM with a hierarchical-FiLM model trained under expanding-window
rolling-origin cross-validation — fixing the v0.33 random-split temporal leak
that collapsed out-of-sample forecasts to a near-flat line.

**Architecture.** A 3-stack LSTM trunk (128->64->32, recurrent dropout) maps the
13-week covariate window to a shared climate-response latent `z`; a region
embedding and a zero-initialized country *deviation* embedding modulate `z` via
two FiLM stages (`tanh` gains in `[0,2]`, identity at init), with an L2
partial-pool penalty shrinking data-sparse countries toward their region. BCE
loss under `balanced_uniform` sample weights (correcting the zero-week
imbalance) plus an optional per-row confidence-weight overlay that down-weights
AI-mined surveillance rows. Each step validates strictly forward in time
(4-week embargo); the model refits on full in-sample data at
`median(best_epoch)`; `n_seeds` fits are combined on the logit scale.

**Output schema (Option A).** The prediction CSVs now carry a single canonical
`psi` column (smoothed; bias-corrected when `bias_correct = TRUE`) consumed by
every psi->`psi_jt` reader, plus transparent diagnostics (`pred_raw`,
`pred_smooth`, `pred_bias_corrected`, and seed-dispersion quantiles
`q025/q25/q75/q975` — diagnostic, NOT predictive intervals). The latent v0.33
no-op (bias correction wrote `pred_calibrated` while readers used `pred_smooth`)
is fixed.

**Signature.** New shared toggles `feature_set` (default `"v7.3"`), `response_var`
(default `"transmission_intensity"`), `bias_correct` (renamed from `calibrate`),
`architecture`, and a single `arch_control` list holding all lstm_v2
hyperparameters (loaded from `inst/fixtures/B4_rolling_cv_spec.yml`). The v0.33
process knobs (`n_splits`, `seed_base`, ...) and `exclude_covariates` are frozen
inside the legacy path and absorbed via `...` with a deprecation message;
`calibrate` is mapped to `bias_correct`.

**Bias correction.** `calibrate_psi_predictions()` rewritten to a per-country
logit-scale affine fit on outbreak (non-zero observed) weeks only, applied via
the monotone inverse logit (no hard `[0,1]` clip). Zero-history / low-incidence
countries fall back to identity (uncorrected, region-FiLM-modulated psi).

**Region maps.** `arch_control$region_map` selects one of `snf_k5` (production
default; Similarity Network Fusion 4-view clustering), `csv` (4 WHO admin
regions; the B4 reproduction baseline), `seasonal_v1`, `hydro_v1`, or `snf_k4`.

**Provisional defaults.** The v0.34.0 defaults
(`response_var = "transmission_intensity"`, `region_map = "snf_k5"`,
`bias_correct = TRUE`) are provisional, gated by a post-merge psi->LASER
case-skill experiment; `snf_k5` reverts to `csv` if it does not beat it, and
`bias_correct` to `FALSE` if it worsens downstream case-skill.

**Legacy.** `architecture = "lstm_v1_legacy"` preserves the frozen v0.33
shared-LSTM path (random split + sequential fine-tuning) for
reproducibility/rollback.

# MOSAIC 0.33.4

* `run_rolling_cv()` now scores all four model types (ensemble, optimized, best, medoid); added `models` and `n_reps_best_medoid` knobs to `compile_rolling_cv_predictions()`.

# MOSAIC 0.33.3

* `run_rolling_cv()`: enabled the `run_MOSAIC` best-subset optimizer per cutoff.

# MOSAIC 0.33.2

* Added `evaluate_rolling_cv()` for post-hoc out-of-sample forecast scoring.

# MOSAIC 0.33.1

* `run_rolling_cv()`: always refit psi per cutoff — removed the `refit_psi` flag.

# MOSAIC 0.33.0

* `est_suitability()` v0.33 AI-data production spec: v7.3 feature set, `target_C` response, and per-country calibration.

# MOSAIC 0.32.9

## Phase 3 (#101): cast observed surveillance matrices to double before scatter

End-to-end Dask smoke run surfaced a silent reticulate serialization bug
that returned `-Inf` for every sim's likelihood. Root cause:
`get_location_config(iso = "ETH")` (and every other location's config)
ships `reported_cases` / `reported_deaths` as R **integer** matrices —
counts are natively integer-valued — with `NA_integer_` for missing
surveillance weeks. When `reticulate::r_to_py()` serializes an integer
matrix to numpy, `NA_integer_` becomes `INT32_MIN` (`-2147483648`) on
the worker side, because numpy `int32` has no NaN representation. Those
huge negative sentinels then evaluate as valid (extreme) counts in
`calc_model_likelihood`'s NB term, returning `-Inf` for every sim.

The smoke run's parquet read `likelihood: -Inf` for all 5000 sims; the
gather adapter's multi-iter collapse then turned 5 copies of `-Inf`
into `NA_real_` (no finite values to log-mean-exp), so `samples.parquet`
showed `% finite: 0`.

### Fix

`.mosaic_inject_likelihood_settings()` now casts `config$reported_cases`
and `config$reported_deaths` to `storage.mode = "double"` before scatter.
`storage.mode()` preserves matrix `dim` and only flips the underlying
type; `NA_integer_` → `NA_real_` → `np.nan` via reticulate, which
`calc_model_likelihood` then masks correctly via `np.isfinite()`.

The cast lives in the Dask-path-specific helper so the local-path
R-side `calc_model_likelihood` is unaffected — it handles
`NA_integer_` and `NA_real_` identically via `is.finite()` masking.

### Validation

1-sim ETH smoke against a Coiled cluster (1 D4s_v6 worker, freshly
rebuilt v0.32.7 image with laser-cholera 0.13.1):
- Before fix: `likelihood: -Inf, is finite: FALSE`
- After fix: `likelihood: -17454.14, is finite: TRUE`

The end-to-end on-worker scoring path is now confirmed functional.
Production smoke (5K sims) and the 1K-sim performance benchmark
remain to be re-run.

---

# MOSAIC 0.32.8

## Phase 3 (#101): swap removed MOZ fixtures for global `config_default` in parity test

`tests/testthat/test-dask_worker_schema_parity.R` loaded `config_default_MOZ`
and `priors_default_MOZ`, both removed in v0.30.49. All three parity tests
were SKIPping with "config_default_MOZ / priors_default_MOZ not available"
since the merge, leaving Phase 3's worker-schema regression coverage inert.

Switched the fixture loader (renamed `skip_if_no_moz_data()` →
`skip_if_no_data()`) to use the global multi-country `config_default` /
`priors_default`. Because the parity tests are R-side flattening only — no
LASER simulation — the ~40-location config is cheap to exercise and gives
strictly broader coverage of the ISO-suffix invariant than the single-
country MOZ fixture would: TEST 2's per-location loop now runs ~40
assertions instead of 1, validating that every SSA ISO suffix survives
the worker round trip. All three tests now PASS (was: 3 SKIPs).

Docker post-fix: `test_file()` reports `[ FAIL 0 | SKIP 0 | PASS 42 ]`.

---

# MOSAIC 0.32.7

## azure/Dockerfile: harden Python-env build against silent failures

The previous `:latest` push silently produced a broken image: the `r-mosaic`
virtualenv was never created (only reticulate's default `r-reticulate`
conda env with just `numpy`), and `laser-cholera` wasn't installed at all.
Diagnosis revealed two latent bugs in [azure/Dockerfile](azure/Dockerfile)
that combined to mask the failure:

1. **Failure-masking shell chain in the python-deps step.** The single
   trailing semicolon (`; rm -rf ...`) before the final `echo` short-circuited
   the `&&` chain — if `MOSAIC::install_dependencies()` or any chained pip
   command failed, the `rm -rf && echo "..."` tail still exited 0, so the
   build looked green. **Fix**: all conjunctions now `&&`; `conda clean`
   wrapped in `(... || true)` since its `2>/dev/null` redirect intentionally
   ignores warnings but should not fail the build.

2. **Verify step too lenient.** `MOSAIC::check_dependencies()` prints
   warnings rather than erroring on missing modules, so a half-installed
   venv (no laser-cholera) passed the previous `tryCatch` guard.
   **Fix**: prepend three hard assertions before the R-side check —
   `test -x /root/.virtualenvs/r-mosaic/bin/python`, then a `python -c
   "import laser.cholera; from laser.cholera import calc_model_likelihood"`
   one-liner that fails the build if either import is missing. Confirms
   the venv exists AND the v0.13+ analyzer submodule is reachable.

3. **`RETICULATE_PYTHON` pinned at image level.** The container only had
   `ENV PATH=/root/.virtualenvs/r-mosaic/bin:$PATH`. reticulate's
   discovery doesn't always honor PATH — `Rscript` started outside the
   MOSAIC `.onLoad` hook fell back to a uv-bootstrapped ephemeral
   Python at `/root/.cache/R/reticulate/uv/...`, completely bypassing
   the installed venv. This caused `devtools::test()`'s pre-`library()`
   setup phase to report laser-cholera as missing and SKIP 11 tests
   that depend on the engine being importable. **Fix**: add
   `ENV RETICULATE_PYTHON=/root/.virtualenvs/r-mosaic/bin/python` so
   every R session in the image — MOSAIC-loaded or not — sees the
   right Python.

### Migration

Next image rebuild via `docker build -f azure/Dockerfile ...`
(use `--no-cache` to evict the now-stale layers, or just retag the
freshly-rebuilt image). Existing pulled images are unaffected until
re-pull.

---

# MOSAIC 0.32.6

## Phase 3 (#101): test fixes for post-merge contract updates

Three test failures surfaced by the first post-merge `devtools::test()`
docker run, all from cross-PR-merge interactions rather than behavior
regressions:

- **`test-config_default.R::"make_LASER_config validation: unknown iso_code..."`** —
  v0.32.0 promoted the warning to a hard error (per the v0.13+
  `epidemic_peaks ⊆ location_name` assertion at
  [R/make_LASER_config.R:918](R/make_LASER_config.R#L918)), but the test
  was still asserting `expect_warning`. Switched to `expect_error` and
  retitled the test to reflect the new contract.

- **`test-run_MOSAIC_resume.R::".mosaic_likelihood_provenance reports R engine..."`** —
  this is the test John wrote alongside the v0.32.4 forward hook
  (`# scoring is R-side on all paths until phase 3`). v0.32.5 activated
  the hook, so the assertion has to flip: `use_dask = TRUE` now returns
  `engine = "python"` with `impl_version` carrying the laser-cholera
  engine version. Test now asserts both branches of the helper
  explicitly.

- **`test-run_MOSAIC_resume.R::".mosaic_resume_check_inputs rejects a different likelihood provenance"`** —
  the third sub-check wrote a hard-coded `pkg_laser_cholera = "0.13.0"`
  into the fixture. On docker images whose installed laser-cholera lags
  `inst/python/environment.yml` (e.g., a `:latest` tag that pre-dates
  the v0.32.0 engine pin), the deaths-scale engine-version guard fires
  before the likelihood-provenance check the test was trying to
  exercise. Fixed by querying the live engine version via
  `importlib.metadata.version("laser-cholera")` (the same call the
  production guard uses at
  [R/run_MOSAIC_helpers.R:1015-1020](R/run_MOSAIC_helpers.R#L1015-L1020))
  and writing that into the fixture, so the engine guard is satisfied
  regardless of which container the suite runs in.

## rho (care-seeking) prior re-derived: Wiens 2-stratum RE pool

`R/get_rho_care_seeking_params.R` now anchors the rho prior on a random-effects meta-analytic pool of **both** Wiens et al. 2025 case-definition strata, replacing the prior pool of 12 GEMS Nasrin 2013 pediatric MSD strata + 1 Wiens severe/cholera summary.

**Anchors (Wiens et al. 2025, PMC12013865):**
- General diarrhea: 29.9% [25.3, 35.1] from 122 observations
- Severe diarrhea + cholera: 58.6% [39.9, 75.2] from 22 observations

**Pooled prior:** `Beta(5.38, 7.10)`, mean **0.423**, 95% CI [0.21, 0.65], ESS ≈ 12.5 (was: `Beta(6.81, 17.89)`, mean 0.276, 95% CI [0.12, 0.46], ESS ≈ 25).

**Why this change:**
1. **Severity match.** Symptomatic cholera spans the full mild-to-severe spectrum. Anchoring on the severe-only stratum biases rho upward (severe-only is dominated by outbreak-response settings); anchoring on general diarrhea alone biases rho downward (includes many self-resolving mild episodes). Pooling both honestly reflects the severity distribution.
2. **GEMS already included via Wiens.** The Wiens 2025 dataset includes 6 GEMS-derived Study IDs covering all 7 GEMS sites. The prior GEMS+Wiens pool double-counted the same source populations.
3. **Dimensional fix.** The prior pool weighted 12 GEMS stratum-level observations against 1 Wiens meta-analytic summary — upside-down vs Wiens's 23-study underlying meta-analysis. The new pool treats each Wiens stratum (122 + 22 obs) as one observation in the meta-analysis.
4. **Cascading mu_j_baseline update.** The steady-state CFR identity `mu_j_baseline = CFR × rho / (rho_deaths × chi)` propagates the rho change: per-country `mu_j_baseline` values rescale by ~1.5× (0.423 / 0.276). Implied per-symptomatic-episode CFRs for high-N test countries remain within the [0.5%, 15%] plausibility range: MOZ 3.5%, ETH 9.3%, KEN 10.1%, COD 14.9%.

**Files updated:**
- `R/get_rho_care_seeking_params.R` — new derivation with RE pool of two Wiens strata
- `R/plot_rho_care_seeking_params.R` — diagnostic figure updated to show both strata
- `model/input/param_rho_care_seeking.csv` — regenerated Beta parameters
- `data/priors_default.rda` + `inst/extdata/priors_default.json` — v15.8 (rho + all per-country mu_j_baseline cascaded through the CFR identity)
- `data/config_default.rda` + `inst/extdata/config_default.json` — v3.8 (rho + per-country mu_j_baseline updated surgically; full make_config_default.R build deferred due to pre-existing unrelated psi_jt validation issue)
- `data-raw/make_priors_default.R`, `data-raw/make_config_default.R` — header comments and hardcoded values updated

**Geographic note:** the Wiens severe+cholera stratum draws from 23 underlying studies (16 SSA + 3 LAC + 2 Bangladesh + 1 MENA + 1 South Asia non-BGD); 70% SSA-dominant. Wiens's geographic provenance was validated against the raw extractions at github.com/wienslab/diarrhea-careseeking.

**Test suite:** 1881 PASS / 0 FAIL / 6 SKIP (unchanged from v0.32.5).

---

# MOSAIC 0.32.5

## Phase 3 (#101): on-worker likelihood scoring on the Dask path

Workers on the Dask/Coiled path now compute the likelihood on-worker
immediately after each LASER iteration and return a compact
`{iter, seed_iter, likelihood}` dict plus a sim-level `params` echo,
instead of returning full per-iter time-series matrices for the R
orchestrator to score serially. The post-gather serial bottleneck
(~40 minutes on a 24K-sim predictive batch) collapses to seconds; at
47-country scale the per-sim payload drops from ~3.4 MB to a few
hundred bytes.

This release activates the `.mosaic_likelihood_provenance()` forward
hook introduced in v0.32.4: on the Dask path the helper now stamps
`engine = "python"`, and the resume guard automatically rejects pooling
Python-scored shards with R-scored shards from earlier runs. Depends on
`laser-cholera >= 0.13` (pinned in v0.32.0).

### Changed

- **`R/run_MOSAIC.R`** — adds a Dask preflight that rejects
  `control$io$save_simresults = TRUE`. The new worker schema no
  longer returns the raw (j, t) time-series the simresults writer
  needs. The local path is unaffected.

- **`R/run_MOSAIC_helpers.R`** — `.mosaic_run_batch_dask()` collapses
  its two R-side scoring branches (parallel PSOCK + sequential
  fallback, ~420 lines) into a single ~95-line gather-and-write
  loop. The new loop reads the per-iter scalar `likelihood` directly
  from the worker dict and re-injects two base-config fields stripped
  by `.extract_sampled_params()` before flattening: `location_name`
  (drives ISO column suffixes — without this `convert_config_to_matrix()`
  falls back to numeric suffixes like `beta_j0_tot_1` instead of
  `beta_j0_tot_ETH`) and `N_j_initial` (per-location initial
  population). Multi-iter likelihoods collapse via `calc_log_mean_exp()`
  exactly as the local path does (mirrors `run_MOSAIC.R` ~285-300).
  `.mosaic_likelihood_provenance()` now returns `engine = "python"`
  when `use_dask = TRUE`.

- **`inst/python/mosaic_dask_worker.py`** — `run_laser_sim()` return
  shape flips per the issue #101 contract. Per-iter entries change
  from `{j, seed, reported_cases, reported_deaths}` (nested lists of
  doubles) to `{iter, seed_iter, likelihood}` (three scalars). A new
  top-level `params` key carries the sampled scalars/vectors echoed
  from the JSON-deserialized `sampled` dict, minus `_MATRIX_FIELDS`.
  A defensive `getattr(model, "log_likelihood", None)` shim returns a
  clean per-sim failure (`success = False`, actionable error message)
  when the worker imports an engine older than laser-cholera 0.13.
  `run_laser_postca()` is unchanged — the post-calibration ensemble
  path still needs trajectories, not likelihoods.

### Tests

- **New `tests/testthat/test-dask_worker_schema_parity.R`** —
  engine-free regression tests that simulate the worker round trip
  in pure R (no Dask cluster, no Python), so they run in every CI
  configuration. Three cases:
  1. Column-name parity between the local-path
     `convert_config_to_matrix(params_sim)` and the Dask-path
     `convert_config_to_matrix(reconstituted)`, compared against the
     canonical `param_names_all` schema
     (`convert_config_to_matrix() minus seed`).
  2. ISO-suffix presence on every location — locks in that vector
     parameters land as `beta_j0_tot_ETH`, not `beta_j0_tot_1`.
  3. Failure-mode lock-in — drops the `location_name` re-injection
     and asserts numeric suffixes appear, so a future refactor that
     removes the injection trips a clear failure pointing back here.

- **`tests/testthat/test-dask_local_cluster_integration.R`** —
  schema assertions updated to the new contract:
  - New `skip_if_no_log_likelihood()` helper submits a sentinel sim
    and skips when the worker fails-by-design on engines `< 0.13`.
  - Test 3 (`client$submit`) asserts the result has `params` plus
    `iterations` of `{iter, seed_iter, likelihood}`; `location_name`
    is intentionally absent from `res$params`.
  - The `likelihood` finiteness check is relaxed to a numeric-type
    check — finiteness depends on Phase 1's
    `.mosaic_inject_likelihood_settings()` flattening the analyzer's
    input keys, which this fixture deliberately bypasses.
  - Test 4 (`client$map`) gated on the same skip helper.

### Migration

Calibrations that previously set `control$io$save_simresults = TRUE`
together with `dask_spec` now error early. Use the local backend for
diagnostic runs; `save_simresults` on the local path is unchanged.

### Cross-references

- Parent: [laser-cholera#47](https://github.com/InstituteforDiseaseModeling/laser-cholera/issues/47)
- This issue: [#101](https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues/101)
- Phase 1 (v0.30.1–v0.30.3): [#100](https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues/100)
- Phase 2 / laser-cholera v0.13.0: [laser-cholera#58](https://github.com/InstituteforDiseaseModeling/laser-cholera/issues/58)
- Resume hand-off (v0.32.4): the provenance guard now flips
  automatically on the Dask path with this release.

## laser-cholera v0.13.0 → v0.13.1

`inst/python/environment.yml` wheel URL bumped to v0.13.1. The release's only behavioral change is in the Python `calc_model_likelihood` module — peak rows whose `peak_date` falls outside `[date_start, date_stop]` are now dropped before index assignment instead of being clamped by `np.argmin` to t=0 or t=n-1. The Python port is now in alignment with the R-side in-window filter at `R/calc_model_likelihood.R:147-150, 423-426, 485-488`.

**No MOSAIC production code change required.** MOSAIC imports `laser.cholera.metapop.model` (the simulation engine) but never calls the Python likelihood in calibration or ensemble code paths — only in the parity test.

**Parity test gains.** The previously documented peak-term divergences in `tests/testthat/test-calc_model_likelihood_python_parity.R` are now closed:
- Test #4 (daily cadence, peak timing + magnitude): was R = -495 / Python = -606 (~22%); now both = -527.79 within `1e-4`.
- Test #5 (weekly cadence, peak timing): was R = -282 / Python = -1101 (~290%); now both = -282.32 within `1e-4`.

The skips for these two tests are removed (parity-test PASS count: 4 → 6). The two remaining skips (test #6 NA-masking in WIS, test #7 zero-prediction penalty scaling) are unrelated to v0.13.1's fix and remain pending an upstream port.

# MOSAIC 0.32.1

## CFR pipeline reworked for v0.13+ schema; WHO data refreshed through 2025

### rho_deaths variant: production pin to informative Beta(36.95, 51.02)

`rho_deaths` uses the **informative** variant Beta(36.95, 51.02) (mean 0.42, sd 0.052, 95% CI [0.32, 0.52]) for production calibrations. This is the SYNTHESIS_REPORT §3.3 pooled-mean-CI fit. The wider prediction-interval variant Beta(6.30, 8.52) is retained for sensitivity analysis only. **Rationale:** within a single country, the deaths likelihood identifies the product `mu_j_baseline * rho_deaths`, leaving a flat factorization direction. Pinning `rho_deaths` near 0.42 lets `mu_j_baseline` posteriors carry the cross-country CFR signal cleanly. All three meta-analysis anchor studies (Routh 2017, Shikanga 2009, Bwire 2013) are SSA outbreak settings — the regime MOSAIC calibrates — so the pooled-mean precision is the operative target.

### CFR parameter tracking — closing the gaps

Parameter tracking surfaces were updated for consistency with the v0.13+ CFR pipeline:

- `R/plot_model_parameters.R` location_params_base now includes `mu_j_baseline`, `mu_j_slope`, `mu_j_epidemic_factor`, `epidemic_threshold`, `beta_j0_tot`, `psi_star_*` — these were silently dropped from posterior visualization despite being sampled per-country.
- `R/get_param_names.R` replaces the stale `mu_j` placeholder with the actual sampled triple (`mu_j_baseline`, `mu_j_slope`, `mu_j_epidemic_factor`) plus `epidemic_threshold` and the `delta_reporting_*` integers.
- `R/calc_model_ess_parameter.R` docstring example updated from `mu_j` to `mu_j_baseline`.
- `R/run_MOSAIC.R` docstring example flags updated from `sample_mu_j` to `sample_mu_j_baseline`.

The mu_j_baseline derivation in `data-raw/make_priors_default.R` is corrected for the laser-cholera v0.13+ schema. The conversion factor from observed reported CFR to the engine's daily mortality hazard is now `rho / (rho_deaths * chi)` (~1.015) instead of `rho / chi` (~0.43). Per-country `mu_j_baseline` Gamma priors are ~2.36x their pre-v0.32.1 values, which corrects the pre-v0.13 under-scaling where mu_j_baseline implicitly absorbed `1/rho_deaths`. The conversion factor is now derived inline from the actual `rho`, `rho_deaths`, `chi_endemic`, `chi_epidemic` Beta priors so it stays in sync if those upstream priors are updated.

`make_config_default.R` now sources `mu_j_baseline` UNIVERSALLY from `priors_default` Gamma means (was: raw CFR `rowMeans(mu_jt)` with an ETH-only hand-patch). config_default and priors_default agree by construction; a new regression test (`test-cfr-pipeline-consistency.R`) asserts this invariant per country.

**MOZ-specific overrides dropped.** The MOZ `mu_j_baseline = Gamma(2, 1176)` and `mu_j_epidemic_factor = Gamma(1.5, 0.5)` overrides were both calibrated under the pre-v0.13 misspecified likelihood. They are superseded by the universal data-driven prior derived from the corrected steady-state identity. MOZ now inherits `mu_j_baseline ≈ 0.0045` (from its observed ~0.43% CFR × 1.015) and the global `mu_j_epidemic_factor ~ Gamma(1, 2)` default. If country-specific calibration evidence under the new schema warrants re-introducing an override, it should be derived from a v0.32+ calibration.

**WHO annual data refreshed through 2025.** `R/process_WHO_annual_data.R` was rewritten to (a) parse the actual `first_epiwk` / `last_epiwk` date ranges instead of hard-coding `year <- 2024`, (b) ingest ALL `cholera_adm0_public_*.csv` files in `raw/WHO/annual/who_global_dashboard/` and dedupe by `(iso, year)` keeping max coverage, (c) archive each ArcGIS dashboard download with a snapshot date instead of overwriting, and (d) add `coverage_days` and `year_fraction` columns so partial-year snapshots are clearly labeled. The 2024 and 2025 calendar-year CSVs (downloaded from the WHO Global Cholera & AWD Hub UI) are now in the pipeline alongside the rolling 2026 partial-year snapshot. The output CSV is renamed from `who_afro_annual_1949_2024.csv` to `who_afro_annual.csv` (year-agnostic canonical name); 6 consumers updated.

**MOSAIC-docs math spec updated.** `04-model-description.Rmd` describes the v0.13+ derivation identity, the sigma cancellation, and the reinterpretation of pre-v0.32.0 mu_j_baseline posteriors.

# MOSAIC 0.32.0

## Upgrade engine to laser-cholera v0.13.0 (deaths likelihood scale correction)

The Python engine is bumped from v0.12.5 to v0.13.0 (`inst/python/environment.yml`). The engine now emits a separate `model.results.reported_deaths` time series equal to `round(disease_deaths[t - delta_reporting_deaths] * rho_deaths)`, and MOSAIC scores observed surveillance `reported_deaths` against simulated `reported_deaths` — not against raw `disease_deaths`.

**Behavior change (deaths likelihood scale).** Before v0.32.0 the deaths NB likelihood compared observed reported deaths to simulated raw disease deaths, so the simulated series was on a ~1/rho_deaths ≈ 2.4× higher scale than the observations. Calibration absorbed the missing factor by inflating `mu_j_baseline` posteriors. The new schema fixes the scale; deaths bias-ratio diagnostics should now centre on 1.0. Resume from a pre-v0.32 run on v0.32+ is **not** safe — the `deaths` column in parquet shards has flipped semantics.

**Field rename surface.** Every R extraction of `model$results$disease_deaths` flipped to `reported_deaths`. The Dask worker dict (`inst/python/mosaic_dask_worker.py`) and the R-side gatherers now use `reported_deaths`. `plot_model_ppc`, `calc_model_ensemble`, the test fixtures, scenario scripts, and the vignette were updated in lockstep. The MOSAIC-Mozambique scenario scripts (peer repo) were updated too.

**Schema changes consumed.**

- v0.13.0 hard-asserts every `iso_code` in `epidemic_peaks` appears in `location_name`. `get_location_config()` and the Dask injection (`.mosaic_inject_likelihood_settings()`) now call `.filter_epidemic_peaks()` to drop foreign-iso and out-of-window rows. `make_LASER_config()` promotes its stale warning to an error when foreign iso_codes are present.
- v0.13.0 auto-computes `epidemic_peaks$loc_idx` from `iso_code` at config-load time; MOSAIC does not need to inject it.
- HDF5 paramfile loading was removed in v0.13.0; MOSAIC has always shipped JSON.
- The Python namespace flipped from `laser_cholera.*` to `laser.cholera.*` (this was actually pre-v0.13 but the test fixture and three scenario scripts still pointed at the old name).

**Prior updates.**

- `priors_default` v15.4 → v15.5:
  - `rho_deaths` switched from informative variant `Beta(36.95, 51.02)` to recommended `Beta(6.30, 8.52)` per `claude/rho_deaths_research/SYNTHESIS_REPORT.md` §3.4. Both share mean ~0.42, but the recommended variant fits the 95% prediction interval and accommodates between-study heterogeneity. The informative variant remains documented for sensitivity analysis.
  - `delta_reporting_deaths` description corrected: this parameter is the **death-event-to-death-report** delay, not symptom-onset-to-death-report. The symptom-onset-to-death interval is implicit in the SEIR dynamics (γ₁⁻¹ ~ 5–7 days) and is NOT folded into `delta_reporting_deaths`. Default 5 days, prior Truncnorm(mean=4, sd=3) unchanged.

**Resume guard.** The resume path compares the live `laser-cholera` version against the `python$pkg_laser_cholera` field already recorded in `1_inputs/environment.json` and **hard-errors when the persisted and current versions straddle the v0.12 → v0.13 deaths-scale boundary** (resuming would mix incompatible deaths scales in the on-disk shards). Two versions on the same side of the boundary warn but proceed.

**Parity tests.** A new `tests/testthat/test-calc_model_likelihood_python_parity.R` (4 PASS) validates R↔Python likelihood parity on the core NB, cumulative-progression, and WIS paths. Four parity tests are SKIPped with documented Python-side divergences (peak-term scoring, NA-masking in WIS, zero-prediction penalty) — issues to file upstream against laser-cholera.

**Known Python-side issues (filed for upstream fix):**

1. `np.round(disease_deaths * rho_deaths)` deterministic rounding biases low-incidence patches toward zero (ETH smoke shows ratio 0.13 vs rho_deaths=0.46). Should use `prng.binomial`.
2. Peak-term scoring diverges ~22% on daily cadence and ~290% on weekly cadence; only surfaces in tests when `loc_idx` is supplied (Python silently drops peak rows missing it).
3. WIS-path NaN propagation when observations contain NAs (~0.5% LL drift).
4. Zero-prediction penalty scaling differs ~1.78× between R and Python.

# MOSAIC 0.31.0

## Add `resume = TRUE` to `run_MOSAIC()` — continue an interrupted calibration

`run_MOSAIC()` gains a `resume` argument (default `FALSE`). When `TRUE`, the run reconstructs calibration state from the per-sim shards already present in `<dir_output>/2_calibration/samples/` and continues from the next unused `sim_id` instead of restarting from scratch. Because each simulation's parameters are a deterministic function of `seed = sim_id`, a resumed run is bit-identical to an uninterrupted one.

- The shards on disk are the source of truth. The next `sim_id` is always `max(id on disk) + 1`, so no draw is ever duplicated.
- An internal `2_calibration/state/resume_checkpoint.rds` (written every batch) restores the adaptive ESS/phase state exactly; runs without a checkpoint bootstrap the state from the shards.
- `resume = TRUE` is rejected with `clean_output = TRUE`; when the run already completed (a consolidated `samples.parquet` the shards would shrink); and when the supplied `config`, `priors`, `control$likelihood`, `control$sampling`, `control$calibration$n_iterations`, or the calibration mode (auto vs fixed) differ from those persisted in `1_inputs/` (each changes the draws or likelihood and would make the pooled shards incomparable — a hard error). In adaptive mode resume continues from `max(sim_id)+1` and does not backfill interior gaps; fixed mode re-runs any missing id to hit the target. Corrupt/invalid shards are read-validated and quarantined; resumed runs are recorded in `summary.json` (`resumed`, `n_simulations_reused`). Each run also stamps a likelihood-value provenance (`likelihood_provenance` in `environment.json`); resume refuses to pool shards scored by a different likelihood engine/implementation — a forward hook for the Dask/Coiled worker-side scoring port (laser-cholera issue #47), which flips the engine from R `calc_model_likelihood` to on-worker Python.
- `resume = FALSE` (the default) is byte-identical to prior behavior. The slim `run_state.json` monitoring file is unchanged.

# MOSAIC 0.30.49

## Remove `config_default_MOZ` / `priors_default_MOZ` (BREAKING for direct users)

The `config_default_MOZ` and `priors_default_MOZ` data objects and their `data-raw` builders are removed from the package. The objects were never exported in `NAMESPACE`, never documented in `man/`, and never consumed by production R code; their only callers were three integration test files (CRAN-skipped) and the MOSAIC-Mozambique calibration project deliberately does not depend on them — it builds its own MOZ artifacts from scratch via `MOSAIC::make_LASER_config()` + cherry-picked entries from the *global* `MOSAIC::priors_default`.

The parallel `_MOZ` build pipeline was a recurring source of silent drift: it had to be hand-maintained whenever the global `make_config_default.R` / `make_priors_default.R` changed, and the v0.30.47 `rho_deaths` switch caught one such miss (the .rda kept the old 0.6 value while the JSON was updated to 0.42).

**Removed files:**

- `data/config_default_MOZ.rda`
- `data/priors_default_MOZ.rda`
- `inst/extdata/config_default_MOZ.json`
- `inst/extdata/priors_default_MOZ.json`
- `data-raw/make_config_default_MOZ.R`
- `data-raw/make_priors_default_MOZ.R`
- `tests/testthat/test-dask_local_cluster_integration.R` (only consumer; depended entirely on the fixture)
- 4 integration tests at the end of `tests/testthat/test-ic_moment_match.R` (the deterministic-math + guard-clause tests stay; the integration tests that needed a single-location config fixture are removed)

**Migration:** if you were loading `MOSAIC::config_default_MOZ` for ad-hoc experiments, the MOSAIC-Mozambique project (`MOSAIC-Mozambique/code/R/make_config_MOZ.R`) is the canonical way to build a Mozambique calibration config from the package's global priors.

# MOSAIC 0.30.48

## Audit pass: ic_moment_match latent bug + metadata + docs

Deep-review sweep across v0.30.33 → v0.30.47 surfaced three notable issues; this release fixes them.

- **`moment_match_E_I` two latent bugs (R/sample_parameters.R).**
  The optional initial-conditions moment match (default `ic_moment_match = FALSE`) had two bugs that did not surface in production but broke the unit tests:
  - **Vector reshape direction.** When `reported_cases` arrives as a length-T vector (e.g. single-location tests), `as.matrix(vec)` returned a T×1 column matrix; the function then read `obs_mat[j, ]` and got just element [1]. Production code always passes an n×T matrix so the branch was never hit there. Fixed by reshaping vectors as `matrix(vec, nrow = 1)` (single-location row).
  - **Guard clause NA propagation.** When `gamma_1` (or `iota` / `sigma`) was `NULL` in the config, `is.finite(NULL) || NULL <= 0` yielded `NA`, and the enclosing `if(...)` failed with "missing value where TRUE/FALSE needed." Rewritten with `isTRUE(is.finite(x) && x > 0)` so the guard coerces NA/NULL to FALSE and falls back cleanly to the prior IC.
  Three unit-test expectations in `test-ic_moment_match.R` that pinned the old (pre-v0.30.40) `E = I * iota` formula were also updated to the corrected `E = Isym * gamma_1 / (sigma * iota)` formula.
- **MOZ `config_default_MOZ` `rho_deaths` artifact missed in v0.30.47.** `data/config_default_MOZ.rda` still carried the old `rho_deaths = 0.6` even though the JSON sidecar had been updated to 0.42. Both are now consistent at 0.42, with the MOZ config metadata bumped to v2.8 and the rationale linked.
- **`config_default` metadata bumped (v3.4 → v3.5).** The global config metadata description was extended to reference the v0.30.47 `rho_deaths` switch; previously it referenced v3.4 (the beta_j0_tot per-iso seeding) as the most recent change.
- **`R/est_suitability.R` missing `@param exclude_covariates`.** The function signature has accepted `exclude_covariates = character(0)` for ablation studies for some time, but the parameter was not roxygen-documented. Added the tag; `man/est_suitability.Rd` regenerated.
- **`NEWS.md` backfill for v0.30.41–v0.30.47.** Six version entries were missing; added below.

# MOSAIC 0.30.47

## rho_deaths informative prior from SSA meta-analysis

The `rho_deaths` default prior is replaced (`Beta(3, 2)` → `Beta(36.95, 51.02)`) and the default value drops from 0.6 to 0.42.

- **Methodology.** Random-effects (DerSimonian-Laird, logit-scale) meta-analysis of three Sub-Saharan African studies — Routh 2017 (Tanzania, EID; binomial CI from 48/101), Shikanga 2009 (Kenya, AJTMH; approximate binomial CI from implied ~25/73), Bwire 2013 (Uganda, PLOS NTDs; sensitivity range). Inverse-variance weighting yields Routh 53%, Shikanga 42%, Bwire 5% — Bwire's wide CI auto-down-weights it. Pooled mean 0.419 (essentially identical to the earlier ad-hoc 3:2 quality-weighted mean 0.417).
- **Informative variant.** Beta fit to the 95% CI of the pooled mean (not the prediction interval) gives `Beta(36.95, 51.02)`, mean 0.420, 95% CI [0.319, 0.524], ESS ~88.
- **Methodology mirrors `R/get_rho_care_seeking_params.R`** (cases-side `rho`), establishing parallel statistical machinery for the two observation-model detection probabilities.
- **Previous attribution to Finger et al. 2024 corrected** — that paper is an editorial without a quantitative anchor; the 23–96% range cited belonged to Pampaka et al. 2025 and described place of death, not surveillance capture. Full provenance: `claude/rho_deaths_research/SYNTHESIS_REPORT.md`.

# MOSAIC 0.30.46

## `config_default`: phase-switching params seeded from per-iso priors

`epidemic_threshold` and `mu_j_epidemic_factor` in the global `config_default` are now sourced PER-COUNTRY from `priors_default$parameters_location` rather than flat values (formerly `1/10000` and `1.25`). The per-iso prior means span roughly 7e-7 to 8.7e-6 for `epidemic_threshold` and 0.5 to 3.0 for `mu_j_epidemic_factor`, restoring the epidemiological heterogeneity required for the engine's phase-switching to actually engage.

# MOSAIC 0.30.45

## `config_default`: initial conditions seeded from per-iso priors

`S_j_initial`, `E_j_initial`, `I_j_initial`, `R_j_initial`, `V1_j_initial`, `V2_j_initial` are now seeded from each country's `prop_*_initial` Beta priors with Hamilton apportionment, rather than the old flat `S = 80%` / `R = 50%` defaults. The flat default had `R_eff = 0.72 < 1` at many ISOs, blocking transmission before the calibration loop could begin.

# MOSAIC 0.30.44

## Docs: mislabeled params, phantom function refs, stale value claims

Audit-driven cleanup of roxygen documentation across `R/priors_default.R`, `R/config_default.R`, `R/config_simulation_endemic.R`, and `R/make_LASER_config.R`: corrected 19 mislabeled `@param` fields, removed phantom `\link{}` references to functions that no longer exist, and resynced value claims in roxygen text with the actual data shipped in `data/priors_default.rda`. Man pages regenerated.

# MOSAIC 0.30.43

## `epidemic_peaks`: filter at build time + warn on bad configs

The 129-row `epidemic_peaks` table shipped with `config_default` is now filtered at build time to `[date_start, date_stop]` (47 rows after filtering). The 82 out-of-window rows were silently snapping to `t = 1` / `t = N` in the peak-shape likelihood terms and bloating the JSON. New helper `filter_epidemic_peaks()` (`R/filter_epidemic_peaks.R`) is also called from `make_LASER_config()` to warn whenever a user-supplied config carries peaks outside the simulation window.

# MOSAIC 0.30.42

## `write_json_or_gz`: peer-arg API + JSON rename to match .rda

`R/write_json_with_optional_gz.R` is replaced by `R/write_json_or_gz.R` with a clearer peer-arg API (`write_json = TRUE` / `write_gz = TRUE` controlled independently). The four `inst/extdata` JSON sidecars are renamed to match the `.rda` they correspond to: `default_parameters*.json` → `config_default*.json`, `simulated_parameters.json` → `config_simulation_epidemic.json`, `sim_endemic_parameters.json` → `config_simulation_endemic.json`.

# MOSAIC 0.30.41

## `inst/extdata`: opt-in .gz sidecar for production configs only

The `.json.gz` sidecars that previously shipped automatically alongside every `inst/extdata/*.json` are now opt-in. They were redundant for the simulation configs (`config_simulation_*`) which are small and never read from disk by performance-sensitive code; production builds for `config_default` / `priors_default` still produce the `.gz` sidecar when explicitly requested via `write_gz = TRUE` in the data-raw scripts.

# MOSAIC 0.30.40

## Biology fixes from deep-review re-validation

Two findings from the disease-modeling deep review that survived
re-validation against MOSAIC-docs and project history:

- **moment_match_E_I steady-state formula corrected.** The optional
  `ic_moment_match` feature was using `E_count = I_count * iota`, which
  is dimensionally inconsistent (count x 1/day) and over-seeded E by
  ~5x at prior medians. The laser-cholera engine has
  `E -> Isym` at rate `sigma * iota` per E and `Isym -> R` at rate
  `gamma_1`, so the steady-state balance is
  `E = Isym * gamma_1 / (sigma * iota)`. At prior medians
  (`gamma_1 = 0.1/d`, `sigma = 0.24`, `iota = 0.71/d`) the corrected
  E/Isym ratio is ~0.59 instead of ~0.71. The `I_count` variable was
  renamed `Isym_count` internally to reflect that the reporting chain
  (`cases = Isym * rho * chi_endemic`) only observes the symptomatic
  compartment. Flag still defaults to FALSE; this fixes the formula
  for any caller who turns it on.
- **epsilon prior 2x variance-inflation removed.** A
  variance-inflation step at `data-raw/make_priors_default.R:127-128`
  was doubling the sd of the natural-immunity waning rate from 2.0e-4
  to 4.0e-4, pushing the 95% CI on immunity duration to roughly
  [1.9, 53] years -- the upper-tail value is biologically
  implausible and the inflation had no documented rationale. With
  the inflation removed, sd is back to 2.0e-4 (95% CI on rate
  [1.7e-4, 1.03e-3], corresponding immunity duration CI ~[2.7,
  16] years) -- the range supported by the cited King et al. 2008
  and the project's two-cohort re-fit (~7 yr mean). `priors_default`
  metadata version bumped to 15.2 and the .rda re-built.

# MOSAIC 0.30.39

## Engineering review sweep (v0.30.29 – v0.30.38)

Series of focused fixes from a five-agent deep-review of the package.
No model-behavior changes; package hygiene, correctness on edge cases,
and consistency with the documented design.

- **v0.30.29** — Add `rlang` to `DESCRIPTION` Imports (`NAMESPACE` had
  `import(rlang)` but the dep was missing, producing an R CMD check
  ERROR). Declare column names referenced by recent plot edits in
  `R/globals.R` to silence "no visible binding" NOTEs.
- **v0.30.30** — Repo hygiene: remove tracked zero-byte `=` file,
  delete stray `MOSAIC.Rcheck/`, `..Rcheck/`, `.Rd2pdf*/`, root
  `Rplots.pdf`, and `model/input/*.bak{,_*}` on disk. Expand
  `.Rbuildignore` (vm/, CLAUDE.md, .Rcheck/, =, .RData, .Rhistory, …)
  and `.gitignore` (*.bak, .Rd2pdf*/).
- **v0.30.31** — `data-raw/make_*` builders patched only `.json`
  outputs (`grepl("\\.json$")`) so the `.json.gz` siblings were
  shipping without `zeta_ratio` and `decay_days_spread`, breaking the
  downstream `zeta_2 = zeta_1 / zeta_ratio` derivation. Switch regex
  to `\\.json(\\.gz)?$`, read via `read_json_to_list()`, write via
  `write_list_to_json()` with the right compress flag. Regenerated
  all four `.json.gz` artifacts so each pair is now content-identical.
- **v0.30.32** — `get_default_config()` and `get_default_LASER_config()`
  hardcoded `file.path(PATHS\$ROOT, "MOSAIC-pkg", ...)` -- only works on
  a developer checkout. Switch to `system.file("extdata", ...,
  package = "MOSAIC")`; `PATHS` retained for backward compatibility.
- **v0.30.33** — Introduce `.mosaic_set_all_thread_env(n)` as a single
  source of truth for the six canonical thread-env vars documented in
  CLAUDE.md (`OMP_NUM_THREADS`, `MKL_NUM_THREADS`, `OPENBLAS_NUM_THREADS`,
  `NUMEXPR_NUM_THREADS`, `TBB_NUM_THREADS`, `NUMBA_NUM_THREADS`). The
  fallback branch of `.mosaic_set_blas_threads` had been setting only
  3, `calc_model_ensemble.R` 5, `make_mosaic_cluster.R` 5 in two
  places. Route every site through the helper.
- **v0.30.34** — `plot_vaccination_maps.R`: `ggsave(plot = print(...))`
  was saving the invisible return value and double-printing to the
  active device. Replaced with `plot = final_combined_plot`.
- **v0.30.35** — `calc_model_likelihood()` peak-shape terms:
  `which.min(abs(date_seq - peak_date))` always returns an in-range
  index, so peaks whose date fell outside `[date_start, date_stop]`
  were silently snapped to t=1 / t=N, biasing the peak-timing and
  peak-magnitude likelihoods. Filter `peak_date` against
  `[date_seq[1], date_seq[N]]` before snapping, in all three sites
  (main path + two legacy helpers).
- **v0.30.36** — `calc_model_ensemble()` weighted mean was biased
  toward zero under simulation failures: `sum(values * w,
  na.rm=TRUE)` drops the NA term but does not redistribute its
  weight mass. Filter valid then divide by sum of surviving weights,
  matching the behavior of `weighted_quantiles()`.
- **v0.30.37** — `props_to_counts()` integer-rounding fallback could
  leave a compartment negative or sum off by 1 in edge cases.
  Replace per-compartment `round()` + residual-on-S with Hamilton
  (largest-remainder) apportionment in a single per-location pass;
  `stopifnot()` asserts sum == N and all counts >= 0.
- **v0.30.38** — `moment_match_E_I()` was location-invariant due to
  `unlist(reported_cases)` flattening the matrix; every country got
  identical initial infections. Compute first-week-of-positives
  window per location row. (E_count = I_count * iota formula left
  unchanged with an inline note for the next biology pass; flag is
  OFF by default.)
- **v0.30.39** — Update `epidemic_peaks` docstring to match actual
  code defaults (28-day smoothing, 10-day comparison, 75-day
  separation, 8% prominence) and backfill NEWS for the
  v0.30.27/0.30.28 entries that were silently missed.

# MOSAIC 0.30.28

## `calc_model_likelihood`: source `epidemic_peaks` from config

When the caller supplies `config\$epidemic_peaks` (a data.frame with
`iso_code` + `peak_date` columns), the likelihood now prefers that
over the lazy-loaded `MOSAIC::epidemic_peaks` package dataset. This
keeps the R likelihood aligned with the Python port at
`laser_cholera.metapop.calc_model_likelihood`, which reads the same
field from its config dict. The legacy helpers
`calc_multi_peak_timing_ll` and `calc_multi_peak_magnitude_ll` now
accept an optional `epidemic_peaks=` argument with the same fallback.

# MOSAIC 0.30.27

## Ship `epidemic_peaks` in `config_default` for the Python port

`config_default\$epidemic_peaks` is now populated from
`MOSAIC::epidemic_peaks` at build time and written into both the
`.rda` and the `default_parameters.json{,.gz}` artifacts. This is the
counterpart to a parallel change in laser-cholera that consumes
`config["epidemic_peaks"]` instead of requiring the R package to be
loaded. Configs built before this version remain readable: missing
`epidemic_peaks` falls back to the lazy-loaded package dataset.

# MOSAIC 0.30.26

## `compile_suitability_data()` — remove three dormant blocks

Three blocks of derived covariates were being computed and persisted to
`cholera_country_weekly_suitability_data.csv` even though
`est_suitability()`'s `covariates_all` list intentionally excludes
every one of them. Computing and saving them was wasted IO + a foot-gun
for any future contributor who re-enables them without realising why
they were excluded. Removed entirely from the function:

- **Vaccination block (3 columns)**: `vaccination_rate_daily`,
  `cumulative_vaccine_doses`, `log1p_cum_vaccine_doses`. Vaccination is
  a downstream intervention -- vaccines are deployed in response to
  cholera, not in anticipation of environmental suitability -- so
  these are non-causal predictors for the suitability model.
- **Epidemic-memory block (8 columns)**: `r_t`, `cum_cases_4w/8w/12w/52w`,
  `nonzero_ratio_52w`, `weeks_since_major_outbreak`, `peak_cases_52w`,
  `seasonal_outbreak_risk`, plus the `epidemic_week` alias. All
  case-derived; predicting a case-derived target (`cases_binary`)
  from any function of `cases` is target leakage even with the
  forecast-safe `memory_lag`.
- **Spatial-import block (7 columns)**: `import_vulnerability`,
  `export_potential`, `weighted_import_connectivity`,
  `connectivity_degree`, `betweenness_centrality`,
  `eigenvector_centrality`, `import_export_balance`. Mobility-network
  features that were computed but excluded from the LSTM input set.

Net: 18 columns dropped from the saved suitability CSV. The columns
were already excluded from the LSTM covariate list so this is purely
hygiene -- no model behavior changes. Compile wall-time should drop
slightly (no vaccination merge + cumulative-doses computation, no
epidemic-memory `slide_dbl()` loops, no mobility-matrix calculations).

The `forecast_mode` argument is now mostly cosmetic (controls summary
messages and a small bit of climate-anomaly leading-edge behavior).
Kept for backward compatibility.

---

# MOSAIC 0.30.25

## Flood-prob GAM: select=TRUE shrinkage + precip-window tensor product

Two changes validated independently by a software-engineer agent and a
statistical-modeler agent running parallel experiments in separate
sandboxes. Both converged on (1); the modeler additionally identified
(2):

1. **Replace the iterative p-value pruning loop with a single bam() fit
   using `select = TRUE`.** Smoothness null-space shrinkage is mgcv's
   principled mechanism for driving uninformative smooths' coefficients
   toward zero. The v0.30.24 in-sample-p-value pruning was multicollinearity-sensitive
   (the docstring already documented this caveat) and produced
   fragile drop sequences. select=TRUE delivers equivalent CV AUC
   (0.851 -> 0.852 in the SWE experiments) with a +2.6 to +4.2
   percentile-point lift on the worst-detected cyclone (Kenneth 2019,
   from 71.6 to 74.2 in production after this change). Removes the
   prune_p_threshold and prune_max_iter args + the pruning_log
   diagnostic. Single ~10-second fit replaces the iterative ~30-60s
   loop.
2. **Replace `s(precip_sum_4w) + s(precip_sum_24w)` with
   `te(precip_sum_4w, precip_sum_24w, k = c(10, 10))`.** The "heavy
   4-week rain ON an already-saturated catchment (6-month antecedent)"
   interaction is genuinely jointly nonlinear; the tensor captures it
   where the univariates plus the existing `precip_x_soil_anom`
   approximated it crudely. Modeler-agent experiment: cyclone median
   percentile rank 91.6 -> 93.8 (+2.2), top-5\% hits 3 -> 4.

End-to-end production cyclone benchmark (re-measured):

  Idai     MOZ 2019-W11   pctile = (run pending)
  Idai     MWI 2019-W11
  Idai     ZWE 2019-W11
  Kenneth  MOZ 2019-W17
  Eloise   MOZ 2021-W04
  Ana      MOZ 2022-W04
  Ana      MWI 2022-W04
  Gombe    MOZ 2022-W11
  Freddy-1 MOZ 2023-W08
  Freddy-2 MOZ 2023-W11

Tested but rejected by both agents: nthreads (no-op on macOS), method =
REML/ML/GCV.Cp (no metric change or impractically slow), `bs = "ad"`
adaptive smoothers, `k = 20+` on precip windows (overfit), `bs = "cr"`
cubic regression (no change), per-country `by = iso_code_f` smooths
(overfit), nested random effects, cloglog link, quasibinomial,
betabinomial, Tweedie on log1p(affected), smoothed +/-2w binary
target, class weighting (trades AUC for cyclone recall).

Caveat surfaced by both agents: country-stratified 5-fold CV AUC drops
to 0.72 (vs 0.85 with random folds). Most of the model's lift comes
from the country random effect; climate predictors are doing less
work than random-fold CV suggests. The model would fail badly on a
country with no training data. Worth keeping in mind when interpreting
the imputed flood_prob in any out-of-sample country setting.

---

# MOSAIC 0.30.24

## `impute_flood_probability()` gains iterative p-value pruning loop

After each GAM fit, the term (smooth or parametric) with the highest
in-sample p-value above \code{prune_p_threshold} (default 0.05) is
dropped and the GAM is refit. The cycle repeats until every remaining
prunable term is significant, fewer than 5 prunable terms remain, or
\code{prune_max_iter} (default 30) iterations are reached. The country
random-effect smooth and the region-conditional
\code{s(precip_sum_4w, by = region_f)} block are treated as structural
and never dropped. The trace is written to
\code{flood_gam_pruning_log.csv} in the diagnostics directory.

Pruning behavior on the v0.30.24 production run dropped 5 precip-block
terms whose contribution was absorbed by their collinear neighbors:
\code{s(precipitation_sum)} (p=0.74), \code{s(precip_sum_12w)}
(p=0.65), \code{s(precip_anom)} (p=0.37),
\code{precip_extreme_p90_count} (p=0.21), \code{s(precip_sum_2w)}
(p=0.18). The model retains \code{precip_sum_4w/8w/24w/52w}, the
region-conditional smooth, plus soil-moisture / SPEI / humidity / wind
/ ENSO / IOD terms.

End-to-end cyclone benchmark (median percentile rank across 10
catastrophic cyclones in MOZ/MWI/ZWE) is unchanged at 91.1 vs 93.0
pre-pruning. Idai loses ~1.3 pts in MOZ/MWI/ZWE; Ana and Freddy gain
0.3-2.1 pts. Top-5\% hits stay at 3 of 10. Median seasonal-variance
share unchanged at 0.398.

Caveat: in-sample p-values are not predictive-importance tests and are
sensitive to multicollinearity. The loop produces a smaller, cleaner
model whose remaining smooths are all in-sample significant; verify
against the cyclone benchmark + rolling-year CV after major covariate
changes. Set \code{prune_p_threshold = 1.0} to disable pruning
entirely (recovers v0.30.23 behavior).

Wall-time impact: pipeline runs in ~69 sec (vs ~41 sec for v0.30.23),
the extra 28 sec covering the 5 additional GAM fits the pruning loop
performs.

---

# MOSAIC 0.30.23

## Flood-prob imputer: revert to enriched binomial after cyclone benchmark

The v0.30.22 Tweedie-on-severity formulation made Cyclone Freddy stand
out as MOZ's #1 historical week (the headline visual goal) but a
10-cyclone benchmark across Idai 2019 (MOZ/MWI/ZWE), Kenneth 2019,
Eloise 2021, Ana 2022 (MOZ/MWI), Gombe 2022, and Freddy 2023 (x2 landfalls)
showed the Tweedie variant under-detects other catastrophic events --
median percentile rank 81.4, minimum 77.8. The root cause: the Tweedie
target chases EM-DAT's logged Total Affected, and Idai's Total Affected
was logged smaller than its real impact warranted, so the model
under-amplified it.

This release reverts to the family + target of v0.30.21 (binomial logit
on emdat_flood_active 0/1) while keeping the four physics-driven
predictor enrichments that the v0.30.22 S2 exploration found
load-bearing:

- `s(precip_sum_24w)` -- ~6-month basin saturation memory (inline-computed)
- `s(precip_sum_52w)` -- annual antecedent precipitation (inline-computed)
- `s(precip_sum_4w, by = region_f)` -- region-conditional 4-week precip smooth
- `s(precip_x_soil_anom)` -- joint precip-anomaly x soil-moisture-anomaly index

Result: median cyclone percentile rank 93.2 (vs 81.4 for Tweedie, 90.8
for v0.30.21 baseline), minimum 79.1 (every cyclone-week in the top 21%
of its country's history). Three cyclones in the top 5%.

| Method | Median cyclone pctile | Min | Top 5% hits / 10 |
|---|---|---|---|
| **v0.30.23 (enriched binomial)** | **93.2** | **79.1** | **3** |
| v0.30.22 (Tweedie hybrid) | 81.4 | 77.8 | 2 |
| v0.30.21 (baseline binomial) | 90.8 | 69.4 | 3 |

Trade-off vs v0.30.22: Freddy's headline rank in MOZ drops back from
#1 to top-5%, but every other catastrophic cyclone is more reliably
detected. The right call for cholera forecasting where we care about
all flood events, not just one outlier.

Output is now naturally on [0, 1] from the logit link -- no global-max
scaling needed. Test thresholds restored to the binomial-appropriate
AUC > 0.80 and mean(hi)-mean(lo) > 0.3 discrimination tests.

---

# MOSAIC 0.30.22

## Flood-prob imputer: Tweedie severity target + enriched predictors

The v0.30.20 binomial-on-active formulation produced an imputed
`emdat_flood_prob` that didn't separate major events from seasonal
climatology -- Cyclone Freddy (MOZ, Feb-Mar 2023) only reached
probability 0.503, barely above the MOZ Feb-Apr p90 of 0.396 and below
the all-time MOZ max. Four parallel strategies were explored (Tweedie
severity target; enriched predictor set; xgboost; two-stage hurdle).
This release adopts the hybrid of the two winners:

**Tweedie family on raw `Total Affected`.** Encodes flood SEVERITY
rather than just presence/absence. The Tweedie compound-Poisson-gamma
mixture handles the zero-inflated continuous target natively.

**Enriched predictor set** (computed inline by the imputer, no
upstream compile changes required beyond the existing `region`
column):

- `precip_sum_24w` and `precip_sum_52w` -- long-memory antecedent
  precipitation capturing basin saturation. These were the largest
  single contributors to interannual variance in the rebuild
  experiments.
- `s(precip_sum_4w, by = region_f)` -- region-conditional 4-week
  precipitation smooth (4-region WHO classification: Central / East /
  Southern / West Africa).
- `s(precip_x_soil_anom)` -- joint precip-anomaly x soil-moisture-anomaly
  index for the "very-wet AND already-saturated" regime.

**Output normalization**: predictions on the raw Tweedie scale are
non-negative, heavy-tailed continuous. Normalized by global maximum to
`[0, 1]` -- rank-normalization was tested and rejected because it
erases the long-tail major-event amplification the Tweedie target was
chosen for.

**End-to-end measured impact** (full AFRO panel, real climate + EM-DAT
data):

| Metric | v0.30.21 (binomial) | v0.30.22 (Tweedie hybrid) |
|---|---|---|
| Cyclone Freddy rank in MOZ history | not top-5 | **1st of 872** |
| Freddy / MOZ Feb-Apr p90 ratio | 1.27x | **8.14x** |
| Median seasonal variance share | 0.436 | **0.162** |
| OOT Spearman vs severity | 0.290 | 0.267 |
| OOT AUC vs binary | 0.857 | 0.808 |

Cost: 0.05 AUC and 0.02 Spearman on the held-out binary-target metrics
(unavoidable when switching from binomial-on-binary to
Tweedie-on-continuous). Acceptable trade-off given the 6.4x lift on the
Freddy benchmark and 63% reduction in seasonal-variance share.

**Strategies tested and rejected**: xgboost (sharper peaks but
worsened seasonal share); two-stage hurdle (Freddy peak actually
*dropped* because Total Affected for Freddy isn't a global outlier).

---

# MOSAIC 0.30.21

## Two correctness fixes in the flood-prob pipeline

Surfaced by an internal review of the EM-DAT integration:

- **Gate / GAM-contract drift fixed.** The `if (include_flood_prob && ...)`
  gate in `compile_suitability_data()` was still checking the v0.30.19-era
  predictors (`temp_anom`, `elevation`, `urban_population_pct`) which were
  removed from the GAM in v0.30.20, and was failing to check the 9 new
  columns the rebuilt GAM actually requires (`precipitation_sum`,
  `precip_sum_2w/4w/8w`, `precip_extreme_p90_count`,
  `soil_moisture_0_to_10cm_mean`, `relative_humidity_2m_mean`,
  `rh_mean_12w`, `wind_speed_10m_max`, `ENSO3`, `ENSO4`). The result: a
  missing required column would have caused a hard `stop()` deep inside
  `impute_flood_probability()` instead of being skipped cleanly by the
  `else` branch. Both `impute_flood_probability()` and the gate now
  source the canonical required-column list from a single internal
  helper `.impute_flood_probability_required()`, eliminating the drift
  surface entirely.
- **`emdat_flood_prob_anom` historical baseline fixed.** The per-country
  mean used as the anomaly baseline was computed over the full series
  including forecast-window rows, whose probabilities are themselves
  GAM-extrapolated. This silently leaked a small amount of forecast
  information into the historical-period anomaly used as an LSTM training
  feature. The baseline now uses only rows where
  `emdat_flood_active` is non-NA (the historical EM-DAT panel coverage).

---

# MOSAIC 0.30.20

## Flood-probability GAM: rebuild around hydrological drivers + ENSO/IOD

Variance decomposition of v0.30.19's imputed `emdat_flood_prob` showed
the highest-flood-activity countries (NGA 92%, TCD 83%, SSD 83%, MLI 75%,
ETH 75%) were heavily dominated by seasonal pattern, and `summary(model)`
revealed that the cyclic-week and country-RE terms plus the raw 12-week
rainfall sum carried virtually all the explanatory power while every
anomaly/teleconnection term was shrunk by the `select = TRUE` penalty.
This release rebuilds the GAM formula around variables that are
ostensibly linked with flood physics, and removes the seasonal /
static-baseline channels that were crowding out the climate-driven
interannual signal:

- **Removed** `s(week, bs = "cc")` (cyclic seasonality), `elevation`, and
  `urban_population_pct`. The country random effect alone absorbs the
  remaining country-level baseline differences.
- **Removed** `s(temp_anom)` — temperature isn't a direct flood driver
  in AFRO (no snowmelt).
- **Added every precipitation channel**: `precipitation_sum`,
  `precip_anom`, `precip_sum_2w/4w/8w/12w`, plus the binary
  `precip_extreme_p90_count` extreme-rain trigger.
- **Added soil moisture (raw + anomaly)**, atmospheric humidity (raw +
  12w rolling), and the storm/cyclone proxy `wind_speed_10m_max`.
- **Heavy ENSO/IOD bench**: ENSO34 at current + 8/16/24-week lags,
  current ENSO3 and ENSO4 (Eastern and Western Pacific regions), IOD at
  current + 8/16/24-week lags. Lag-N columns are computed inline.
- **Removed `select = TRUE`** — the smooth-shrinkage penalty was the
  mechanism that suppressed exactly the teleconnection terms we now
  want to emphasise. Without it the smooths keep their unpenalized REML
  weight; rolling-year CV is the arbiter.

---

# MOSAIC 0.30.19

## Code-review fixes in `compile_suitability_data` and `est_suitability`

Round of correctness fixes surfaced by an internal code review:

- **B1 — `est_suitability()` date auto-detection** referenced `d_all$date_start` / `enso_complete$date_stop`, columns that `compile_suitability_data()` drops before saving. `min(NULL, na.rm=TRUE)` silently returns `Inf` and propagated as bogus dates. Fixed to use `d_all$date`.
- **B2 — Fine-tuning splits trained against the wrong target scale.** The model output head is `activation = "linear", name = "logit_head"` and the first split correctly fed `qlogis()`-transformed targets. Subsequent fine-tuning splits (default `n_splits = 10`) fed raw `[0, 1]` probabilities, pushing the linear head toward the wrong range and silently undoing the first split's training. Now applies `qlogis(pmax(eps, pmin(1-eps, ...)))` consistently.
- **B3 — `seasonal_outbreak_risk`** computed `mean(cases_lagged)` inside `group_by(iso_code) %>% mutate(...)`, producing a single per-country mean rather than the week-of-year climatology the comment claimed. Now uses `stats::ave(cases_lagged, week, FUN = mean)` to give the intended weekly seasonality.
- **B4 — `weeks_since_major_outbreak`** incremented its counter through the `memory_lag` warm-up window (default 17 weeks in forecast mode), producing fake `1, 2, 3, ...` values when the lag target was NA. Now stays NA until the first real observation.
- **B5 — ISO week 53.** Investigated and documented. Two distinct things were happening: (a) legitimate W53 in 2015 and 2020 was being dropped (~80 country-weeks, 0.2% of training data); (b) an upstream bug in `process_open_meteo_data` emits spurious W53 entries labelled by calendar year for ISO years 2005/2010/2016/2021. The drop-all-W53 filter robustly handles (b). Added a comment block explaining the trade-off; the upstream fix to `process_open_meteo_data` is the structural follow-up (separate ticket).
- **B6 — Vaccination weekly key** paired `lubridate::isoweek(date)` with `format(date, "%Y")` (calendar year). Dates near the year boundary (e.g., 2024-12-30, which is ISO 2025-W01) were joining under `(year=2024, week=1)`, silently missing the right upstream row. Now uses `lubridate::isoyear()`.
- **B7 — `pivot_wider` for climate and ENSO** had no `values_fn`, so any residual duplicate `(iso_code, year, week, variable)` row would silently produce list-columns and break every downstream merge. Now passes `values_fn = mean`.
- **B8 — `est_suitability()` plotting block** was gated by `if (T)` (always on) with a comment claiming "disabled by default", and `par(mfrow = c(2,1))` leaked into the caller's device state. Replaced with a real `plot_country_diagnostics = FALSE` argument and `on.exit(par(old_par))`.
- **B9 — Positional column renames** (`names(x)[3] <- "..."`) replaced with `dplyr::rename()` so an upstream column reorder can't silently corrupt downstream values.
- **B10 — Negative case counts** in `est_suitability()` are now flagged with a warning before being clamped to 0 (previously silent).
- **B11 — Duplicate identical `df <- data.frame(...)` block** in `est_suitability()` removed.
- **B12 — Dead first `covariates_all` assignment** in `est_suitability()` removed; only the comprehensive (live) assignment remains.

---

# MOSAIC 0.30.18

## Speed up flood-prob GAM and clean the suitability covariate list

- `impute_flood_probability()` now fits with `mgcv::bam(method = "fREML", discrete = TRUE)`
  rather than `mgcv::gam(method = "REML")`. Same formula, essentially identical
  fitted values; wall-time on the real ~30k-row training set drops from ~13 min
  to ~10 sec. Rolling-year CV is also capped at the 3 most recent fully-observed
  years (previously open-ended). End-to-end pipeline now runs in ~12 sec instead
  of ~100 min.
- `est_suitability()` covariate list cleaned: removed `log1p_cum_vaccine_doses`
  (vaccination intervention — correlated with outbreaks because deployment
  follows cholera, so including it teaches a non-causal shortcut that breaks at
  forecast time) and the raw `year` / `month` / `week` timestamps (let the model
  memorise period-specific cholera waves; the sin/cos cyclic terms already in
  the list carry seasonality cleanly). Added a comment block to `covariates_all`
  documenting the rationale.
- End-to-end validation against the real data: mean rolling-year CV AUC 0.848
  on 2023/2024/2025 held out; forecast-window `emdat_flood_prob` distribution
  is shape-similar to historical (mean 0.095 vs 0.062), confirming the GAM
  uses climate signal rather than collapsing to a constant.

---

# MOSAIC 0.30.17

## Impute country-week flood probability for use in `est_suitability()`

EM-DAT is observed-only — for forecast-window weeks, the raw `emdat_flood_active`
binary added in v0.30.16 is identically zero, creating a distribution shift
between training and inference for the suitability LSTM.

This release replaces the raw binary with a continuous **imputed flood
probability** that is populated identically in historical and forecast
weeks:

- **New: `impute_flood_probability()`** — fits a binomial GAM
  (`mgcv::gam`, logit link, REML, `select = TRUE`) on observed
  `emdat_flood_active` events using climate anomalies, cyclic seasonality,
  country random effects, and inline-computed `ENSO34_lag20` /
  `IOD_lag16` predictors. Predicts a non-NA `emdat_flood_prob ∈ [0, 1]`
  for every (iso × week) row. With `diagnostics = TRUE` (default) writes
  smooth-term plots, decile-calibration plot, rolling-year CV metrics
  CSV, and `summary(model)` to `<PATHS$DOCS_FIGURES>/flood_imputation/`.
  Warns if mean CV AUC < 0.65.
- **`compile_suitability_data()`** gains `include_flood_prob = TRUE`
  (default). When on, calls `impute_flood_probability()` after the
  existing NA-imputation block and adds four rolling-window aggregates:
  `emdat_flood_prob_4w_max`, `emdat_flood_prob_12w_max`,
  `emdat_flood_prob_12w_sum`, `emdat_flood_prob_anom`. The previous
  zero-fill on the raw EM-DAT join is removed so forecast-window rows
  remain NA for the GAM to impute.
- **`est_suitability()`** consumes the five `emdat_flood_prob*` columns
  in place of the four raw `emdat_flood_*` columns added in v0.30.16.
  The raw columns remain in the suitability dataframe for audit but are
  not fed to the LSTM.

No new dependencies (`mgcv` was already an Imports dep).

---

# MOSAIC 0.30.6

## Add GitHub profile links for Dejan Lukacevic and Meikang Wu

`_pkgdown.yml` now wires Dejan to `DLukacevic-IDM` and Meikang to `MeWu-IDM`. All five contributors in the sidebar and authors page now have working GitHub profile links.

---

# MOSAIC 0.30.5

## Show all contributors in pkgdown sidebar; fix Christopher's broken GitHub link

Two pkgdown configuration fixes for the authors page and Developers sidebar:

1. **Sidebar shows all five roles, not just `aut` + `cre`.** Added `authors.sidebar.roles: [aut, cre, ctb]` to `_pkgdown.yml`. Previously the right-hand "Developers" block on the home page listed only John (aut, cre) and Christopher (aut, ctb); Tony, Dejan, and Meikang (all `ctb` only) were hidden. They now all appear.
2. **Christopher's GitHub link was 404.** `_pkgdown.yml` had `https://github.com/ChristopherWLorton` (no such user); the actual handle is `clorton`. Fixed.
3. **Tony's GitHub link added.** `https://github.com/tinghf`.

John's `gilesjohnr` link was already correct.

---

# MOSAIC 0.30.3

## Sync `rho_deaths` into MOZ data-raw, JSON sidecars, and simulation configs

Cleanup pass after independent review of v0.30.2 surfaced three medium-severity gaps left over from the targeted `.rda` rebuild. v0.30.2 rebuilt the top-level `.rda` binaries surgically but left several auxiliary data-raw scripts and `inst/extdata` JSON sidecars out of sync — re-running any of them would have regressed the `rho_deaths` plumbing. This release closes those gaps so all source-of-truth artifacts agree:

- **MOZ data-raw scripts.** `data-raw/make_priors_default_MOZ.R` adds the Beta(3, 2) `rho_deaths` prior block (metadata bumped 3.0 → 3.1); `data-raw/make_config_default_MOZ.R` adds `rho_deaths = 0.6` (metadata bumped 2.5 → 2.6).
- **LASER config templates.** `data-raw/make_default_LASER_config_files.R`, `make_simulation_endemic_LASER_config_files.R`, and `make_simulation_epidemic_LASER_config_files.R` add `rho_deaths = 0.6`.
- **`inst/extdata/` JSON sidecars.** All six (`default_parameters{,_MOZ}.json`, `sim_endemic_parameters.json`, `simulated_parameters.json`, `priors_default{,_MOZ}.json`) regenerated with `rho_deaths`; `.gz` mirrors refreshed. The two priors JSONs also bumped to versions 15.1 and 3.1.
- **Simulation `.rda` binaries.** `config_simulation_endemic.rda` and `config_simulation_epidemic.rda` rebuilt with `rho_deaths = 0.6`.
- **Version metadata sync.** `priors_default_MOZ.rda` had inherited the wrong version (15.1) during the v0.30.2 surgical rebuild; corrected to the MOZ-track 3.1 to match the source script.

Refs #100. Phase 1 of the `calc_model_likelihood` Python port (laser-cholera#47) is now complete and consistent across all source-of-truth artifacts.

---

# MOSAIC 0.30.2

## Add `rho_deaths` plumbing for death detection model

Adds the MOSAIC-side wiring for the `rho_deaths` parameter introduced by [laser-cholera#49](https://github.com/InstituteforDiseaseModeling/laser-cholera/issues/49) (death detection probability, analogous to `rho` for cases). Folded into the Phase 1 paramfile work (issue #100 addendum): laser-cholera 0.12.x's permissive validator silently tolerates the new key in the paramfile; once 0.13 ships with #49, the engine consumes it and produces `reported_deaths`.

- **Defaults.** `data-raw/make_config_default.R` sets `rho_deaths = 0.6` (mean of Beta(3, 2); Finger et al. 2024 documents ~60% surveillance capture of true cholera deaths). `data-raw/make_priors_default.R` adds the Beta(3, 2) prior block; `priors_default` metadata version bumped 15.0 → 15.1.
- **Targeted `.rda` rebuild** for `config_default`, `config_default_MOZ`, `priors_default`, `priors_default_MOZ`. Avoids re-running the full data-raw pipeline (which depends on paths and external data files).
- **Sampling control.** `mosaic_control_defaults()$sampling$sample_rho_deaths = TRUE` in `R/run_MOSAIC.R`. `R/sample_parameters.R` adds `sample_rho_deaths = TRUE` to defaults; includes `rho_deaths` in `validate_sampled_config` global params; included in the `disease_only` disabled-flag-resolution list alongside `sample_rho`.
- **`make_LASER_config()`.** New `rho_deaths = NULL` argument with `[0, 1]` validation when supplied; pass-through to params; `@param` documented.
- **Round-trip schema.** `R/convert_config_to_matrix.R`, `R/convert_config_to_dataframe.R`, `R/get_param_names.R`, `R/plot_model_parameters.R` include `rho_deaths` in the relevant params/keep lists so it appears in `samples.parquet` and posterior plots.
- **Test coverage.** New `tests/testthat/test-sample_parameters_rho_deaths.R` confirms the prior exists and shapes are Beta(3, 2); sampling under `sample_rho_deaths = TRUE` produces draws in (0, 1); `sample_rho_deaths = FALSE` holds `rho_deaths` at `config_default`; empirical mean ≈ 0.6 over 200 draws. Tests skip cleanly when MOSAIC root or rebuilt prior is unavailable.

Refs #100, laser-cholera#49.

---

# MOSAIC 0.30.1

## Wire likelihood control + `epidemic_peaks` into Dask paramfile

Phase 1 of the `calc_model_likelihood` Python port ([laser-cholera#47](https://github.com/InstituteforDiseaseModeling/laser-cholera/issues/47), tracked in issue #100). Additive wiring on the Dask path so the laser-cholera analyzer can score on-worker once Phase 2 (laser-cholera 0.13) and Phase 3 (Dask worker schema flip, MOSAIC-pkg #101) land. Local PSOCK/FORK path is unchanged.

- **Inject helper.** New `.mosaic_inject_likelihood_settings()` in `R/run_MOSAIC.R` flattens 12 likelihood-control keys (`weight_cases`, `weight_deaths`, `weights_time` (= `.weights_time_resolved`), `weights_location`, `nb_k_min_cases`, `nb_k_min_deaths`, `weight_peak_timing`, `weight_peak_magnitude`, `weight_cumulative_total`, `weight_wis`, `sigma_peak_time`, `sigma_peak_log`) plus `epidemic_peaks` (trimmed to `iso_code`/`peak_date`) plus `calc_likelihood = TRUE` onto config.
- **Dask preflight.** `run_MOSAIC()` calls the inject helper before `.extract_base_config()`, so the keys ride along on the scattered `base_config` and land on `model.params` for the analyzer to read. Uses `.weights_time_resolved` (the rescaled-to-sum-`n_t` vector), not the raw `control$likelihood$weights_time`.
- **Keep list extension.** `R/run_MOSAIC_helpers.R::.extract_base_config()` now keeps the 13 new keys plus `calc_likelihood`. Local-path configs (no inject) keep none of them by construction — the keep filter is membership-based.
- **Defensive guard.** `.mosaic_run_simulation_worker()` explicitly sets `params_sim$calc_likelihood = FALSE` before the LASER call so a stray user-set `TRUE` on the local path can't trigger the analyzer with un-flattened keys.
- **Unit tests.** New `tests/testthat/test-inject_likelihood_settings.R` covers the inject helper (forwards `.weights_time_resolved`, additive only, NULL handling) and the `.extract_base_config` keep list (presence on Dask path, absence on local). 14 assertions, all green.

Refs #100.

---

# MOSAIC 0.30.0

## Route per-location prediction CSVs to `3_results/predictions/`

Previously `plot_model_ensemble(save_predictions = TRUE)` wrote per-location CSVs (`predictions_<type>_<LOC>.csv`) to its `output_dir`, which `run_MOSAIC()` sets to `3_results/figures/predictions/` — mixing data files into a tree meant for figures. The combining step then wrote `predictions_<type>_all.csv` into `3_results/predictions/`, a byte-identical duplicate when the run covers a single location.

This release:

1. **Separate data/figures trees.** Added `data_dir` argument to `plot_model_ensemble()`. When provided, per-location CSVs are written there instead of `output_dir`. `run_MOSAIC()` passes `data_dir = dirs$res_predictions` at all three call sites (best, medoid, ensemble). `figures/predictions/` now holds PDFs only.
2. **Combining step reads from `predictions/`** (the new canonical location) instead of `figures/predictions/`. Regex excludes `*_all.csv` so the combine pass ignores its own output.
3. **Single-location runs no longer emit `_all.csv`.** When only one per-location CSV exists, the canonical file is the per-location CSV itself — no `file.copy()` to a byte-identical `_all.csv`. `_all.csv` is written only for multi-location runs where there's genuine concatenation happening.
4. **`plot_model_ppc()`** now reads from `dirs$res_predictions` rather than `dirs$res_fig_pred` (auto-discovery still works; the function's docstring example updated to match).

Backwards-compatibility: `plot_model_ensemble()` defaults `data_dir = NULL`, which falls back to `output_dir` — external callers of the function keep working unchanged.

---

# MOSAIC 0.29.9

## Prior/posterior panels: draw mode vlines for every method in every panel

Dashed central-tendency lines in `distributions_*_Prior_Posterior.pdf` panels now:

1. Mark the **mode** of the plotted density (argmax of the displayed curve) rather than the mean (v0.29.4 and earlier) or median (v0.29.8). On a log-scale axis the mode corresponds to `exp(meanlog)` for a lognormal (= median of X, the visible peak of the log10-density bell). On a linear axis the mode corresponds to the classical mode of X (e.g. `(s1-1)/(s1+s2-2)` for a Beta with `s1>1` and `s2>1`). Computed directly from `x[which.max(y)]` on the grid, so it always aligns with the visible peak regardless of axis.
2. Draw for **every method** in every panel, not just the ones whose center falls inside the data range. The previous `line_val >= x_range[1] && line_val <= x_range[2]` gate occasionally suppressed a line when the mean of a skewed distribution fell outside the plotted support. Lines are drawn unconditionally now — ggplot extends the axis if the mode sits outside the density's effective range.

Near-delta distributions demoted to `fixed_values` vlines (v0.29.8) still render as solid lines at the median, so the prior/posterior marks remain visually distinct when one method is effectively a point estimate.

---

# MOSAIC 0.29.8

## Fix vertical-line alignment, near-delta posteriors, and add beta_j0_tot to log scale

Three follow-ups to the log-scale posterior plotting in v0.29.6/0.29.7:

1. **Mean vertical lines were misaligned with the visible peak on log-x.** For a wide lognormal the arithmetic mean `exp(meanlog + sdlog²/2)` sits many decades to the right of the density peak on the log10 axis (the peak is at the median `exp(meanlog)`). For zeta_ratio prior the mean was ~15,000× to the right of the visible peak. Fix: for log-scale params the dashed central-tendency line is now drawn at the median (`qbeta(0.5, ...)` for Beta, `exp(meanlog)` for lognormal). Linear-axis params still use the mean as before.

2. **prop_E_initial / prop_I_initial prior curves collapsed to flat lines.** The posterior Beta(62199, 6e11) has a 0.008-decade effective support — a near-delta distribution whose peak density on log10-axis is ~600× larger than the prior's. On shared y-axis the wider prior rendered as a flat line. Fix: distributions with `log10(q_0.99) - log10(q_0.01) < 0.05` on log-scale axes are demoted to a solid vertical line at the median (matching the `fixed_values` styling), letting the wider prior/posterior set the y-axis scale.

3. **Added beta_j0_tot to the log-scale list.** `beta_j0_tot` is lognormal(meanlog=-10.8, sdlog=2.0), a 3.4-decade span — a natural log-scale candidate, missed in v0.29.6.

---

# MOSAIC 0.29.7

## Fix collapsed posteriors on log-scale prior/posterior plots

The v0.29.6 log-scale switch in `plot_model_distributions()` plotted `dlnorm(x, ...)` (density w.r.t. `x`) against a log-x axis. Linear-space densities for wide lognormals shrink with scale — e.g. the zeta_ratio posterior in MOZ_s1_explore_v2 has a peak `dlnorm` of 9.9e-7 vs the prior at 8.1 (7 orders of magnitude smaller). On a shared linear y-axis the posterior flattened to a line at zero.

Fix is the standard change of variables for density on a log axis: for `Y = log10(X)`,

`f_Y(log10(x)) = f_X(x) · x · ln(10)`

Applied to the lognormal and beta branches whenever the param is on log-scale. For lognormal, this is equivalent to `dnorm(log10(x), meanlog/ln(10), sdlog/ln(10))` — the same symmetric-bell convention used in `est_kappa_prior.R:281`.

After the fix, the zeta_1 and zeta_ratio posteriors for the MOZ run now render as visible curves rather than collapsed lines. Peak heights are comparable across prior/posterior (all in the 0.2–0.4 range for zeta_* on the log10 density scale) regardless of where mass sits on the x-axis.

---

# MOSAIC 0.29.6

## Log-scale x-axis for wide-span parameters in global prior/posterior plots

`plot_model_distributions()` renders prior vs. posterior densities for all global parameters into `distributions_global_Prior_Posterior.pdf`. Parameters with multi-order-of-magnitude support (lognormal `kappa`, `zeta_1`, `zeta_2`, `zeta_ratio`; heavily left-skewed Beta priors on `prop_E_initial`, `prop_I_initial`) previously rendered on a linear x-axis — the distribution looked like a spike at zero with an invisible right tail, wasting the panel and hiding the posterior shape.

This release switches those six panels to `scale_x_log10()` with `annotation_logticks(sides = "b")`, matching the styling already used in `est_kappa_prior.R:375` for the `kappa_prior.png` forest/density plots. Density grids for these params are now log-spaced (`exp(seq(log(x_min), log(x_max), ...))`) so the rendered curve is smooth across the full log range rather than linear-clumped at the right edge.

**Log-scale params:** `kappa`, `zeta_1`, `zeta_2`, `zeta_ratio`, `prop_E_initial`, `prop_I_initial`.

**Unchanged:** All Beta priors on [0,1] probabilities, truncnorm priors on bounded absolute ranges, and narrow-span lognormal/gamma priors still use linear x.

---

# MOSAIC 0.29.5

## Best/medoid prediction plots: independent `n_iter` and parallel execution

Best and medoid single-config prediction plots previously reused `ensemble_n_sims_per_param` (default 5, sequential) because they were refactored into `calc_model_ensemble()` in 0.29.2 without giving them their own iteration control. For tight stochastic CI envelopes on the best/medoid plots, users had to bump the ensemble count — paying the cost across all N posterior parameter sets.

This release splits the two:

* `control$predictions$n_iter_ensemble` (default **10L**) — stochastic runs per posterior parameter set in the weighted ensemble. Renamed from `ensemble_n_sims_per_param`.
* `control$predictions$n_iter_best` (default **100L**) — stochastic runs for the best and medoid single-config plots. Applied identically to both models.

Best/medoid now also run in parallel: `calc_model_ensemble()` is invoked with `parallel = control$parallel$enable` and `n_cores = control$parallel$n_cores` (previously hardcoded `parallel = FALSE`). Parallelization uses an internal PSOCK cluster — same infrastructure the posterior ensemble already uses in the non-Dask path. For Dask calibrations, the best/medoid step still uses local PSOCK since the Dask cluster is closed after the posterior-ensemble sims.

**Breaking rename:** `control$predictions$ensemble_n_sims_per_param` → `control$predictions$n_iter_ensemble`. User scripts (`vm/`, `azure/`, vignettes) updated accordingly. Existing scripts setting `best_model_n_sims` (previously an orphaned no-op control field) migrate to `n_iter_best` and are now wired in.

---

# MOSAIC 0.29.3

## Unified stochastic-median R² and bias across best, medoid, and ensemble

Follow-up to 0.29.2. The 0.29.2 fix routed the best and medoid prediction **plots** through `calc_model_ensemble()` + `plot_model_ensemble()` so the plot captions report R²/bias from the stochastic median. But the separate `"Best model R²"` / `"Medoid model R²"` **log lines** still came from a single deterministic `lc$run_model()` call, producing two different numbers for the same thing (log vs plot caption). Red-team review flagged the inconsistency.

This release eliminates the separate deterministic LASER calls for best/medoid and sources all three sets of reported metrics — best, medoid, ensemble — from the stochastic median of their respective `mosaic_ensemble` objects:

* `R/run_MOSAIC.R` best-model block reordered: `calc_model_ensemble(configs = list(config_best), ...)` is called once; R²/bias are computed from `best_ensemble$cases_median` / `$deaths_median` and passed to both the log line and downstream `summary.json`. Same refactor applied to the medoid block.
* Log format now: `"Best model R² (1 params x N stoch): cases = X (bias=Y), deaths = ..."` — mirrors the existing ensemble log format.
* Removes two per-run LASER calls (the single deterministic `best_model <-` and `medoid_model <-` runs) that are no longer needed; best/medoid each now run `n_ensemble_stochastic_per` LASER sims total (default 10), same as before the 0.29.2 fix *plus* plot but minus the deterministic R² helper run.
* Retires the `lc <- reticulate::import(...)` import in the main `run_MOSAIC` body — `calc_model_ensemble` handles the import internally.

No public API changes.

---

# MOSAIC 0.29.2

## Fix best/medoid prediction plots: `reported_cases`, stochastic CI, unified naming

`plot_model_fit()` (used by the best-model and medoid-model plots in `run_MOSAIC()`) rendered `model$results$expected_cases` — the back-calculated burden `new_symptomatic / rho`, typically 5-10x inflated vs surveillance-comparable cases. Every other part of the pipeline (likelihood, R², ensemble) used `model$results$reported_cases = Isym * rho / chi_eff`. The v0.14.22 commit that renamed `expected_cases` → `reported_cases` in sibling plotting functions missed this fourth file; the bug was latent until v0.22.15 re-wired `plot_model_fit()` into the best-model block, and real from v0.22.15 through v0.29.1.

**Fix consolidates best/medoid plots into the existing `calc_model_ensemble` + `plot_model_ensemble` pipeline** — the same functions the posterior ensemble uses — in single-config mode (one parameter set × N stochastic reruns). One codepath for all three plot types eliminates the parallel-implementation drift that caused the original bug.

**Changes:**

* `R/plot_model_ensemble.R`: new `file_prefix` (default `"ensemble"`) and `title_label` (default `"Posterior Ensemble"`) parameters; hardcoded filename and title strings replaced; single-param-set subtitle branch added.
* `R/run_MOSAIC.R`: best and medoid plot calls replaced with `calc_model_ensemble(configs = list(config_<...>), n_simulations_per_config = n_ensemble_stochastic_per, envelope_quantiles = c(0.025, 0.975))` + `plot_model_ensemble(file_prefix = "best" | "medoid", ...)`. Medoid output moved from `2_calibration/best_model/` to `3_results/figures/predictions/` alongside the ensemble plots. Explicit `file_prefix = "ensemble"` added at the main ensemble plot call for self-documentation.
* Prediction-CSV combining loop extended to iterate `c("ensemble", "best", "medoid", "stochastic")` and filename pattern renamed from `all_predictions_<type>.csv` → `predictions_<type>_all.csv` for consistency with the plot naming.
* `R/plot_model_fit.R`: deleted (retired). The function was the single source of the drift bug and had no internal callers after the refactor.

**Output naming** (all under `3_results/figures/predictions/` unless noted):

* `predictions_<prefix>_<LOC>.pdf` + `.csv` per-location (prefixes: `ensemble`, `best`, `medoid`)
* `predictions_<prefix>_cases_all.pdf`, `predictions_<prefix>_deaths_all.pdf` faceted multi-location overviews
* `predictions_<type>_all.csv` combined across locations (in `3_results/predictions/`)

**Lesson recorded** in `CLAUDE.md` (item 11): when renaming a field across sibling functions, grep exhaustively; do not skip temporarily-unused functions; prefer consolidating into one shared code path over maintaining N parallel implementations that must be updated in lockstep.

---

# MOSAIC 0.29.1

## Bias corrections + zeta_ratio channel switch (follow-up to 0.29.0)

Two changes landed together in this patch:

1. **`zeta_ratio` default switched from combined (C) to direct literature-anchor channel (A).** The combined precision-weighted fit at median 2.16e5 was pulled high by the derived-from-marginals channel (~1.15e6), overestimating the per-day asymptomatic:symptomatic shedding asymmetry vs modelling-convention + household-transmission evidence (Smith 2026 ~1.6x, Chao/Finger ~10). The direct channel is now what `make_priors_default.R` and `make_priors_default_MOZ.R` write into `priors_default$parameters_global$zeta_ratio`. The combined and derived fits remain available via `est_zeta_ratio_prior()$diagnostics$fit_combined` and `$fit_derived`.

2. **Bias corrections to zeta_1.** Code review identified several compounding upward biases in the v0.29.0 `zeta_1` fit. All are addressed here; `zeta_1` median drops from 3.72e11 to 1.39e11 (mode ÷66).

**Corrections applied:**
* `R/est_zeta_1_prior.R`: V_sev central value lowered from 8 L/day to 4 L/day (time-averaged over the 1-2 week clinical course rather than first-24-h peak rate from Harris 2012); V_mod 4 -> 2 L/day; V_mild 500 -> 300 mL/day. Mild concentration lowered from 10^6 to 10^5 cells/mL (non-rice-water stool). Nelson 2020, Kaper 1995, and Harris 2012 downweighted from 0.50 to 0.10 (reviews that cite the same Nelson-era primary data, not independent measurements). Endemic and outbreak severity-weighted pool rows given weight 0 (they are derived quantities of rows 1/4/5 and including them was triple-counting the severe class).
* `R/est_zeta_2_prior.R`: Kaper rows downweighted from 0.25 to 0.10 (review overlap).
* `R/est_zeta_ratio_prior.R` direct channel: Nelson 2009 paired weight 1.00 -> 0.30 (value 10^5 is a stool concentration ratio, not a per-day rate ratio - unit-inconsistent with zeta_ratio). Kaper and Harris paired rows 0.25 -> 0.10 (review overlap).

**Net prior shifts (main = MOZ):**
* `zeta_1`: LN(26.64, 1.69) -> LN(25.65, 2.46); median 3.72e11 -> 1.39e11; mode 2.15e10 -> **3.29e8** (the config point estimate uses the mode).
* `zeta_2`: LN(12.69, 2.00) -> LN(12.30, 2.00); median 3.23e5 -> 2.20e5.
* `zeta_ratio` direct channel: LN(6.64, 4.81) -> LN(4.31, 4.39); median 763 -> **74.7** (config point estimate uses median; mode is pathological for sdlog=4.39).

**config_default and config_default_MOZ placeholders updated** to reflect the new modes/medians.

**Test updates:** `tests/testthat/test-sample_parameters_zeta.R` coverage range for `zeta_1` widened from `(1e9, 1e14)` to `(1e8, 1e14)` to match the wider bias-corrected sdlog. All 18 zeta tests pass.

---

# MOSAIC 0.29.0

## Breaking changes (prior scale shift)

* **`zeta_1`, `zeta_2`, and `zeta_ratio` priors are re-estimated from a literature meta-analysis** (`R/est_zeta_1_prior.R`, `R/est_zeta_2_prior.R`, `R/est_zeta_ratio_prior.R`). The new priors encode the biological scale of *V. cholerae* shedding (cells per infected person per day) rather than the Frame-B LASER count-scale used by prior defaults. This is a ~6 order-of-magnitude upward shift on `zeta_1` (prior median moves from 70 000 to ~1e11-1e12 cells/person/day) and a corresponding re-centring of `zeta_ratio`. The previous defaults `LN(log(70 000), 0.8)` and `LN(log(300), 1.2)` are replaced by weighted-MLE lognormal fits on primary-source anchors (Nelson 2009, Merrell 2002, Harris 2012, Smith 2026 medRxiv, Kaper 1995, etc.).
* **`zeta_2` becomes a first-class prior.** `priors_default$parameters_global$zeta_2` is now populated with the literature-derived lognormal. `sample_parameters()` still derives the sampled `zeta_2 = zeta_1 / zeta_ratio` at sampling time (guarantees `zeta_1 > zeta_2` algebraically); the stored `zeta_2` prior is the reference distribution used for validation and downstream diagnostics.
* **Existing calibration posteriors are invalidated.** The current `zeta_1` posterior centred at ~48 k has effectively probability 0 under the new prior. Every existing calibration artefact under `MOSAIC-Mozambique/output/calibration/` must be re-run with the new priors before use.
* **`config_default.rda` scale shift.** `make_config_default.R` placeholder constants (`zeta_1`, `zeta_2`, `.zeta_ratio_default`) have been updated to the new prior medians. Any code that reads `config_default$zeta_*` expecting the old numeric scale will behave differently.
* **`config_default_MOZ.rda` scale shift.** The same placeholder constants in `make_config_default_MOZ.R` have been updated.
* **MOZ project override (stand-alone MOSAIC-Mozambique)** uses its own `zeta_ratio` centre (50) independent of the pkg default. That override is unaffected; the MOZ team decides adoption there.
* **LASER reservoir storage precision.** At the new biological scale (`zeta_1 ~ 1e11`, `I_sym ~ 100`), the daily Poisson mean contribution to `W` reaches ~1e13 cells - far above float32's exact-integer limit (~1.7e7). **`laser-cholera/src/laser/cholera/metapop/environmental.py` must widen `W` / `W_next` to float64 before this release can be merged.** This is an EXTERNAL (READ-ONLY) laser-cholera change and is tracked as a pending prerequisite; until the dtype change lands, running `run_MOSAIC()` against the new priors will silently accumulate rounding error in the reservoir update each tick.

## New functions

* `est_zeta_1_prior(PATHS, severity_mix)` - weighted-MLE lognormal on symptomatic shedding anchors.
* `est_zeta_2_prior(PATHS)` - weighted-MLE lognormal on asymptomatic shedding anchors with hard `sdlog >= 2.0` floor.
* `est_zeta_ratio_prior(PATHS, zeta_1_fit, zeta_2_fit, n_sim, seed)` - precision-weighted combination of direct-literature and derived paired-Monte-Carlo channels.

## Migration notes

* Rebuild priors: `source("data-raw/make_priors_default.R")`.
* Rebuild MOZ priors: `source("data-raw/make_priors_default_MOZ.R")`.
* Rebuild configs: `source("data-raw/make_config_default.R")` and `source("data-raw/make_config_default_MOZ.R")`.
* Downstream MOSAIC-docs figures with `zeta_*` axes must be regenerated.
* Re-run calibration for every production configuration before using outputs in interventions analyses.

# MOSAIC 0.28.13

## Other

* Added Tony Ting, Dejan Lukacevic, and Meikang Wu to package authors as contributors (`ctb`) in `DESCRIPTION` and the pkgdown site.

# MOSAIC 0.24.1

## Bug fixes

* `process_open_meteo_data()` now renames `soil_moisture_0_to_7cm_mean` to `soil_moisture_0_to_10cm_mean` on raw ERA5 historical parquets before splicing with climate-model projections. Upstream open-meteo-pipeline [issue #5](https://github.com/InstituteforDiseaseModeling/open-meteo-pipeline/issues/5) switched ERA5 requests to the 0-7 cm band (the ERA5 Historical API silently returned all-null for the 0-10 cm band), so without this rename the `rbind()` of historical + climate frames produced mismatched columns and every downstream soil-moisture feature in `compile_suitability_data()` / `est_suitability()` became NA. Users who have cached outputs from before the upstream fix should run `process_open_meteo_data(PATHS, force = TRUE)` once to force regeneration; the cache check compares source vs. output mtimes and will not otherwise pick up the upstream schema change.

# MOSAIC 0.24.0

## Behavior change (not API)

* **`control$predictions$optimize_subset = TRUE` now drives the canonical posterior artifacts.** Previously the optimized subset was a parallel reporting track: it produced an `ensemble_optimized.rds` and `*_optimized` metrics in `summary.json`, but `posteriors.json`, `posterior_quantiles.csv`, ensemble plots, and chained downstream priors were all computed from the tier-selected subset. After this release, when the flag is on the optimized subset is written to new `is_best_subset_opt` / `weight_best_opt` columns in `samples.parquet` and every posterior-consuming function reads from those columns. The tier-selected subset remains in `is_best_subset` / `weight_best` for provenance.
* **`summary.json` field rename.** The previous `r2_cases_ensemble_optimized` / `r2_deaths_ensemble_optimized` / `bias_ratio_*_ensemble_optimized` / `n_ensemble_params_optimized` fields are renamed to `*_ensemble_tier` / `n_ensemble_params_tier`. The canonical `r2_cases_ensemble` (etc.) now holds the optimized metrics when `optimize_subset = TRUE` and the tier metrics when the flag is off; the new `*_tier` fields preserve the tier-subset metrics for side-by-side comparison (NA when the flag is off). Downstream consumers reading the old `_optimized` field names must be updated.
* **Ensemble construction moved earlier in `run_MOSAIC()`.** The Dask reconnect + stochastic sims + `calc_model_ensemble` block now runs **before** posterior quantile/distribution/sensitivity construction so that the optimizer (when enabled) can refine the posterior. Best-model PPC and ensemble metrics/plots remain after posterior construction.
* Users relying on the old behavior (tier subset drives posteriors regardless of flag) should set `control$predictions$optimize_subset = FALSE`.

## New arguments

* `calc_model_posterior_quantiles()`, `plot_model_parameter_correlation()`, `plot_model_parameter_sensitivity()`, and `plot_model_posteriors_detail()` gained `subset_col` and `weight_col` arguments defaulting to `"is_best_subset"` / `"weight_best"`. Existing callers see no change.
* `optimize_ensemble_subset()` gained an optional `seeds` argument and returns `optimal_seeds` so callers can map the optimized subset back to `samples.parquet` without duplicating the internal sort logic.
* `calc_convergence_diagnostics()` gained an optional `n_best_subset_optimized` argument; when supplied, the JSON output includes a new `metrics$B_size_optimized` entry and `summary$n_best_subset_optimized` field.

## Defaults

* `control$predictions$optimize_min_n` raised from `4L` to `30L`. Four was the statistical minimum per Fox et al. (2024), but posterior density estimation on the optimized subset needs more samples; a warning is logged when the optimizer selects `< 30`.

## Internal

* Added `.mosaic_active_subset_cols(results, control)` helper that returns the canonical subset/weight column names plus a `"tier"` / `"optimized"` source tag. Used by `run_MOSAIC()` to thread the correct columns through all posterior-consuming calls.

# MOSAIC 0.13.21

## Bug Fixes

* **Respect control$parallel$enable flag in prediction plotting functions**
  - **Problem**: `plot_model_fit_stochastic()` and `plot_model_fit_stochastic_param()` were hardcoded to use `parallel = TRUE`, ignoring the user's `control$parallel$enable` setting
  - **Solution**: Changed both function calls in `run_MOSAIC()` to use `parallel = control$parallel$enable` instead of hardcoded `TRUE`
  - **Impact**: Users can now disable parallel execution for ensemble predictions by setting `control$parallel$enable = FALSE`, useful for debugging or when parallel execution causes issues
  - **Files modified**: `R/run_MOSAIC.R` (lines 1444, 1478)

# MOSAIC 0.13.20

## Bug Fixes

* **Fix Numba/TBB threading conflict in ALL parallel execution contexts**
  - **Problem**: Numba (used by laser-cholera) and Intel TBB library cause threading conflicts when R forks parallel workers, resulting in "Attempted to fork from a non-main thread" warnings and potential deadlocks or hangs
  - **Solution**: Set threading environment variables to 1 before cluster creation and in each worker across ALL functions that use parallel execution
  - **Environment variables set**:
    - `TBB_NUM_THREADS = "1"` - Intel Threading Building Blocks
    - `NUMBA_NUM_THREADS = "1"` - Numba JIT compiler
    - `OMP_NUM_THREADS = "1"` - OpenMP
    - `MKL_NUM_THREADS = "1"` - Intel MKL
    - `OPENBLAS_NUM_THREADS = "1"` - OpenBLAS
  - **Implementation**: Applied to all 4 locations where `parallel::makeCluster()` is called:
    - `R/run_MOSAIC.R` - Main calibration workflow
    - `R/plot_model_fit_stochastic_param.R` - Ensemble predictions (was causing hangs at "Generating ensemble predictions")
    - `R/plot_model_fit_stochastic.R` - Stochastic predictions
    - `R/calc_npe_diagnostics.R` - NPE SBC diagnostics
  - **Each location now has**:
    - Environment variables set in main process before cluster creation
    - BLAS thread limiting in workers
    - Environment variables set again in each worker
  - **Impact**: Prevents fork-related threading conflicts across entire package, ensures stable parallel execution with laser-cholera simulations, fixes hangs during ensemble predictions
  - **Files modified**: `R/run_MOSAIC.R`, `R/plot_model_fit_stochastic_param.R`, `R/plot_model_fit_stochastic.R`, `R/calc_npe_diagnostics.R`

# MOSAIC 0.13.5

## Improvements

* **Use linear interpolation for NA values in observed data**
  - **Problem**: Previously converted all NAs to 0, artificially introducing "no cases" observations that could mislead the model
  - **Solution**: Linear interpolation within each location preserves temporal trends
  - **Method**:
    - Uses `approx(method = "linear", rule = 1)` to interpolate interior NAs
    - `rule = 1` prevents extrapolation beyond data range (preserves boundaries)
    - Only sets start/end NAs to 0 when interpolation is impossible (no surrounding data points)
    - Applies independently to each location for multi-location data
    - Handles both cases and deaths time series
  - **Example**: Time series `NA, NA, 10, 20, NA, 30, 40, NA, 50, NA` becomes `0, 0, 10, 20, 25, 30, 40, 45, 50, 0`
    - Interior NAs interpolated: position 5 → 25 (between 20 and 30), position 8 → 45 (between 40 and 50)
    - Boundary NAs set to 0: positions 1-2 (before first data), position 10 (after last data)
  - **Impact**: More accurate representation of missing data, better model training quality
  - **Output**:
    - Reports number of NAs interpolated vs. set to 0
    - "Interpolated: X" shows successful linear interpolation
    - "Set to 0 (start/end): Y" shows boundary NAs
  - **Files modified**: `R/npe_posterior.R`

# MOSAIC 0.13.4

## Bug Fixes

* **Add comprehensive data validation to train_npe() to catch NAs/Infs early**
  - **Problem**: PyTorch silently propagates NaN/Inf values through the network, causing cryptic training failures
  - **Root cause**: No validation of input data (X, y, weights) before tensor conversion
  - **Impact**: If parameters or observations contain NAs/Infs (from corrupted files, invalid samples, or bugs), they propagate silently and cause losses to become NaN
  - **Fix**: Add explicit validation checks for X (parameters), y (observations), and weights before tensor conversion (line 170-266 in npe.R)
  - **Validation checks**:
    - `anyNA(X)` and `any(!is.finite(X))` - parameters matrix
    - `anyNA(y)` and `any(!is.finite(y))` - observations matrix
    - `anyNA(weights)` and `any(!is.finite(weights))` - weight vector
  - **Error messages include**:
    - Which data structure failed (X, y, or weights)
    - Whether NAs or Infs were found
    - Affected rows and columns (first 5-10 shown)
    - Likely sources of corruption
    - Specific solutions to diagnose and fix
  - **Benefits**:
    - Catches data corruption BEFORE training starts (saves time)
    - Clear diagnostic messages pinpoint exact problem
    - Prevents silent NaN propagation through network
    - Helps identify upstream bugs in data preparation
  - **Files modified**: `R/npe.R`
  - **Note**: This addresses the user's concern that NA/NaN errors with 25k evenly weighted samples (binary_retained) should not be due to numerical instability, but rather data corruption

# MOSAIC 0.13.3

## Bug Fixes

* **Fix cryptic "missing value where TRUE/FALSE needed" error in NPE training**
  - **Root cause**: Validation loss became NA/NaN during training, causing `if (val_loss < best_val_loss)` comparison to fail with cryptic error
  - **Error**: `Error during wrapup: missing value where TRUE/FALSE needed` followed by recursive error and abort
  - **Trigger**: Low ESS (Kish: 1.2, Perplexity: 3.0) with `continuous_retained` weight strategy causing numerical instability
  - **Fix**: Add explicit NA/NaN checking for train_loss and val_loss before early stopping comparison (line 384-405 in npe.R)
  - **Impact**: Now provides clear, actionable error message with diagnostic information and solutions
  - **Error message includes**:
    - Which epoch failed and what the loss values were
    - Common causes (low ESS, weight concentration, architecture complexity)
    - Specific solutions (try 'continuous_best', use 'light' tier, reduce learning rate)
  - **Files modified**: `R/npe.R`
  - **Related**: This error was masked by recursive error handling - actual issue is numerical instability from degenerate weight distributions

# MOSAIC 0.13.2

## Bug Fixes

* **CRITICAL: Fix run_NPE() JSON loading causing persistent list column errors**
  - **Root cause**: Used `jsonlite::fromJSON(..., simplifyVector = FALSE)` when loading config/priors from disk, keeping JSON arrays as R lists instead of converting to vectors
  - **Error**: Even after v0.13.1 fix in get_npe_observed_data(), `config$reported_cases` was still a list, creating list columns in data frames
  - **Fix**: Replace `jsonlite::fromJSON()` with `read_json_to_list()` (MOSAIC standard loader) at 3 locations in run_NPE.R (lines 244, 264, 284)
  - **Benefits**:
    - Uses MOSAIC codebase standard for JSON loading (consistent with other functions)
    - Properly simplifies JSON arrays to R vectors (`simplifyVector = TRUE` default)
    - Supports gzipped JSON files
    - More maintainable and consistent
  - **Impact**: Resolves persistent CSV write errors in run_NPE() standalone mode
  - **Files modified**: `R/run_NPE.R`
  - **Verified**: JSON loading, get_npe_observed_data(), CSV write all succeed

# MOSAIC 0.13.1

## Bug Fixes

* **Fix list column error in get_npe_observed_data() causing CSV write failures**
  - **Root cause**: When `config$location_name` or `config$iso_code` is a list (common in LASER config format), single-bracket extraction `location_names[1]` returned a list of length 1 instead of scalar, creating list columns in data frames
  - **Error**: `Error in utils::write.table(...): unimplemented type 'list' in 'EncodeElement'` when writing observed_data.csv in run_NPE()
  - **Fix**: Convert location_names to character vector using `unlist()` at function start (line 1008)
  - **Fix**: Changed all `location_names[index]` to `location_names[[index]]` for scalar extraction (lines 1032, 1055, 1134)
  - **Impact**: NPE workflow now handles list-type location identifiers correctly
  - **Files modified**: `R/npe_posterior.R` (get_npe_observed_data function)
  - **Verified**: CSV write succeeds, data frame structure correct, existing tests still pass

# MOSAIC 0.13.0

## Major Features

* **New run_NPE() function for flexible Neural Posterior Estimation**
  - Complete NPE workflow extracted into standalone function in `R/run_NPE.R` (1000+ lines)
  - **Dual-mode architecture**:
    - **Embedded mode**: Runs inside `run_MOSAIC()` with in-memory objects (no disk I/O)
    - **Standalone mode**: Runs independently after calibration completes, loading from disk
  - **Key features**:
    - Automatic mode detection based on arguments provided
    - Custom `output_dir` support for experimenting with multiple NPE strategies
    - Root directory auto-detection from `getOption('root_directory')`
    - Full control object support for all NPE hyperparameters
    - Complete error handling and validation
  - **Benefits**:
    - Post-hoc NPE without re-running expensive BFRS calibration
    - Experiment with different weight strategies (continuous_best, continuous_retained, etc.)
    - Cleaner, more maintainable code architecture
    - Reusable in custom workflows
  - **run_MOSAIC.R refactored**: Replaced 300+ lines of inline NPE code with clean `run_NPE()` call (lines 1500-1523)
  - **Standalone examples added**: `vm/launch_mosaic.R` now includes post-hoc NPE usage examples (lines 201-233)
  - See function documentation: `?run_NPE`

# MOSAIC 0.11.5

## Changes

* **Simplify plot_model_ppc: Remove by_location argument**
  - Function now always creates both aggregate and per-location plots by default
  - **Removed argument**: `by_location` (previously: "aggregate", "both", "per_location")
  - **New behavior**: Always creates comprehensive diagnostics (aggregate + per-location)
  - Simplifies API - no configuration needed for plot output mode
  - Legacy model mode still only creates aggregate plots (as before)
  - Updated function signature: `plot_model_ppc(predictions_dir, predictions_files, locations, model, output_dir, verbose)`
  - Updated call in run_MOSAIC.R to remove by_location argument

# MOSAIC 0.11.4

## Bug Fixes

* **Add backward compatibility for plot_model_ppc function signature**
  - Wrapped plot_model_ppc call in run_MOSAIC with tryCatch to handle old package versions
  - **Issue**: Clusters with cached old package versions (pre-v0.11.0) have different function signature
  - Old signature: `plot_model_ppc(model, output_dir, verbose)`
  - New signature: `plot_model_ppc(predictions_dir, predictions_files, by_location, locations, model, output_dir, verbose)`
  - **Solution**: Try new signature first; if "unused arguments" error, log warning and skip PPC plots
  - Prevents workflow from crashing on clusters that need package reinstallation
  - Fixed at lines 1415-1441 in `run_MOSAIC.R`
  - **Note**: Users should reinstall package on cluster to get full PPC functionality

# MOSAIC 0.10.25

## Bug Fixes

* **CRITICAL: Fixed missing Prior/BFRS curves for seasonality parameters in distribution plots**
  - Fixed gsub order in parameter name variant generation for seasonality params
  - **Root cause**: Wrong order in `gsub()` calls generated incorrect variant "a1j" instead of "a1"
  - Original: `gsub("_j$", "", gsub("_", "", "a_1_j"))` → "a1j" (WRONG)
  - Fixed: `gsub("_", "", gsub("_j$", "", "a_1_j"))` → "a1" (CORRECT)
  - **Impact**: Prior/BFRS use `a1`, NPE uses `a_1_j` - variant "a1j" didn't match either
  - Plotting function now correctly finds seasonality params in all three JSONs
  - Fixed at lines 515-516 in `plot_model_distributions.R`
  - **Result**: All three curves (Prior, BFRS, NPE) now appear for seasonality parameters

# MOSAIC 0.10.23

## Bug Fixes

* **CRITICAL: Fixed missing NPE posteriors in distribution plots**
  - Added uniform distribution handling to `.fit_distribution()` in NPE posterior processing
  - **Root cause**: Function only handled beta, gamma, lognormal, normal - NOT uniform
  - When dist_type="uniform", function fell through to normal case, setting mean/sd instead of min/max
  - Result: NPE posteriors.json had `{distribution: "uniform", parameters: []}`
  - Plotting function couldn't plot uniform without min/max parameters → NPE curves missing
  - **Solution**: Calculate min/max from 1% and 99% quantiles (robust to outliers) + 1% buffer
  - Fixed at lines 1516-1528 in `npe_posterior.R`
  - **Impact**: NPE posteriors now appear in distribution plots (Prior vs BFRS vs NPE)

# MOSAIC 0.10.20

## Bug Fixes

* **CRITICAL: Fixed derived parameters not added to posteriors.json**
  - Modified `calc_model_posterior_distributions()` to dynamically add parameters missing from priors template
  - Derived parameters (beta_j0_hum, beta_j0_env) now correctly added to posteriors.json
  - **Root cause**: Function used priors.json as template, which only contains sampled parameters
  - **Solution**: Dynamically create parameter structure for derived parameters not in priors
  - Handles multiple locations correctly (adds each location as it's processed)
  - Fixed at lines 391-420 in `calc_model_posterior_distributions.R`
  - **Result**: beta_j0_hum and beta_j0_env now appear in distributions_ETH_Prior_Posterior.pdf

# MOSAIC 0.10.19

## Bug Fixes

* **Fixed missing derived parameters (beta_j0_hum, beta_j0_env) in distribution plots**
  - Changed distribution type from "derived" to "gamma" in estimated_parameters source data
  - Derived rate parameters (beta_j0_hum, beta_j0_env) now fitted with gamma distributions
  - **Background**: beta_j0_hum = p_beta × beta_j0_tot, beta_j0_env = (1 - p_beta) × beta_j0_tot
  - Previously marked as "failed" because "derived" distribution type was unhandled
  - Now appear in distributions_ETH_Prior_Posterior.pdf alongside beta_j0_tot and p_beta
  - Fixed at line 289 in `data-raw/make_estimated_parameters_inventory.R`

# MOSAIC 0.10.17

## Bug Fixes

* **Fixed plot_model_convergence diagnostic text formatting**
  - Corrected convergence metrics display in diagnostic plots
  - Fixed in `R/plot_model_convergence.R`

# MOSAIC 0.10.15

## Bug Fixes

* **CRITICAL: Fixed "subscript out of bounds" error in convergence diagnostic plots**
  - Fixed regression from v0.10.14 where `targets[["ESS_min"]]` would error if element missing
  - **Root cause**: `safe_numeric()` returned `numeric(0)` for NULL inputs, causing elements to be dropped from vectors
  - **Solution 1**: Enhanced `safe_numeric()` to check for NULL and empty vectors before conversion
  - **Solution 2**: Changed extraction from `[[` to `as.numeric(x["name"])` for safe handling of missing elements
  - `[[` throws error on missing elements; `as.numeric(x["name"])` returns NA safely
  - Fixed in both `plot_model_convergence.R` and `plot_model_convergence_loss.R`
  - Now properly handles cases where JSON diagnostics may have missing target values

# MOSAIC 0.10.14

## Bug Fixes

* **Fixed sprintf formatting errors in convergence diagnostic plots**
  - Fixed "Error formatting: ESS: %.0f [target >= %.0f]" messages in convergence_diagnostic.pdf
  - **Root cause**: Using single brackets `metrics["ESS"]` returns named vector element, not just value
  - **Solution**: Changed to double brackets `metrics[["ESS"]]` to extract raw values
  - Fixed in both `plot_model_convergence.R` (lines 214-223, 330-336) and `plot_model_convergence_loss.R` (lines 108-114)
  - All convergence metrics (ESS, A, CVw, B) now format correctly in diagnostic text

# MOSAIC 0.10.13

## Bug Fixes

* **Fixed plotting scale for extremely small initial condition posteriors (E_initial, I_initial)**
  - Implemented automatic detection of tiny values (< 0.001) in posterior distributions
  - Applied scientific notation formatting for x-axis labels when values < 0.001
  - Increased padding from 10% to 30% for better visibility of tiny value distributions
  - Fixed all 8 panels: Prior, Retained, Retained Weighted, Best Unweighted, Best Weighted, Caterpillar, Distributions (Empirical), Distributions (Theoretical)
  - **Root cause**: E_initial and I_initial have epidemiologically correct tiny values (~10⁻⁷ proportion, representing ~50-60 people in population of 126M), which appeared as flat lines when plotted on wide [0,1] axis
  - **Solution**: Tight x-axis limits with scientific notation (e.g., "2.0e-07", "4.0e-07", "6.0e-07")
  - Fixed throughout `plot_model_posteriors_detail.R` (lines 380-769)
  - See `claude/initial_EI_plotting_issue.md` for complete analysis

# MOSAIC 0.10.12

## Bug Fixes

* **Fixed missing dplyr namespace prefix in `plot_model_posteriors_detail()`**
  - Added explicit `dplyr::` prefix to `slice()` function call
  - Fixes "could not find function 'slice'" error
  - Fixed at line 908 in `plot_model_posteriors_detail.R`

# MOSAIC 0.10.11

## Bug Fixes

* **Fixed S7 class conflict with patchwork operators in `plot_model_posteriors_detail()`**
  - Replaced `/` operator with explicit `patchwork::wrap_plots(ncol=1)` calls
  - Fixes "Can't find method for generic `/(e1, e2)`" error from S7 class system
  - S7 was intercepting the patchwork `/` operator between ggplot objects
  - Using explicit `wrap_plots()` avoids operator dispatch conflicts
  - Fixed at lines 727-754 in `plot_model_posteriors_detail.R`

# MOSAIC 0.10.10

## Bug Fixes

* **CRITICAL: Fixed incorrect weighting in NPE ensemble predictions**
  - Removed density-based weighting that caused double-weighting artifact
  - NPE samples are drawn directly from posterior p(θ|x), so uniform weights are correct
  - Using density as weights was creating effective distribution [p(θ|x)]²
  - This over-emphasized high-density regions and under-represented uncertainty
  - Now uses uniform weights (NULL) for proper posterior predictive sampling
  - Fixed at lines 1679-1686 in `run_MOSAIC.R`
  - **Impact:** NPE ensemble predictions should now have appropriate uncertainty bands
  - **Theory:** For posterior predictive p(y|x) ≈ (1/N) Σ p(y|θᵢ) where θᵢ ~ p(θ|x)
  - Density weights only correct for importance sampling from q(θ) ≠ p(θ|x)
  - See `claude/npe_weighting_analysis.md` for complete theoretical analysis

# MOSAIC 0.10.9

## Bug Fixes

* **Fixed missing namespace prefixes in `plot_model_posteriors_detail()`**
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions (68+ occurrences)
  - Added `grid::` prefix for `unit()` calls
  - Added `arrow::` prefix for `read_parquet()`
  - Added `patchwork::` prefixes for `wrap_plots()` and `plot_layout()`
  - Added `cowplot::` prefixes for `plot_grid()` and `get_legend()`
  - Fixes "could not find function 'geom_histogram'" error
  - Fixed throughout `plot_model_posteriors_detail.R`

# MOSAIC 0.10.8

## Bug Fixes

* **Fixed inappropriate hard-coded bounds for truncated normal distribution**
  - Replaced arbitrary -45/45 defaults with -Inf/Inf to match fitting function behavior
  - Added intelligent plotting range selection for infinite bounds
  - Use mean ± 4sd for plotting range when bounds are infinite (covers 99.99%)
  - Properly handle one-sided truncation (e.g., a=-Inf, b=10)
  - Format display strings to show "Inf" for infinite bounds
  - Fixed at lines 445-481 in `plot_model_distributions.R`

# MOSAIC 0.10.7

## Bug Fixes

* **CRITICAL: Fixed remaining NA handling error in `calc_distribution_density()`**
  - Fixed "missing value where TRUE/FALSE needed" error for truncated normal distribution
  - Completed NA handling fix missed in v0.10.6
  - Fixed truncnorm distribution at lines 446-453 in `plot_model_distributions.R`
  - Now all distribution types properly handle NULL parameters

# MOSAIC 0.10.6

## Bug Fixes

* **CRITICAL: Fixed NA handling error in `calc_distribution_density()`**
  - Fixed "missing value where TRUE/FALSE needed" error in `plot_model_distributions()`
  - Replaced unsafe `as.numeric(NULL)` pattern with explicit `NA_real_` conversion
  - Fixed for uniform, normal, and gompertz distributions (lines 402-427)
  - Prevents crashes when parameter bounds are NULL

* **Fixed all ggplot2 3.4.0+ deprecation warnings**
  - Replaced deprecated `size=` with `linewidth=` in `geom_line()` (14 instances)
  - Replaced deprecated `size=` with `linewidth=` in `geom_smooth()` (2 instances)
  - Replaced deprecated `size=` with `linewidth=` in `element_line()` (10 instances)
  - Replaced deprecated `size=` with `linewidth=` in `geom_vline()` (6 instances)
  - Fixed across 10 files: plot_generation_time.R, plot_vibrio_decay_rate.R, est_symptomatic_prop.R, plot_suspected_cases.R, plot_CFR_by_country.R, plot_vaccine_effectiveness.R, est_WASH_coverage.R, npe_plots.R, plot_africa_map.R, plot_model_distributions.R

# MOSAIC 0.10.5

## Bug Fixes

* Fixed broken documentation links to deprecated `run_mosaic_iso()`
  - Removed references to `run_mosaic_iso()` in `run_MOSAIC()` documentation
  - Fixes roxygen2 warnings about unresolvable links
  - Updated @description and removed @seealso reference

# MOSAIC 0.10.4

## Bug Fixes

* Fixed missing namespace prefixes in `plot_npe_training_loss()`
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions throughout the function
  - Fixes "could not find function 'facet_wrap'" error during NPE training visualization
  - Fixed throughout lines 1585-1768 including: `facet_wrap`, `ggplot`, `aes`, `geom_smooth`, `geom_line`, `geom_vline`, `geom_point`, `geom_text`, `labs`, `theme_minimal`, `theme`, and all theme element functions

# MOSAIC 0.10.3

## Bug Fixes

* Fixed missing `ggsave()` namespace prefix in `plot_model_distributions()`
  - Added explicit `ggplot2::ggsave()` prefix at two save locations
  - Fixes "could not find function 'ggsave'" error when saving plots
  - Fixed for both global parameters plot (line 834) and location-specific plots (line 980)

# MOSAIC 0.10.2

## Bug Fixes

* Fixed missing namespace prefixes in `plot_model_distributions()`
  - Added explicit `ggplot2::` prefixes to `scale_x_continuous()` and all other ggplot2 functions
  - Fixes "could not find function 'scale_x_continuous'" error in `create_multi_method_plot()`
  - Fixed in three locations: main plot creation and two legend plot sections
  - Also added `grid::unit()` prefix for grid package function

# MOSAIC 0.10.1

## Bug Fixes

* Fixed missing namespace prefixes in `plot_model_posterior_quantiles()`
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions
  - Fixes "could not find function 'geom_errorbar'" error
  - Functions not imported in NAMESPACE now called with explicit prefix
  - Affects both global and location-specific plotting sections

# MOSAIC 0.10.0

## Breaking Changes

* **Major refactor: Lean run_MOSAIC() - aggressive simplification**
  - Moved `run_mosaic_iso()` to `deprecated/` directory (use `run_MOSAIC()` directly)
  - Stripped ESS calculation: removed conditional skipping, verbose summaries
  - Stripped convergence diagnostics: removed section banners, verbose logging
  - Stripped posterior sections: removed verbose progress, set verbose=FALSE everywhere
  - Stripped PPC sections: removed conditional checks, verbose status messages
  - Stripped parameter uncertainty: removed ensemble logging
  - Stripped NPE section: removed 66 log_msg calls, verbose diagnostics, section banners
  - Stripped POST-HOC optimization: removed tier-by-tier logging, convergence messages
  - Stripped WEIGHTS section: removed detailed ESS/temperature logging
  - Removed all major section banners (80× '=' decorative headers)
  - Pattern applied: calculate → write → log filepath (no defensive checks)
  - Total: removed 180+ verbose log_msg calls and 90+ section banners
  - **Result: 284 lines removed (2441 → 2157 lines, 12% reduction)**

## Deprecations

* `run_mosaic_iso()` - Use `run_MOSAIC()` with `get_location_config()` and `get_location_priors()` instead

# MOSAIC 0.9.1

## Bug Fixes

* Fixed premature cleanup of `ess_results` variable in `run_MOSAIC()`
  - Removed cleanup at line 1204 that occurred before `calc_convergence_diagnostics()` call
  - Variable is now retained until after convergence diagnostics are calculated
  - Fixes "object 'ess_results' not found" error at runtime

# MOSAIC 0.9.0

## Breaking Changes

* **Major refactor: Removed ALL excessive flow control from `run_MOSAIC()`**
  - Removed validation wrapper around `calc_convergence_diagnostics()` (90 lines → 30 lines)
  - Removed ALL tryCatch blocks around plotting functions (10+ instances)
  - Removed tryCatch around `calc_model_posterior_quantiles()`
  - Removed tryCatch around `calc_model_posterior_distributions()`
  - Removed tryCatch around `sample_parameters()` for best model
  - Removed tryCatch around `lc$run_model()` for best model
  - Removed tryCatch around `plot_model_fit_stochastic()` and related plotting
  - Removed defensive if-else wrapper around NPE posterior samples processing
  - Functions now fail fast with clear error messages at the source
  - **Impact:** Errors will stop execution immediately rather than continuing with NA/NULL values
  - **Benefit:** Much easier to debug - errors show exactly where the problem is
  - **Note:** Parallel worker tryCatch blocks retained (essential for batch processing)

## Why This Change?

Excessive defensive programming was masking real errors and making debugging extremely difficult.
The new fail-fast approach:
- ✅ Errors happen at the source with clear tracebacks
- ✅ Simpler code flow that's easier to understand and maintain
- ✅ Forces fixing root causes instead of papering over problems
- ✅ Better for research/development workflows
- ✅ Reduces code complexity (~150+ lines of error handling removed)

**If you encounter errors after upgrading:** The errors were always there, just hidden.
Fix the underlying issue rather than relying on fallback behavior.

# MOSAIC 0.8.9

## Bug Fixes

* Fixed uninitialized variable in `run_MOSAIC()`
  - Added defensive initialization of `ess_results` to NULL before parameter-specific ESS calculation
  - Prevents "object not found" error when debugging or if code execution is interrupted
  - Variable is properly set later based on sample availability (lines 1161 or 1164)

# MOSAIC 0.8.8

## Bug Fixes

* Fixed test failures in `test-calc_convergence_diagnostics.R`
  - Corrected threshold expectations for "lower is better" metrics (CVw)
  - Changed test values to properly demonstrate warn status (within 120-200% of target)
  - Updated helper function tests to use `MOSAIC:::` for internal function access
  - All 70 tests now pass

# MOSAIC 0.8.0

## New Features

### Initial Conditions Sampling
* Added biologically plausible Beta priors for initial condition compartments (S, V1, V2, E, I, R)
  - `prop_S_initial`: Beta(30, 7.5) - mean 80% susceptible
  - `prop_V1_initial`: Beta(0.5, 49.5) - mean 1% one-dose vaccination  
  - `prop_V2_initial`: Beta(0.5, 99.5) - mean 0.5% two-dose vaccination
  - `prop_E_initial`: Beta(0.01, 9999.99) - mean 0.0001% exposed
  - `prop_I_initial`: Beta(0.01, 9999.99) - mean 0.0001% infected
  - `prop_R_initial`: Beta(3.5, 14) - mean 20% recovered/immune

* Enhanced `sample_parameters()` function:
  - New `sample_initial_conditions` argument to control IC sampling
  - Automatic normalization of compartment proportions to sum to 1.0
  - Proper conversion from proportions to integer counts
  - Rounding error adjustment to ensure exact population totals

* Updated `create_sampling_args()` helper:
  - Added "initial_conditions_only" pattern for sampling just ICs
  - Support for new `sample_initial_conditions` flag

## Data Updates

* Renamed `priors` data object to `priors_default` to match naming convention with `config_default`
* Updated `priors_default` data object to include initial condition priors for all 40 African countries
* Priors now available in both R data format (`data/priors_default.rda`) and JSON (`inst/extdata/priors.json`)

## Documentation

* Added comprehensive documentation for the `priors_default` data object
* Updated `sample_parameters()` documentation to reflect IC sampling capability
* Added examples demonstrating initial conditions sampling workflow

## Internal Changes

* Added `sample_initial_conditions_impl()` internal function for IC sampling logic
* Updated NAMESPACE to import required stats functions (rbeta, rgamma, rlnorm, rnorm, runif)