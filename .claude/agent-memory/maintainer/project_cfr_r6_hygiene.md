---
name: cfr-r6-hygiene
description: CFR-submodel R6 hygiene pass — the canonical implied-CFR helper and how to delegate a config to it, the chi_end/chi_epi=2/3 derivation gap pinned in tests, and the producer/consumer filename-drift pattern
metadata:
  type: project
---

R6 of the CFR restructure (branch `feature/cfr-restructure`, v0.93.0, plan at
`claude/cfr_review/PLAN.md`). Durable facts, not the diff.

## The implied-CFR calculator set — which one is canonical

Four parallel implementations existed. The canonical algebra is
`.mosaic_add_implied_cfr_columns()` in `R/calc_implied_cfr.R` (internal,
`@noRd`, called from `run_MOSAIC.R:1722`). It is **sample-frame shaped**: one
row per posterior draw, globals as columns (`rho`, `rho_deaths`,
`chi_endemic`, `chi_epidemic`, `gamma_1`) plus a per-iso column PAIR
(`mu_j_baseline_<iso>`, `mu_j_epidemic_factor_<iso>`). It emits
`cfr_baseline_<iso>` / `cfr_epidemic_<iso>` (and clinical variants), clamped to
[0,1]; it **omits** the surveillance CFR entirely when `gamma_1` is absent
rather than emitting the dwell-free value.

**To delegate a CONFIG to it** (the pattern now in `.fit_cfr_implied()`,
`R/run_fit_sandbox.R`): build a one-row data.frame, one column pair per selected
location, with SYNTHETIC iso labels (`L1..Ln`) — `config$location_name` can have
duplicates and would collide. Then average the returned per-location columns.
This works and is the right move; there is no reason to keep a second copy.

**Why "10x" and "18x" were both right.** Two agents measured the stale sandbox
copy's understatement differently. On the shipped `config_default` (locations
1:3) the old value was 0.001114; the canonical **baseline/endemic** endpoint is
0.009368 (**8.41x**) and the **epidemic** endpoint is 0.021078 (**18.9x**). The
old copy blended chi 50/50 and dropped the dwell divisor, so it maps to neither
endpoint. A single scalar "implied CFR" is not well defined — the two regime
endpoints bracket the realized period-mixed value.

## The chi gap the round-trip test could not see (R4/R5 will trip it)

`sample_parameters.R:670` ("B2.1") derives mu with **chi_epidemic only**:
`mu = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)`,
but the engine switches chi per tick. So inverting with `chi_endemic` returns
`CFR_target * (chi_endemic / chi_epidemic)`. At the shipped pair
(0.50 / 0.75) that is exactly **2/3 = 0.6667** — the epidemic column round-trips
to `CFR_target` exactly, the endemic column is 33.3% low.

`tests/testthat/test-implied-cfr-roundtrip.R` had `chi_endemic == chi_epidemic`
in every fixture, which collapses the only axis on which the two functions
disagree — a fixture that loosens the contract (CLAUDE.md Lesson 12(iii)). Two
chi-asymmetric tests now pin the 2/3 ratio ON PURPOSE and are annotated to FAIL
when R4/R5 switches the derivation to the effective chi. **Do not widen the
tolerance when they fail — replace the expectation with `CFR`.**

## Producer/consumer filename drift — a recurring smell worth grepping for

`process_CFR_data.R:120` writes `sprintf("case_fatality_ratio_2014_%d.csv", max_year)`
while `plot_CFR_by_country.R:26` read a hardcoded `..._2014_2024.csv`. Both files
sat on disk (May vs Sep), differing up to 4.4 CFR points, and the figure rendered
the stale one silently — it would only hard-error once someone deleted the old
file. Fix pattern: glob `^case_fatality_ratio_2014_[0-9]{4}\.csv$` and take
**max end-year, not newest mtime** (mtime changes if an old file is touched or
re-copied; the year is the artifact's own statement of coverage), and `stop()`
loudly when nothing matches. Whenever a writer uses `sprintf` on a date/year in a
filename, grep for readers of the literal name.

## Deletion hygiene that mattered here

`calc_deaths_from_infections()` / `calc_cases_from_infections()` (1,106 lines
incl. 17 `test_that` blocks) were deleted as orphans. Checks that made it safe:
each file held exactly ONE top-level function (Lesson 8); no `\link{}` to either
topic anywhere in `man/`; `_pkgdown.yml` covers them only via `matches("^calc_")`
so no yaml edit was needed; NAMESPACE is `exportPattern("^[[:alpha:]]+")` so no
export line existed to remove. Note the asymmetry: the roxygen source can be
cleaned by one agent while the generated `.Rd` still carries the stale prose
until someone runs `document()` — grep `man/` separately from `R/`.

## `delta_reporting_deaths` label, settled

Engine truth is `R/sim_components.R:198-206`: reads `disease_deaths` at
`tick - delta_reporting_deaths`, thins by `rho_deaths` ⇒ **death-event-to-report**.
`R/priors_default.R:95-99` and `data-raw/make_estimated_parameters_inventory.R:158`
already state this correctly. The live mislabel is
`R/make_simulation_config.R:120` (`@param ... Symptom-onset-to-death-report`) and
its generated `man/make_simulation_config.Rd:235`. A weaker family of comment
labels says "Infection-to-death reporting delay" — also wrong in the same
direction — at `R/sample_parameters.R:64`, `R/make_simulation_config.R:322`,
`R/run_MOSAIC.R:3568`, `R/convert_config_to_matrix.R:133`,
`R/convert_config_to_dataframe.R:120`, `data-raw/make_config_default.R:607`,
`data-raw/make_simulation_{endemic,epidemic}_config_files.R:171/145`.
Occurrences inside the `make_priors_default.R` / `priors_default.json` changelog
strings are HISTORY ("corrected from X to Y") and must not be "fixed".

See also [[reviewer-checklist]], [[testthat-parallel-nesting-landmine]].
