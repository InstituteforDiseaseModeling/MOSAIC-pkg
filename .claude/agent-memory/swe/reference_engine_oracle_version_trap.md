---
name: engine-oracle-version-trap
description: The laser-cholera working checkout is v0.13.1 but the R engine was ported from v0.16.1 — reading the files on disk fabricates four infectious.py "port bugs"; how to read the real oracle, and what the replay fixtures do NOT cover
metadata:
  type: reference
---

# The on-disk Python oracle is the WRONG VERSION

`/Users/johngiles/MOSAIC/laser-cholera/` is checked out at **`v0.13.1`** (detached
HEAD). The R engine in `R/sim_components.R` was ported from **`v0.16.1`**, commit
`06938c1513b1c4d38fc902b62d1954ba4449ebe3` — which is what
`tests/testthat/fixtures/ORACLE.md` records and what the replay fixtures were
generated from. Verify with `grep '^version' pyproject.toml`.

**Read the real oracle with `git show`, never by opening the file:**
```sh
cd /Users/johngiles/MOSAIC/laser-cholera
git show "06938c1513b1c4d38fc902b62d1954ba4449ebe3:src/laser/cholera/metapop/infectious.py"
```
The commit is in the local object store even though it is not checked out. Paths
are unchanged between the two versions. **zsh gotcha:** write `"${SHA}:src/..."`
with braces — `"$SHA:src/laser/..."` makes zsh apply `:s/laser/cholera/` as a
history modifier and you get `fatal: ambiguous argument`.

## What actually differs between v0.13.1 and v0.16.1

Enough that a reviewer using the on-disk file will report four false bugs in
`infectious.py` plus a missing channel:

| | v0.13.1 (on disk) | v0.16.1 (what R implements) |
|---|---|---|
| `reported_deaths` | `round(disease_deaths[lag] * rho_deaths)` — deterministic | `binomial(disease_deaths[lag], rho_deaths)` — a draw |
| `reported_cases` source | `Isym[idx_probe]` (prevalence) | `binomial(new_symptomatic[idx_probe], rho)` (incidence) |
| `reported_cases` write row | `[tick + 1]` | `[tick]` |
| `expected_cases` | exists, `round(new_symptomatic / rho)` | **removed** |
| `RInterface` trim for `births`/`disease_deaths`/`non_disease_deaths`/`reported_*` | `[1:, :]` (drop first) | `[:-1, :]` (drop last) |

The prevalence→incidence change is laser-cholera issue #67, shipped in its
v0.14.0 — see [[project_laser_cholera_reported_cases_fix_67]] in the user
auto-memory. The R port is correct against v0.16.1 on all five rows.

## The replay gate is strong, but has known blind spots

The Tier B replay harness is genuinely load-bearing: it compares tick, phase,
site, exact binomial `n`, and the rate/probability to a scale-aware tolerance,
then compares all 28 channels. I mutation-tested it with 31 injected faults and
**27 were caught**, including every row-index inversion, every write-target shift,
a `pi_ij` row/column flip, a `round`→truncate change, and 5 of 6 pipeline
reorderings.

Mutation harness recipe (read-only, no edits to `R/`):
```r
ns <- asNamespace("MOSAIC")
unlockBinding(".SIM_PHASE_FUNCTIONS", ns); unlockBinding("SIM_PIPELINE", ns)
# deparse a phase, gsub one line, eval back into ns, swap it into the table
```
Careful: `deparse()` wraps long statements across lines, so a `grep(pattern,
fixed = TRUE)` on the deparsed source can match a half-statement — check
`sum(hit) == 1` and patch the continuation line too.

**What no fixture reaches** (all four fixtures share one config family):
- `nu_2_jt` is **identically zero**, so the whole Vaccinated second-dose block is
  never run; `dose_two_doses` is an all-zero channel that the comparison only
  requires to be exactly zero. `data-raw/make_config_default.R` assigns the whole
  OCV series to `nu_1_jt` and zeroes `nu_2_jt`, so production never runs it either.
- the dose-one `pmin(doses, available)` clamp and the `pmax(available, 1)`
  divide-by-zero floor never bind.
- `delta_reporting_cases = 0` everywhere, so the lagged reported-cases probe never
  reads a row other than the current one.

Deleting either dose clamp or the denominator floor leaves the entire suite green.
If you touch `Vaccinated`, the replay tests are not your gate — write an identity
test instead (see [[reference_sim_engine_identity_tests]]).
