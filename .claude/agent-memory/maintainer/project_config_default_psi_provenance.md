---
name: config-default-psi-provenance
description: How config_default psi_jt is built + the recurring reproducibility gap between the shipped psi and the canonical pipeline
metadata:
  type: project
---

config_default's `psi_jt` is built by `data-raw/make_config_default.R` reading
`model/input/pred_psi_suitability_day.csv` (the est_suitability LSTM forecast).
The build step is reproducible: rebuild via acast(iso_code~date, value.var="psi")
ordered by `config_default$location_name`, subset to [date_start, date_stop] ->
bit-exact match to shipped .rda/.json. `date_stop` = min-across-iso of last psi
date (AGO is the limiting location -> 2026-10-29 as of v4.0); avoids forward-fill
flat tail. config_default has NO rownames after JSON round-trip; psi_jt is 40x1367,
positional. `model/` is in `.Rbuildignore` so the (37MB+) CSV never ships in the
built package -- it is repo bloat only, not runtime.

**Recurring reproducibility gap (verify on every psi/config bump):** the CSV is
the reproducible boundary, but the est_suitability run that PRODUCES the CSV is
typically NOT reproducible from the committed package. v4.0 ("G", commit 71e4b4b4)
was produced by an untracked `claude/run_psi_G_production.R` on hedgehog with
overrides that match NEITHER est_suitability() defaults NOR canonical model/LAUNCH.R:
include_ai=TRUE (sandbox-built data), fit_date_start=2010 (LAUNCH uses 2015-05-06),
n_seeds=10 (default 5), rw_subsample=5, and a runtime assignInNamespace override of
.psi_fit_predict_rw_cv from ~/psi_cv_fix.R. model/LAUNCH.R:278 calls the LEGACY
est_suitability path (n_splits=0, split_method="random"), not the lstm_v2 rolling-CV
path that produces the shipped psi.

**Why:** psi tournaments run ad-hoc on compute VMs; the winning variant gets hand-
promoted into the CSV.
**How to apply:** when reviewing a config_default psi bump, (1) confirm psi_jt
rebuilds bit-exact from the committed CSV, (2) flag that the CSV's est_suitability
provenance is non-reproducible unless the exact recipe is wired into LAUNCH.R or a
committed data-raw script, (3) check the commits are PUSHED (compute nodes install
from remote -> stale config risk), (4) confirm priors_default psi_star_b regime
still matches the new psi level. Related: [[reviewer-checklist]].
