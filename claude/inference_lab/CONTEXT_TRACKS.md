# Shared brief — tracks 2 (CFR) and 3 (ensemble subset)

## Read first
- `/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/inference_lab/PROTOCOL.md` (rules, negative controls, hygiene)
- `/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/review_inference/PLAN.md` (the 7-agent review)
- Findings: `/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/review_inference/findings/*.md`

## Code under review
`/private/tmp/claude-501/-Users-johngiles-MOSAIC-MOSAIC-pkg/c26a96f4-20ef-4a2a-8ee4-36b6e863f4c7/scratchpad/lab`
— a git worktree. Branches: `inference-lab` (baseline harness), `arm/A1b_v2` (the promising arm),
`arm/A5`. READ-ONLY; do not edit or commit there. **Do NOT use
`/Users/johngiles/MOSAIC/MOSAIC-pkg`** — that shared checkout is on `feature/psi-torch-port`,
three releases stale, and carries a colleague's uncommitted EMDAT work.

## What the lab has ALREADY MEASURED (do not re-derive; DO challenge)

**A1b** replaced the `-observed * log(1e6)` zero-prediction penalty with a density at an
epsilon-floored mean, `eps_j = max(1e-4, 0.02 * mean(obs_j))`. On ETH n=10,000:

| | baseline | A1b |
|---|---|---|
| bias cases / deaths (all data) | 1.329 / 2.847 | 1.133 / 1.525 |
| R2 cases / deaths | 0.797 / 0.367 | 0.796 / 0.377 |
| OOS R2 deaths | 0.140 | 0.109 |

**THE CENTRAL MEASUREMENT.** 250 draws per arm, stratified across the likelihood range, each
simulated and scored:

| | baseline | A1b | A5 (weekly) |
|---|---|---|---|
| Spearman(LL, R2) | +0.064 | **-0.001** | -0.082 |
| Spearman(LL, abs(bias-1)) | -0.090 | **-0.510** | -0.465 |
| R2 of top-5% LL draws (median) | 0.257 | 0.358 | 0.059 |

Read that carefully. **The likelihood carries essentially ZERO information about R2 in every arm.**
A1b made it 5x more informative about BIAS but not at all about predictive SHAPE. This bounds what
any re-weighting or subset rule can achieve: **a selection rule cannot extract a ranking the
likelihood does not contain.**

**MOSAIC calibration is FULLY DETERMINISTIC** given (config, priors, control, n): `run_MOSAIC.R:288`
sets `seed = sim_id`. Verified max abs diff 0 over 10,000 likelihoods. Replicates require disjoint
draw blocks; the lab branch provides `INFLAB_SEED_OFFSET` for that (default 0 = stock).

**Draw-block instability.** Three disjoint 10,000-draw blocks, best log-likelihoods within 4% of each
other, gave ETH R2_cases of 0.797, 0.019 and 0.371. The blocks are not failing to find good draws --
they find draws the likelihood rates equally that predict completely differently.

**Subset facts (from the review, verified):** `|B|` is a fixed COUNT (~115 = 1.15 x `ESS_best`), not a
percentile, at every n from 500 to 100,000. Weights are two-valued: 114 of 115 sit exactly at the
`pmin(Delta,4)` cap, ratio exactly e^2. `ess_best`/`A`/`CVw` are closed-form in `|B|` and were
bit-identical (15 s.f.) across two completely different datasets. `param_ess` scores a literal
one-hot point mass identically to the real likelihood.

## Resource rules
dugong currently runs 6 calibrations on 150 of 176 cores (ETA ~2.5 h from 2026-09-19). **Do not
launch large dugong jobs until those finish** -- check `ps -eo comm --no-headers | grep -c '^R$'`.
Analysis of EXISTING outputs (`dugong:~/inflab/`, and local
`/Users/johngiles/MOSAIC/output/eth25k_v0903/`) is free. The local laptop has 10 cores.
Never pipe a long Rscript into `head` (R ignores SIGPIPE and wedges).

## Deliverable
Write to `/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/inference_lab/reports/<NAME>.md`. Findings
prefixed `<NAME>-01` etc. with severity, file:line evidence, measured numbers, and a concrete
recommendation. End with **RECOMMENDATIONS** ranked by (expected gain / cost) and **WHAT I COULD NOT
DETERMINE**. State uncertainty honestly; "not measured" beats a guess.
