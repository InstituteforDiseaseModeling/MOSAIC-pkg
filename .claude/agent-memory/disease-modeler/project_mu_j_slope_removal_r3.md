---
name: mu-j-slope-removal-r3
description: CFR restructure R3 - mu_j_slope is inert (measured gm 1.0034, below MC noise) and the "+/-30% per draw" claim is falsified; removal is bit-identical because every shipped config has slope=0
metadata:
  type: project
---

`mu_j_slope` (the linear-in-time mortality trend, engine term
`(1 + mu_j_slope * tick/nticks)` in `R/sim_components.R`) was removed in CFR
restructure step R3. Measurements below are mine, taken 2026-09-24 in the
`feature/cfr-restructure` worktree at base v0.94.0.

**Why:** unchanged from the evidence base — not estimable (~74,000 deaths needed
for posterior shrinkage 0.5, COD has 4,139, all 40 pooled ~15,600; measured
posterior/prior SD 0.969), mixed-sign in the data (3/21 countries significant),
no secular trend in the literature (WHO Yemen-excluded series flat at 1.7/1.4/1.5%
for 2017/2019/2020), and double-counts the `s(year)` smooth inside `CFR_target`.

**Stage-1 inertness gate (PASSED).** 5 national medoids (ETH/COD/MOZ/KEN/NGA) x
24 accepted parameter draws x 8 seeds/arm. Arms paired at the PARAMETER level
(one base draw, only `mu_j_slope` differs) because `rbinom` rejection sampling
desynchronises the RNG, so matched seeds are NOT matched trajectories.
- deaths drawn-slope / slope-0: **gm 1.0034, 95% CI [0.9991, 1.0077]**, sd(log) 0.0239
- pure MC noise floor (same params, disjoint seeds): sd(log) **0.0331** — i.e. the
  entire effect of the term is smaller than one-simulation noise
- cases pooled ratio 1.0000
- the term explains **0.05%** of across-draw deaths-level variance

**CORRECTION — two claims in the evidence base are falsified.**
1. "Injects +/-30% of uncontrolled deaths level per draw" is NOT reproducible. The
   sweep behind it (ETH deaths bias 1.18 -> 1.95) ran the slope out to ~+/-1.2,
   which is **24 prior SD**. Inside the real N(0, 0.05) 95% interval (+/-0.098) total
   deaths move only [0.952, 1.040] at COD and [0.961, 1.039] at ETH — **+/-4%**.
   Mechanism: `log(deaths ratio) = 0.403 * slope`; the death-weighted mean
   `t_factor` is 0.40, not 1.
2. The predicted ">=20% narrowing of the deaths-bias IQR" does not happen: measured
   -2.6% to +1.6% across the five countries, i.e. noise.
   So removal is justified as deleting **dead weight** (40 sampled dimensions
   carrying ~0.01 nats), NOT as removing a large level injection.

**Removal is bit-identical, and the golden fixtures did NOT need regenerating.**
The brief expected them to fail. They do not: `config_default` and all four
`replay_*` oracle fixtures have carried `mu_j_slope = 0` for every location since
the field existed, so `(1 + 0*t) == 1` exactly. Verified over 26 scenarios / 722
channel digests in BOTH engine modes (`rng` and `replay`), at 40 and 1 patches,
including the 1,398-tick full-length fixture: 25/26 scenarios bit-identical. The
1 that differs is a deliberate non-zero-slope sentinel proving the harness was not
blind. This matters because the replay fixtures are frozen recordings of the
read-only Python `laser-cholera` engine and cannot be regenerated without it.

`t_factor = tick/nticks` had exactly one reader (this term) and went with it.

See also [[reference_cfr_mu_j0_identity]], [[project_prod_deaths_bias_b2_epi_gap]],
[[project_r2_pins_rho_deaths_delta]].
