---
name: r2-pins-rho-deaths-delta
description: CFR-restructure R2 pinned rho_deaths=0.42 and delta_reporting_deaths=5; 4-arm gate proves rho_deaths cancellation (0.3-1.3% vs 6-27% half-changes); deaths run 1-28d LATE at every delta so the fit wants a lag below the prior's lower bound
metadata:
  type: project
---

Shipped in `feature/cfr-restructure` (worktree, uncommitted at time of writing), priors_default **v15.19**.
Two global parameters moved from sampled to pinned. Values unchanged — only their status.

- `rho_deaths` = **0.42** — `R/sample_parameters.R:178`, `R/run_MOSAIC.R:3566`
- `delta_reporting_deaths` = **5** — `R/sample_parameters.R:198`, `R/run_MOSAIC.R:3568`

Both priors RETAINED in `priors_default` as the literature record / source of the point values /
sensitivity-run distributions. Set the flag TRUE to re-enable a draw.

**Why:** `rho_deaths` cancels identically under B2 (mu_j_baseline ∝ 1/rho_deaths, engine thins by
rho_deaths) — see [[reference_cfr_mu_j0_identity]] and [[reference_inert_params_and_prior_defects]].
`delta_reporting_deaths` has no observational anchor: deaths and cases share the WHO bulletin row.

**How to apply:** do not re-introduce a "narrow rho_deaths prior pins the product mu·rho_deaths"
argument — that rationale is PRE-B2 and was retired in v15.19. There is no product left to pin.

---

## The 4-arm gate (reusable design) — rho_deaths

Matched seeds do **not** give matched trajectories when any parameter changes: `rbinom` uses
rejection sampling and consumes a variable number of uniforms, so the RNG stream desynchronises.
Treat arms as UNPAIRED and raise n. (A 12-seed "paired" first pass showed a spurious ETH +7.5%,
z=+3.6; at 48 seeds it was +1.2%, t=0.67.)

Four arms at each national medoid (`output/full_metapop_nmme/national/<ISO>/2_calibration/best_model/config_medoid.json`),
48 seeds each, total `reported_deaths`:

| arm | mu | rho_d | ETH | SOM | ZWE | COD | NGA |
|---|---|---|---|---|---|---|---|
| A pre | mu0 | r0 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| **B post (the pin)** | mu0·r0/.42 | .42 | **1.012** | **1.009** | **0.997** | **1.004** | **0.998** |
| C mu-only | mu0·r0/.42 | r0 | 1.028 | 0.795 | 1.141 | 0.940 | 1.036 |
| D rho-only | mu0 | .42 | 0.984 | 1.272 | 0.873 | 1.069 | 0.965 |

B: |t| ≤ 2.4. C and D: |t| 21–43. **C × D = 1.00 ± 0.01 in every country** — the cancellation shown
as the product of two individually 20–40σ effects. Sweep control: rho_deaths 0.25→0.65 with
re-derivation spans 1.02–1.12×; with mu FROZEN it spans 2.60–2.77× (≈ the 2.6× sweep itself).

`rho_deaths` never touches the case channel (B_post cases are bit-equal to C_mu_only cases).

---

## delta_reporting_deaths — it is NOT inert, and the timing evidence is uncomfortable

Gated on shape (WIS via `MOSAIC:::.compute_wis_from_quantiles`, weekly deaths, 40 reps/arm).

SAMPLED (per-draw TruncNorm) → PINNED(5) WIS: ZWE −1.2%, COD −0.02%, SOM −0.04%, NGA +1.0%,
ETH +9.3%. Median −0.02%; peak-timing error unchanged. **ETH's +9.3% is a predictive-SPREAD
artifact**, not timing: mixing over delta widens weekly quantiles, and ETH under-predicts 2.6× with
CCF r = −0.04, so any extra dispersion flatters WIS. Its peak-timing error is identical in both arms
and its own fixed-delta profile is flat (3.58–3.63 across delta 1→14).

**The real finding (flagged, deliberately NOT acted on):** the CCF-optimal alignment lag is
NEGATIVE at every delta in every country — predicted deaths run **1–28 days LATE**.
COD −13→−24 d, NGA −18→−28 d, ZWE −1→−16 d, SOM −24→−28 d as delta goes 1→14 (the lag tracks
delta ~1:1, so the parameter does rigidly shift timing). WIS is weakly monotone increasing in delta
in 3/5 (COD +2.6%, NGA +3.6%, ZWE +4.9% over 1→14). **The fit wants a delay at or below the prior's
lower bound of 1 day.**

**Why I did not re-centre on that.** It would encode a biologically implausible same-day
death-to-report (the a=1 bound exists precisely because registration + notification + weekly
aggregation cannot complete same-day), and it buys ≤4 days against a 13–28 day gap. The gap is the
~19-day structural infection-to-reported-death dwell — see [[reference_deaths_channel_limits]] and
[[reference_deaths_embargo_dwell]]. Absorbing it into an administrative reporting lag would hide the
dynamics defect. Left for the deaths-timing workstream.

5 days is justified as: rounded truncated median (4.60) and mean (4.86) of TruncNorm(4,3,[1,14]);
midpoint of the 3–7 day IDSR death-to-report window (Routh 2017 Tanzania, Bwire 2013 Uganda);
and mechanically ~3.5 d mean wait to a weekly cycle end + 1–2 d compilation.

Harness scripts (local laptop, session scratchpad — ephemeral):
`.../scratchpad/r2_gateA.R`, `r2_gateA2.R`, `r2_gateB.R`, `r2_gateB_timing.R`.
