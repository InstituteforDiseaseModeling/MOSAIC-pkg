---
name: delta-jt-can-exceed-one
description: engine delta_jt = 1/(decay_days_short + f*spread) exceeds 1 whenever decay_days_short < 1 and psi ~ 0; prior allows it (~0.3-0.5% of samples); any consumer asserting delta <= 1 breaks
metadata:
  type: reference
---

`sim_delta_jt()` = 1/(fast + pbeta(psi)*(slow-fast)). At psi ~ 0 (30% of production psi cells are < 0.01)
it equals 1/decay_days_short exactly. The decay_days_short prior is truncnorm(16, 7, a=0.01), so
~0.3-0.5% of calibration samples have short < 1 day, giving delta_jt up to ~2. The engine tolerates it
(sim_params only checks positive; the Poisson decay draw is clamped to W), so nothing upstream notices.

**Why it matters:** a downstream consumer that validates `delta <= 1` (v0.92.0 `.mosaic_reff_infectiousness`)
errors on those members; in a re-simulation over ~115 param sets that is roughly a 40% chance per run of
killing the whole job, because a single failed member aborts the batch.

**How to apply:** anything that consumes delta_jt as a per-day removal fraction should clamp it with
`pmin(delta, 1)` (the mean-field equivalent of the engine's clamp) rather than reject values above 1. Test
with `decay_days_short = 0.5` and a psi block set to 0.
