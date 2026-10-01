---
name: rebuild-v17-redteam
description: Red-team of priors v17.0 / config v6.0 rebuild (2026-09-30) - reproducible and aligned, but IC/seasonal prior-draw defects and a weak seed hash
metadata:
  type: project
---
Rebuild branch rebuild/defaults-v0100 (MOSAIC v0.100.1): an independent temp-dir rebuild is byte-identical to the committed objects; psi_jt, observed data and weights match their sources exactly. Defects found:

- **Zero-window E/I means no ignition in single-country runs.** est_initial_E_I gives Beta(0.01, 99999.99) when the 3-day window at ic_t0 (2023-02-01, while the sim starts 2023-01-01) has no cases. Measured P(E+I>0) is ~0.08-0.10 for TZA/AGO/GHA (18k/42k/8k observed cases), and 89.5% of TZA smoke draws started unseeded. W starts at 0, so those draws can never produce cases.
- **The seasonal positivity check runs only at the prior means.** 28/40 countries' means sit exactly on the 0.1 floor. Under joint independent draws, 33% have min(1+f) <= 0 (clamped to 0 by the engine), down from 72% at v16.1. NEWS claims a "guaranteed positive envelope", which is not true for draws.
- **`.mosaic_derive_seed` hash has low entropy.** It is sum(k*7919*pos) over the ISO characters, so there are only ~150 possible values for 3-letter codes. 33 unique seeds for the 40 MOSAIC ISOs (ETH/SSD, TCD/UGA, ...): their MC errors are perfectly correlated, though no bias results.
- **Two-route parity test is fragile.** At config v6.0, p_star varies 0.61-0.76 across seeds against a p_beta q999 of 0.727. The test passes only at its fixed seed, and only because the prior changed (fit_beta_from_ci, v0.100.0).

**Why:** checking at the prior mean is not the same as checking the draws. The engine's clamps and ignition requirements act on each sampled draw.
**How to apply:** for any "builder guarantees X" claim, test it on prior draws through sample_parameters. For IC priors, check P(E+I>0) per country against the observed burden.
