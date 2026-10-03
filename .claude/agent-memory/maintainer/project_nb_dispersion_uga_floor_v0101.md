---
name: nb-dispersion-uga-floor-v0101
description: est_nb_dispersion returns the 0.1 floor on sparse short-outbreak series whatever the true k (censoring, not an estimate); RESOLVED in v0.101.0 (c939095fc) - clamped_lower_bound fits take the panel trend at every scale, so UGA's cases k is 0.964 (was 0.100-0.108) in all 4 v1.0 suite scopes
metadata:
  type: project
---

On config_default v6.1 (observed weeks via reported_tier, burn-in 45) UGA has 84 tier-1 weeks, 74 of
them zero; glm.nb fails, theta.ml fallback gives 0.047 corrected, clamped to 0.1 (status
clamped_lower_bound, row shows rung 1 / df 4 / identified TRUE). Resolved k before the fix: 0.100
national, 0.100 eastern_eth (4 fittable < 5, so no shrinkage), 0.105 central_cod, 0.108 continental.
The panel trend at UGA's level gives 0.964; v6.0 gave it 1.00.

**Why it is an artifact:** a synthetic UGA-like test (103 weeks, 6 short outbreaks, outbreak cores
dropped as tier 2) returns median 0.100 for true k = Inf, 5 and 1 alike, and the UGA estimate moves
0.1 (df 2-6) -> 0.24 (df 8) -> Poisson (df 12). With k = 0.1 the weekly cases score is nearly blind to
level (a 2x error costs 0.5 nats over 103 weeks; 4.6 at the trend k) and the observation-level median is
0 whenever the weekly mean is <= 102.3. ZAF (14 observed cases) and CIV (4 non-zero weeks) fall below the
evidence minimums and take the trend at about 1, so the pre-fix routing rule was
discontinuous: less data got k about 1, slightly more got 0.1.

**Resolution (c939095fc, 2026-10-02, in the 0.101.0 release):** routed. A fit with status
`clamped_lower_bound` now takes `.NB_DISP_PANEL_TREND` at every scale, as a no-estimate fit does; the row
keeps `status`/`k_raw` (the fit) with `panel_trend = TRUE` (the k used). A routed location never enters
shrinkage, so only UGA's cases k moved (every other cases row and every deaths row identical; deaths take
no trend, so ZAF's clamped deaths k stays 0.1). Trend constants and drift test unchanged (the trend fit
already excluded clamped fits); the panel-test assertions were updated; likelihood tag
`R/v0.101.0+clamped_k_trend`.

**How to apply:** treat a k at the 0.1 bound as censoring, never as a measurement. Any change to how k
is resolved needs a likelihood impl-tag bump (the resolved k is not in control.json). Related: [[glm-nb-profile-convergence-trap]], [[nb-dispersion-review-v092]].
