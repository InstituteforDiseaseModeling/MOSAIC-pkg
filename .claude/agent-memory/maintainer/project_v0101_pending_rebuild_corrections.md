---
name: v0101-pending-rebuild-corrections
description: Wrong metadata$description strings shipped in config_default v6.1 / priors_default v17.1 (md5-pinned, so deferred); fix them and refresh the v6.1-specific roxygen at the next rebuild
metadata:
  type: project
---

At the v0.101.0 release (2026-10-01) two shipped description strings were found wrong. They
were NOT edited, because `config_default.rda` (d2063393...) and `priors_default.rda`
(f70cdbb3...) are pinned by the v1.0 acceptance rubric and the dugong suite provenance.
Editing a builder string without rebuilding leaves the builder unable to reproduce the
shipped rda. NEWS 0.101.0 records both corrections and promises a fix "at the next rebuild"
(pkg commit df8bbbb1e).

Pending for the next rebuild (config v6.2 / priors v17.2):
- `data-raw/make_config_default.R` description: "read by the cases and deaths dispersion
  estimates, which use tier-1 weeks only" is wrong for deaths. The integrated deaths phi
  falls back to every scored week where the tier-1 weeks are too few. On v6.1 at burn-in 45
  (and 30) that is BEN, BFA, CIV, LBR, NAM and ZAF (finding CFG61-1).
- `data-raw/make_priors_default.R` description: "the SDs follow the envelope scaling (ZAF
  0.30 -> 0.14, CIV 0.22 -> 0.14)" has the wrong cause. The fit SE fell; ZAF's envelope
  scale actually rose 0.41 -> 0.46 while its SE before scaling fell 0.52 -> 0.21 (F4).
- `R/config_default.R` roxygen names the v6.1 fallback set ("on v6.1 at burn_in_days = 45:
  BEN, BFA, CIV, LBR, NAM and ZAF"). Re-derive it on the new object: run
  `.mosaic_resolve_deaths_integration()` and list `dispersion_observed_insufficient`.

**Why:** a deferred fix is invisible once the release ships. Only NEWS and this note carry it.

**How to apply:** when reviewing any config/priors rebuild, grep both builders for the two
quoted strings and the roxygen for "v6.1", and REQUEST-CHANGES if any survive. Open
follow-ups from the same review:
- UGA cases k routing (F1): RESOLVED in v0.101.0 (c939095fc), clamped fits take the panel trend
  (UGA 0.964); see [[nb-dispersion-uga-floor-v0101]].
- `model/LAUNCH_sanitized.R` step 4B still has the stale "G" psi recipe and a TODO(parent).
Related: [[reviewer-checklist]].
