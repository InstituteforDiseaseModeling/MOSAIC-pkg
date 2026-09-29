---
name: reff-route-split-review
description: Post-merge audit of PR #126 route-decomposed R_eff (v0.92.1) - verified-clean list, cross-repo spec-sync gap, lazydata skip-guard trap
metadata:
  type: project
---

PR #126 (merge 96096a659, v0.92.1) rewrote calc_Reff into R_eff = R_hum + R_env. Audited 2026-09-28.

Clean: no in-package or sibling-repo consumer of reproductive_numbers / peak_Rt outside the 3 R_eff files
(only claude/ scratch); all 17 helpers in calc_Reff.R have callers; direct path delta (sim_params) ==
resim path delta (results$delta_jt) at lag 0 with varying psi; runs end to end on a real ETH output in ~2 s.
Six R_eff test files take ~19 s total (12 s of that is the unrelated lasik file).

**Gap found:** the MOSAIC-docs 04-model-description.Rmd route rewrite that calc_Reff.R cites as canonical
theory was UNCOMMITTED in MOSAIC-docs working tree; committed main still called the moment-matched Gamma
kernel canonical. **How to apply:** on any math PR that cites the spec, run `git -C MOSAIC-docs status`
and check the cited equations exist at HEAD, not just in the working tree.

**Trap:** `skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))` is FALSE when the
namespace is loaded but not attached (lazydata is not in the namespace env); TRUE under load_all and
library(). Guard is effectively dead but would silently skip engine tests in a loadNamespace-only runner.
Pattern also in test-env-dose-response.R, test-sigma-split.R. Related: [[reviewer-checklist]].
