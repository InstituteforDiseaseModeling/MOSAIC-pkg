---
name: mu-j-epidemic-factor-prior
description: mu_j_epidemic_factor (epidemic IFR multiplier) prior re-shaped Gamma(1,2)->Gamma(3,6) in priors v15.18 to thin the implausible/unidentified tail; biological derivation + rationale
metadata:
  type: project
---

**FINAL DECISION (IMPLEMENTED, priors_default v15.18, 2026-06-26): Gamma(shape=3, rate=6)**
— mean 0.5 (literature +50% surge anchor KEPT), mode 0.33 (>0), p95 ~1.05, p99 ~1.40. JOINT
STAT+DM pick: DM (this note) first preferred Lognormal(log(0.4),0.5) for mode>0, but Gamma(3,6)
satisfies every hard constraint (center 0.5, mode>0, thin tail p99<=1.4) while staying in the Gamma
family STAT preferred (it is the "reduce variance, keep family" option). Applied to all 40
per-location entries via SURGICAL rebuild on the v15.17 artifact; make_priors_default.R updated for
the next full rebuild. RECALIBRATION-GATED. DM biological reasoning below stands as the rationale.

`mu_j_epidemic_factor` = proportional rise in IFR during epidemic-flagged ticks. Engine:
`mu_jt = mu_j_baseline*(1+mu_j_slope*t)*(1 + mu_j_epidemic_factor*flag)`. Per-location prior in
priors_default; defined in make_priors_default.R ~L1877-1894. Canonical meaning:
04-model-description.Rmd `{#case-fatality-rate}` (mu_{j,epi}, eq Gamma(1,2), L1017-1021).

**Current (v15.17): Gamma(shape=1, rate=2)** — mean 0.5, median 0.347, mode at 0, p90 1.15,
p95 1.50, p99 2.30. Statistically UNIDENTIFIED (posterior ~= prior). The heavy exponential
tail lets the deterministic medoid draw implausible multipliers (MOZ 1.68 ~p93 = epidemic IFR
2.68x baseline; ETH 1.02 ~p87 = 2x) that inflate deaths bias; likelihood can't pull them back.
CD brief: claude/diagnose_fit/epi_lever_exploration/BRIEF_epi_levers.md.

**Biological anchor for epidemic IFR escalation:**
- Endemic/treated cholera CFR target <1% (GTFCC/WHO; well-run CTC <1%).
- Epidemic surges with treatment-access breakdown historically reach 2-5% CFR, i.e. roughly a
  2-5x rise off a <1% treated floor in the WORST documented settings (early Haiti 2010, Yemen
  early-2017, fragile/conflict outbreaks). But those are the EXTREME upper tail, not the typical
  surge. The spec's stated anchor is "approximately 50% increase typically observed during
  outbreak surges" (=epi_factor 0.5). A +50% CENTER is defensible.
- The current prior's tail (epi_factor 1.5-2+ => IFR 2.5-3x) IS biologically reachable in the
  worst conflict/access-collapse outbreaks, but it is NOT a typical surge and the model already
  has OTHER levers for those settings (chi_epidemic switch, low rho). The epidemic_factor should
  encode the TYPICAL surge escalation, not the catastrophic tail.

**RECOMMENDATION (proposal): keep center ~0.5 but thin the tail; prefer Lognormal so mode>0.**
- Biologically, an epidemic regime by definition raises mortality (overwhelm + naive severity),
  so mode should be >0, NOT at 0. Gamma(1,.) mode-at-0 asserts "epidemic most likely adds zero
  excess IFR" which is biologically wrong for a flagged epidemic tick.
- Recommended: **Lognormal(meanlog=log(0.4), sdlog=0.5)** -> mode>0, median 0.40, mean 0.45,
  p95 0.91, p99 1.28. Keeps the +40-50% central escalation, allows a real upper tail to ~1.0
  (2x IFR) for bad outbreaks, but cuts the >1.5 catastrophic draws that were medoid artifacts.
- If staying in Gamma family: Gamma(3,6) (mean 0.5, mode>0, p95 1.05, p99 1.40) is the
  honest-mode alternative. Do NOT use Gamma(1,.) (mode 0) or the CD-floated Gamma(2,8)
  (mean 0.25 is too low — undercuts the documented +50% surge anchor; that's a STAT
  variance-reduction choice, biologically it erases the epidemic signal).
- Per-country: a SHARED prior is fine. Health-system fragility differences are real but
  unidentified per-country here and already partly absorbed by per-country mu_j_baseline and
  the chi_epidemic switch. Don't split the epidemic_factor prior by setting.

STAT owns sizing/identifiability and the variance question; DM (this note) owns the
center/upper-bound plausibility and the mode>0 argument.
