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

---

## UPDATE 2026-09-17 — the "UNIDENTIFIED" premise is FALSIFIED

Direct OAT likelihood profiling on ETH v0.90.3 ([[param-identifiability-eth-v0903]]) puts
`mu_j_epidemic_factor` in **class A**: 1,270 nats across its prior range, z = 34.9. It is a
strongly identified, deaths-channel parameter -- not "calibration leaves posterior ~ prior".
The posterior looked like the prior because the near-uniform best-subset weighting recovers
almost nothing for ANY class-A parameter, not because the data are silent.

Worse, the direction is wrong: the likelihood keeps improving as the factor goes DOWN, past the
2nd percentile and out to the 1e-5 quantile (0.0066), gaining a further +66 nats -- i.e. the data
want **no epidemic CFR escalation at all**. The v15.18 reshape moved the mode UP from 0 to 0.33.

Do not treat this as settled. Revisit with the profile in hand, and bear in mind the companion
finding that ETH's deaths mis-fit is a TIMING problem (best draw over-predicts total deaths 5.16x
while the deaths LL still wants a higher CFR_target), so the epidemic multiplier may be absorbing
mis-located deaths rather than a real IFR escalation.


---

## UPDATE 2026-09-19 — the +50% anchor is UNCITED and points the WRONG WAY. Recommend Gamma(1,8).

Full evidence: `MOSAIC-pkg/claude/inference_lab/reports/CFR-MATH.md` (CFR-MATH-03).

**1. The literature anchor does not exist.** `04-model-description.Rmd:1017` says "reflecting the
approximately 50% increase in cholera CFR typically observed during outbreak surges" with **NO
citation**, and `make_priors_default.R:1879-1881` cites that spec section. Every "literature +50%
surge anchor" statement in this file and in the v15.18 build note traces to an uncited sentence.
The spec ALSO still prints `Gamma(1,2)` (:1020) while the shipped prior is `Gamma(3,6)` — stale.

**2. The engine's indicator selects the periods with the LOWEST observed CFR.** The flag fires on
high symptomatic PREVALENCE (`sim_components.R:181-186`). Applying each country's own
`epidemic_threshold` from priors v15.18 to the weekly combined surveillance file (2014-2026, AI
rows excluded), converting weekly cases to prevalence with the engine's own identity:

| min weekly cases | k | pooled epi/endemic CFR ratio (DerSimonian-Laird) | implied eps | countries >1 |
|---|---|---|---|---|
| 1 | 10 | 0.489 [0.316, 0.758] | -0.51 | 1/10 |
| 20 | 9 | 0.538 [0.328, 0.883] | -0.46 | 1/9 |
| 100 | 3 | 0.455 [0.268, 0.771] | -0.55 | 0/3 |

Robust across case floors. ETH alone (best series): epi/endemic 0.94, quasi-Poisson log-CFR slope
on log weekly cases **-0.062 (se 0.036, p=0.083)**. Caveat: low-incidence weeks can carry deaths
belonging to earlier high-incidence weeks, biasing this DOWN — but it survives a 100-case floor
where that artifact is weakest. No stratum supports +0.5.

**3. Where the escalation actually lives: the EARLY phase.** First 6 weeks of an outbreak vs weeks
7+ (runs separated by >=8 weeks), DL pool over 20 countries:
**ratio 1.129, 95% CI [0.860, 1.481], 10/20 above 1.** Strongest SDN 3.09, CMR 2.68, SSD 2.14;
reversed ZWE 0.34, ETH 0.45, MOZ 0.54, ZMB 0.54, NGA 0.66.

**Biology (the real citations, use these):** cholera CFR is a function of TREATMENT ACCESS, and
the documented pattern is a DECLINE over an outbreak as the response scales, not a rise at peak
prevalence. Haiti 2010-12: ~4-5% in the first weeks falling to ~1% within months
(Barzilay et al. NEJM 2013;368:599-609, doi:10.1056/NEJMoa1204927; Tappero & Tauxe EID
2011;17:2087-93, doi:10.3201/eid1711.110827). Yemen 2016-18: **0.95% in the first wave vs 0.22%
in the much LARGER second wave** (Camacho et al. Lancet Glob Health 2018;6:e680-e690,
doi:10.1016/S2214-109X(18)30230-4) — higher prevalence, 4x lower CFR, the exact opposite of what
the engine's indicator encodes. WHO/GTFCC treated target <1% vs untreated "up to 50%".

**RECOMMENDATION: `Gamma(shape=1, rate=8)`** — mean 0.125, mode 0, median 0.087, p95 0.374,
p97.5 0.461, p99 0.576. Matches the measured early/late anchor 1.129 [0.860, 1.481] almost
exactly. Mode at 0 is now the HONEST shape (the earlier "mode must be >0 for a flagged epidemic
tick" argument in this note assumed the flag means outbreak ONSET; it means high prevalence, where
the CFR is if anything lower). If a mode >0 is still wanted, `Gamma(1.5, 12)` (mean 0.125,
mode 0.042, p95 0.326). DM owns the 0.125 centre and the [0, 0.48] interval; STAT owns the fit.

**Measured effect (paired CRN, 115 ETH posterior members):** `Gamma(1,8)` alone gives aggregate
deaths **x0.793**, median member deaths bias 1.323 -> 1.107, **dR2 = -0.0001 +/- 0.032** (no R2
effect). **With B2.2 already applied the level effect is NIL** (B2.2 alone x0.6959, B2.2+Gamma(1,8)
x0.6973) — ship it for correctness, not for bias. See [[project-prod-deaths-bias-b2-epi-gap]].

**Do NOT re-key the indicator to outbreak phase (an engine change).** The early/late signal is
+13% [-14%, +48%] with 10/20 countries reversed — too weak and too heterogeneous to justify it,
and B2.2 makes the current indicator harmless for the deaths level.
