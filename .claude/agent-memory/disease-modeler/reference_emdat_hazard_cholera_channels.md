---
name: emdat-hazard-cholera-channels
description: EPI ranking of EMDAT hydro-met hazards as cholera drivers + verdict on psi/theta hazard-channel redesign (flood-only bug, drought IN as own channel, 2-3 channels not 1)
metadata:
  type: reference
---

Context: psi flood covariate was built ONLY from EMDAT `Disaster Type == "Flood"`, silently
excluding tropical cyclone + storm surge (and never drought/mass-movement). Design question =
which hazards feed psi and merged-vs-distinct signals. EPI consult (2026-07).

**Causal ranking (strongest->weakest):**
1. Flood — first-order, direct: fecal-contaminated surface water -> wells/latrines/piped supply.
   Exactly MOSAIC's beta_env/reservoir pathway.
2. Tropical cyclone — first-order, COMPOUND: extreme-rain flood + storm surge + WASH destruction +
   displacement. MOZ Idai 2019 (~6,600+ cholera cases within ~2wk). BIGGEST real gap (silently
   excluded), bigger than drought.
3. Drought — real but SECOND-ORDER, OPPOSITE mechanism: scarcity -> crowding at unsafe sources,
   storage contamination, reduced hygiene. Horn of Africa 2022. Slow, chronic, partly already in
   low-psi/precip covariates.
4. Storm surge — real, coastal-only: saltwater WASH damage + brackish niche favors environmental
   V. cholerae. EMDAT too sparse (n=2 + folded in cyclones) -> NOT own covariate, part of cyclone.
5. Mass-movement (wet)/landslide — weak/indirect, collinear with flood (same rain driver).
6. Non-cyclone storm/severe weather (lightning/tornado/hail) — mostly NOT a cholera mechanism.
   Exclude wildfire/earthquake/infestation/volcanic/extreme-temp.

**Lag/sign kernels (decisive for merge-vs-split):** they do NOT share one kernel.
- Flood / cyclone-acute / landslide = short-lag (~1-3wk) POSITIVE contamination spikes.
- Cyclone-sustained = SECOND longer POSITIVE tail (+1-3mo, broken WASH + displacement) a single
  flood kernel won't reproduce.
- Drought = long-lag slow-ramp POSITIVE, with early OPPOSITE-sign transient (less flushing);
  mechanistic INVERSE of flood pulse (WASH-degradation vs acute contamination).

**VERDICT — 3 distinct channels, NOT one merged water-hazard, NOT one-per-EMDAT-type:**
1. Acute-flood-contamination = Flood + cyclone-acute + surge + mass-movement-wet MERGED (shared
   short-lag +contamination, collinear). Merging FIXES the silent-exclusion bug cheaply.
2. Drought/scarcity = SEPARATE (opposite lag/mechanism/early-sign; merging destroys both). n=149 ok.
3. (Phase 2) Cyclone-severity/infrastructure tail — under-powered (n=47), keep inside ch.1 for v1.
Separate GAMs -> separate weightable channels so calibration can down-weight per country
(coastal-cyclone vs inland-drought profiles differ; merged signal can't).

**Mechanistic home:** acute hazards are better modeled as TIME-VARYING theta_j(t) reduction (+
reservoir boost) than a psi bump — mechanism is WASH-defeat + contamination, NOT climate
suitability. psi (LSTM) already carries smooth climate; static theta carries static WASH; hazard
channels add the non-periodic event deviation + time-varying WASH degradation. Cross-lane: loop
[[ml-scientist]] (psi/LSTM owner) + theta-pathway owner before plumbing.

**EMDAT blind spots (first-order AFRO drivers EMDAT structurally cannot see):**
- Conflict/displacement (NGA Borno IDP camps, DRC/Goma, Sudan/Somalia) — NO natural-hazard
  trigger; EMDAT is natural-disaster register. Candidate data: ACLED, UNHCR/IOM-DTM.
- Time-varying WASH failure/rollout (only static theta today).
- No exposure denominator (country-level counts) — same flood != equal risk Cape Town vs Beira;
  interact hazard with theta/sub-national WASH.
- Mass gatherings.
