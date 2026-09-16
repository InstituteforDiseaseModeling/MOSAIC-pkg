---
name: coiled-steer-drifts-across-skills
description: The Coiled/VM operational story is split across 4 skills (dugong-run, hedgehog-run, run-mosaic, forecast-cv) + 2 memories; they drift apart — audit all together
metadata:
  type: feedback
---

The MOSAIC Coiled-hybrid story is the most drift-prone operational fact in the context surface. It
lives in FOUR skills (`dugong-run`, `hedgehog-run`, `run-mosaic`, `forecast-cv`) plus two user
memories (`project_coiled_hybrid_smoke_validated`, `project_per_location_alpha1_outcome`). They drift
independently and contradict each other.

**Why:** the Coiled facts evolved through stages — (1) "#113 INVALID, use PSOCK", (2) "#113 RESOLVED
for matched runs, valid with parity check", (3) "valid at smoke scale but the WAN client↔scheduler
connection DIES ~1h in → any run >~50min must use dugong LOCAL PSOCK". Each stage was patched into
some files but not all. As of 2026-06-26: `forecast-cv` had the >50min-death steer but `dugong-run`
and `hedgehog-run` (the PRIMARY VM run skills) did NOT — they only say "PSOCK RECOMMENDED" without
the WHY, so an operator reaching for the run skill never learns the long-run failure mode.
`hedgehog-run` still had a stale `### Hybrid variant (only once #113 is fixed)` header.

**How to apply:** when auditing this repo, grep ALL of `.claude/skills/*` for `coiled|psock|#113|
50 min|reconnect|parity|laser-cholera/releases` in ONE pass and reconcile them against the current
state, never file-by-file. The single source of truth for the WAN-death fact is
`project_per_location_alpha1_outcome` (>50min → local PSOCK). Also: laser-cholera version strings
(`environment.yml` pin + every `releases/download/vX.Y.Z` URL in dugong-run §1a) rot one patch at a
time — verify against `inst/python/environment.yml` every audit. See [[skill-constants-rot-despite-hedge]].
