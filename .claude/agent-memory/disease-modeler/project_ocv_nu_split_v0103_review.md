---
name: ocv-nu-split-v0103-review
description: 2026-10-03 review of the GTFCC round split of nu (nu_1/nu_2) + delivery-dated release (v0.103.0, branch feat/v0103-nu-split) - verified claims, engine second-dose semantics, and the pre-existing V1/V2 initial-condition defects it exposes
metadata:
  type: project
---
Reviewed on the laptop (worktree .claude/worktrees/v0103-nu, commits 74f97b09b + 1c8b5dab9); scratch
claude/plan_2018_start/ic_decisions/nu_split/. Verdict: SHIP the split as is; IC issues disclosed, not fixed.

- **Verified:** 2018-22 released 83.99M = 51.91M first + 32.08M second; round basis 87.6% rounds /
  8.5% imputed / 3.9% unknown; nu_2 = 0 over 2023+. Expected-value engine replay: the V1 cap delivers
  84.8% (ZMB) to 100% of nu_2; V1+V2 at end-2022 = 0.754x the old all-first-dose input.
- **Engine second-dose semantics:** the cap applies per day to the whole V1 pool, and the (1 - phi_2)
  share stays in V1 and is re-dosed on later days. So over a multi-day round the engine moves
  min(phi_2*d2, V1) into V2, which is the marginal-VE_2 reading of the phi_2 prior. est_initial_V1_V2's
  phi_2*min(d2, phi_1*d1) applies effectiveness twice: V2 comes out 12-21% low for R2/R1 0.9-1.0, with
  V1+V2 unchanged. Docs 04 :1407 wrongly says the IC does it "as in the engine's vaccination step".
- **Pre-existing V1/V2 IC defects (2018 build, against a release-series replay of the engine):**
  - The documented template priors Beta(0.5,49.5)/Beta(0.5,99.5) put ~8M people into V1/V2 with no
    doses: 27 no-history countries at 1.5% of N, plus 11 with history but no paired R2, where V2
    falls back to 0.5% of N. Real doses account for ~7M.
  - Pairing works within a full req_id, so D01/D02 siblings and split ICG requests are not paired.
    Pairing-only counterfactual at 2018 (IC's own formula, prior-mean phi/omega; 2026-10-03,
    claude/v0103_docs/14_split_requests_exact.R): NGA 2017-I11/I14 +0.66M people, ZMB 2016-I04/I08
    +0.22M. The ~0.69M/~0.27M quoted earlier (and in NEWS 0.103.0) are IC-minus-release-replay gaps,
    which mix in dating/overlap effects; docs 04 carries 0.66/0.22. (Both I11 and I04 R01 dose counts
    are NA, so those requests enter via deliveries as first doses.)
  - The drip double count at t0 is negligible at 2018: NGA's 256.9k are second doses (V1 -> V2 only),
    SSD 37.8k. It is material in the shipped 2023 objects: MWI 2022-I13 1.8M first doses = ~1.41M
    people = 6.8% of N; CMR 0.94M; KEN 0.56M; SOM 0.26M.
  - est_initial_V1_V2 uses config$N_j_initial, so a first-pass 2018 build from the installed 2023 config
    gets props 11-18% low. Pass 2 corrects it, and pass 1 != pass 2 is expected.
- **Data facts:**
  - GTFCC `context` is "Outbreak response" for every row (no consumer), but 15 "G" preventive requests
    carry 33.1M of the 2018-22 doses.
  - The MWI 2017-G03 664.4K shipments under D01 (2020-02-28, completing a 3.2M approval) and D02
    (2020-11-06, its own 676K approval) look distinct.
  - The 20k/day release (update_mosaic_data) is slower than real rounds. Docs 04 now says 20k
    (MOSAIC-docs 6784c23), but figures/vaccination_*.png are still WHO-ICG-only at the 100k cap
    (TZA/UGA show 0 doses) and need regenerating from the combined, delivery-dated record.
  - Shipped v7.0 facts: 13 countries with pre-2018 doses (V1 0.09% ETH to 18.8% SSD); data-based V2
    only NGA 0.34% and ZMB 1.55%; engine replay from config ICs admits 85.9% (ZMB) to 100% of nu_2.
**How to apply:** post-1.0, rebuild est_initial_V1_V2 as an expected-value replay of
sim_phase_vaccinated over the released dose1/dose2 series before t0, with ~0 for locations without
doses. That removes all five defects at once. See [[2018-ic-tier3-quiet-horizon]], [[v0100-rebuild-priors-v17]].
