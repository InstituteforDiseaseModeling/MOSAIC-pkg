---
name: ocv-round-split-provenance
description: GTFCC OCV log semantics (req_id I/G/D, Round vs Delivery doses), the v0.103.0 nu_1/nu_2 split + delivery-dated release, the open IC/nu double count at t0, D01/D02 parallel release, and the vaccination-figure gotchas (prop_vaccinated = doses/capita, hard-coded ZMB window)
metadata:
  type: project
---

GTFCC request log = `ees-cholera-mapping/data/cholera/epicentre/gtfcc/cholera_vacc_requests.csv`
(read-only; last data change 0e13df4, 2025-12-18). Field semantics that bit us:
- `req_id` `YYYY-<I|G|L>NN-DNN`: **I** = ICG request, **G** = GTFCC preventive programme
  (via_tool "GTFCC", multi-campaign, two-dose), **D01/D02** = separate decisions/shipments of
  one request (D02 is often the round-2 shipment, e.g. MOZ 2019-I01-D02 = all R02).
  `process_GTFCC_vaccination_data()` hard-codes `context = "Outbreak response"` even for G
  requests, so "all 2018-22 campaigns are ICG outbreak response" claims are an artifact.
- **Delivery** doses = shipped; **Round** doses (`C##-R##`) = administered, NA = "(Unknown)".
  Duplicate campaign-round rows are reported+NA pairs (ZWE 2018-I11, KEN 2023-I10) -> count once.
  Campaign numbers are chronological; one R02 is dated before its R01 (SSD 2018-I08).
- No MOSAIC-country request delivered from 2023 has an R02 (ICG suspended 2-dose Oct 2022).

v0.103.0 (branch feat/v0103-nu-split, commits 74f97b09b split, 1c8b5dab9 release):
round_sequence/round_basis/delivery_schedule columns; nu_1 + nu_2 = nu exactly; 2023+ unchanged.
The old processing released every delivery from the FIRST delivery date (22 pre-2023 requests,
up to 746 d early; 2.37M doses fell before a 2018 t0: MWI 2017-G03-D01, SSD 2017-G04-D01).

**Open (flagged, not fixed):** `est_initial_V1_V2()` counts pre-t0 Round (else Delivery) doses
in full while nu releases the same request's doses past t0 -> double count. At 2023-01-01
(shipped v6.2 too): MWI 2022-I13 1.8M, CMR 2022-I17 0.94M, KEN 2022-I21 ~0.56M, SOM 2022-I15
0.26M; at 2018: NGA 2017-I14 0.26M. Also MWI 2017-G03 D01 (2020-02-28) and D02 (2020-11-06)
both list a 664.4K delivery: possible duplicate shipment.

D01/D02 of one request are separate rows, so each releases at its own 20k/day and the two add
up in parallel (ZMB 2017-G07 runs at 40k/day over 2018-03-28..04-20). On the combined record the
daily max is 80k. Only deliveries listed in ONE row's delivery_schedule share a stock.

Vaccination figures (checked 2026-10-03): `plot_vaccination_data()`/`plot_vaccination_maps()`
default `data_source = "WHO"`; the model uses "BOTH" (LAUNCH.R passes it).
`prop_vaccinated` = ALL cumulative doses (first + second, every campaign) / 2023 population. It is
not people vaccinated: on the combined record SSD = 140.6%, yet the plots label it
"% vaccinated". The ZMB example window is hard-coded (2024-06-01..08-01), keyed to the WHO
dashboard date of 2024-I03. On the combined record (GTFCC delivery 2024-04-05) it holds no
shipment. Proposed fix (NOT applied to the pkg):
MOSAIC-pkg/claude/v1_docs/patch/plot_vaccination_data.diff, which moves the window to 2023-I09.

**Why:** the 2018-start v1.0 candidate needs pre-2023 OCV history right.
**How to apply:** check `round_basis`/`delivery_schedule` before trusting a request's
dose timing; treat IC + nu dose totals around t0 as non-additive until the double count is fixed.
Engine note for priors work: the daily nu_2 drip re-doses V1 and can move all of a campaign's
V1 to V2, while `est_initial_V1_V2()` pairs once (V2 = phi_2 * min(d2, phi_1 d1)).
