---
name: ocv-who-gtfcc-request-dedup
description: combine_vaccination_data double-counted ~17M OCV doses because GTFCC rows are per-ICG-request totals while WHO rows are per shipment; fixed by request-key matching (2026-09-30)
metadata:
  type: project
---
GTFCC processed rows (process_GTFCC_vaccination_data) SUM every delivery of a request into one row
dated at the first delivery; the WHO ICG table lists each SHIPMENT (continuation rows repeat the
request number with NA dates). Request numbers align: WHO `20174` = GTFCC `201704` (key "2017:4");
GTFCC `G..` (GTFCC-mechanism) requests become NA.

Measured on model/input CSVs: shipped combined file had 14 WHO_only rows / 18.0M doses; the review's
Step-3 "one WHO per GTFCC" fix raised it to 16 / 19.07M (+1.05M = MOZ 2017-I04 354.6K and CMR
2019-I03 691.2K -- DUPLICATES, verified in ees-cholera-mapping cholera_vacc_requests.csv and raw WHO).
Fix (fix/handoff-h2-priors): request key restricts candidates in every step; Step 4 absorbs further
same-request shipments while cumulative WHO doses <= (1+dose_tol) x GTFCC; decision-date (+/-7d)
fallback for renumbered requests (MOZ WHO 20203 = GTFCC 2020-I02). Result: 2 WHO_only / 1.0M;
total combined 200.35M -> 183.33M doses (COD -5.0M 2023, ZWE -2.3M, ZMB -2.2M, ETH -2.8M, CMR...).

Residual: MWI WHO 20182 (2 x 500.6K, 2018) duplicates deliveries inside GTFCC 2017-G03-D01 (3.24M,
multi-year, dated 2017-08-25) -- cannot be caught at request level; needs per-delivery GTFCC rows
(data-engineer). Same root: GTFCC multi-delivery requests are dated at first delivery, misdating doses.

**How to apply:** after this lands, nu_jt / V1-V2 IC priors must be regenerated; expect lower
vaccine coverage for COD/ZWE/ZMB/ETH/CMR/MOZ.
