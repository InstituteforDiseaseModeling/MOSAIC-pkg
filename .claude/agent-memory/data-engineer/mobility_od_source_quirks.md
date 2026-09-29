---
name: mobility-od-source-quirks
description: Field-semantics traps in the four overland-mobility OD sources (UN DESA IMS 2024, Abel-Cohen 2022, Meta SCI country.csv, ADM0 contiguity) — the Namibia "NA" read.csv trap, da_min_closed sparsity, DESA column 15, and the Kazungula false border
metadata:
  type: reference
---

Verified 2026-09-18 against the actual downloads in
`MOSAIC-data/raw/mobility_od/snapshot_2026-09-18/`.

**Meta SCI `country.csv` — the Namibia "NA" trap.** Namibia's ISO2 is the
literal string `NA`. `read.csv(f)` with default `na.strings = "NA"` silently
converts all 178 Namibia rows (as user_country) + 178 (as friend_country) to
missing. `countrycode("NA","iso2c","iso3c")` is fine and returns `NAM` — the
loss is entirely in the CSV reader. **Always pass `na.strings = ""`** to any
reader of an ISO2-keyed file. Same trap applies to any ISO2 file (NA=Namibia)
and, for ISO3 files, there is no equivalent (NAM is safe).

**Meta SCI semantics.** `scaled_sci` is EXACTLY symmetric (verified: max
|SCI_ij - SCI_ji| = 0) and is min-max scaled to [1, 1e9]. It is
conn_ij / (users_i * users_j) — the destination's user base is DIVIDED OUT, so
row-normalising it allocates mass proportional to connections-per-destination-user,
not to destination choice. Measured: within-row Spearman(log share, log N_dest)
= **-0.06 for SCI** vs +0.38 for DESA. ZAF's mean inbound share is 0.026 under
SCI vs 0.144 under DESA (5.5x deflation of the largest destination); tiny GMB is
inflated. SCI as a destination-allocation kernel removes the population pull that
a gravity omega is supposed to supply.
HDX resource IDs: `652cf9c9-...` = country.csv (563 KB), `8e1b8b59-...` = gadm1.csv
(273 MB). HDX `dataset_date` is 2025-12-26..2026-01-25 and HDX lists the licence
as CC0 (`other-pd-nr`), not CC BY-NC.

**Abel & Cohen 2022 (`bilat_mig_sex.csv`).** `year0` is the PERIOD START; only
1990/1995/2000/2005/2010/2015 exist, so `year0 >= 2015` isolates 2015-2020 and
cannot pull anything later. Values are **5-year period totals**, not annual.
Only `male`/`female` rows exist (no total), so summing sexes is required — but
the paper notes the sex-specific estimates do NOT exactly sum to the total.
**Estimator choice matters a lot:** the authors find `da_pb_closed`
(pseudo-Bayesian) performs consistently best; the `da_min_*` minimisation
estimators are the sparse minimum-flow solution (paper: 0.59-0.62 zeros vs 0.33).
Measured intra-MOSAIC-40 for 2015: `da_min_closed` 1316 nonzero rows vs
`da_pb_closed` 2450. Under `da_min_closed`, **AGO / GNQ / SOM have ZERO intra-40
mass** and **NAM has exactly one cell: NAM->SEN = 0.22 persons**, which
row-normalises to 1.00.

**UN DESA International Migrant Stock 2024 (Table 1).** Header is on row 11
(`skip = 10`). Layout: col 2 = destination name, col 5 = destination M49,
col 6 = origin name, col 7 = origin M49, cols 8-15 = 1990..2024 both sexes,
16-23 male, 24-31 female. **Column 15 is 2024 both-sexes** (verified: CIV<-BFA
1,820,882 = male 1,097,934 + female 722,948). readxl repairs the duplicate year
headers to `2024...15 / ...23 / ...31`. The merged group-label ranges (H8:N10,
Q10:V10, Y10:AD10) do NOT delimit the blocks and must not be used to infer them.
Table is **destination-major**: a row (dest D, orig O, v) = v people born in O
residing in D, so `M[origin, destination] <- v`.
**Only 514 of the 1560 possible intra-MOSAIC-40 directed pairs exist as rows at
all** (507 positive, 7 exact zero, no `..` cells) — absent is not zero.
Sanity anchor reproduced exactly: intra-west-10 = 68 pairs / 5.64M persons.

**ADM0 contiguity via `sf::st_distance` < 10 km.** For the MOSAIC-40 this yields
84 undirected pairs; the true land-border count is **83**. The extra is
**NAM-ZWE**, whose polygons touch at *exactly* 0 m at the Kazungula quadripoint —
so it is a polygon-topology artifact, NOT threshold-induced, and `st_touches`
would catch it too. The threshold earns its keep on **BWA-ZMB**, a real
(~150 m) border that these polygons leave a 1332 m gap in. Nothing else falls in
(0, 10 km). Real corridors the ADM0 test cannot see: **COD-TZA across Lake
Tanganyika (34 km apart here)** — the E3 design memo required ferry corridors
(Tanganyika / Malawi / Chad / Victoria / Kivu) be burned in.

Related: [[tau-i-is-daily-not-weekly]].
