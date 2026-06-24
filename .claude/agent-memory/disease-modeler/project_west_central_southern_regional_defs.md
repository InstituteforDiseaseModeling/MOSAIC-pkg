---
name: west-central-southern-regional-defs
description: West/Central/Southern regional metapop membership vectors (fittable B2-Stage-1 only) extending the East/hmmCE pilot; Central resolved away as non-unit; report §7a
metadata:
  type: project
---

# West / Central / Southern regional metapop definitions (§7a)

Extension of the completed East-Africa (hmmCE) regional pilot to the remaining
regional-tier clusters. Genomics-led (DiPrete 2025 hmm units, [[diprete-2025-transmission-units]]),
Weill-corroborated ([[weill-2017-waves-palette]]), case-synchrony-CHECKED (not defined —
per §1 Part B West synchrony is incoherent). Restricted to the **27 B2 Stage-1 fittable**
countries (`claude/full_metapop/b2_vs_baseline_comparison.csv`); the 13 data-thin SSA
countries enter only via the continental model. Report: `claude/regional_clustering/
REPORT_regional_clustering.md` §7a.

**Why:** building regional metapop fits per cluster next, mirroring the EA pilot.
**How to apply:** these are the concrete membership vectors for the regional runners.

## Membership vectors (fittable only)
- **east (hmmCE pilot, reference):** COD BDI RWA UGA KEN TZA SSD ETH SOM ZMB (+COG — see below).
  reservoir = eastern-DRC COD; ZMB ∈ CE both genomic methods; bridge to South = TZA (0.7/yr).
- **southern:** MOZ MWI ZWE ZAF AGO NAM. reservoir = MOZ/MWI/ZWE corridor (WHO DON443 2022-23,
  Weill T11). **Strongest within-synchrony block in SSA (mean lag-0 Spearman ρ 0.477** —
  ZAF-ZMB 0.73, ZAF-MOZ/ZWE 0.68). NAM = weak peripheral node. **Do NOT fold ZMB in** (genomics
  put it East; its corridor synchrony is the common-cause-climate artifact). **DEFENSIBLE — build.**
- **west:** NGA NER TCD CMR BFA GHA CIV BEN TGO LBR. reservoir = NGA/Lake-Chad (NGA 256k cases,
  B2 R² 0.79, only West node with deaths signal). **Case synchrony WEAK (mean ρ 0.079**; only
  strong edge NGA-NER 0.48) — cohesion rests on DiPrete hmmW genomics (8.5×), not data. LBR =
  unidentified dead node (B2 R² 0.14). **MARGINAL — build but flag: network weakly identified,
  per-country β carries the signal, treat LBR + Gulf-of-Guinea tail as weak nodes.**

## Central — RESOLVED AWAY (not a standalone unit)
Congo basin = COG/GAB/CAF/GNQ (+COD). Only **COG is fittable** (rest 0 / near-0 cases, no B2 fit);
COD is already the East reservoir. A "Central" model = 1 fittable patch with its reservoir (COD)
amputated → leaky single patch. **Call:**
- COD → East (done, reservoir).
- **COG → East as weak node.** Deliberate departure from DiPrete (who folds COG into hmmW):
  justified because (a) attaching COG to West amputates its COD reservoir, (b) present-day case
  data agree with Weill not DiPrete — COG's strongest edge is **COG-COD 0.37** (vs NGA 0.20,
  CMR 0.19), matching Weill's Congo-River East-tie (T10, 2011-12).
- GAB/CAF/GNQ → continental model only (data-thin); attach to West if they ever get data.

## Leakiness flags (the deliverable the user asked for)
- **Central = NOT defensible** (no internal reservoir) — do not build.
- **West = defensible but leakiest** — genomics-justified, synchrony ~0; expect poorly-constrained
  spatial network.
- **Southern = cleanly defensible.**
- Weak nodes to down-weight (not reservoir anchors): RWA/UGA (east), NAM (southern), LBR+GoG (west).

Analysis-only; no package source / dugong / pilot touched.
