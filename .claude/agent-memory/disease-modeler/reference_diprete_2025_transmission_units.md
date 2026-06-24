---
name: diprete-2025-transmission-units
description: DiPrete et al. 2025 cholera transmission-unit per-country membership (phy + HMM) read off the retrieved PDF figures; resolves the East-vs-South + Horn boundary
metadata:
  type: reference
---

# DiPrete et al. 2025 — cholera transmission units (per-country)

"Defining Epidemiologically Relevant Units of Cholera Transmission in sub-Saharan Africa."
DiPrete, Perez-Saez, Wohl, Matteson, Kim, Azman, Lessler. medRxiv 2025.06.06.25329161 v1 (posted 2025-06-07). CC-BY-NC. Gates grant INV-044865.

## Retrieval
- medRxiv HTML/PDF is Cloudflare-403, BUT the **Unpaywall mirror** of the PDF works:
  `https://www.medrxiv.org/content/medrxiv/early/2025/06/07/2025.06.06.25329161.full.pdf`
  (found via Europe PMC core API `fullTextUrlList`, site=Unpaywall). 15-page main text + figs; supplement (Methods, Figs S1-S7, Tables S1-S3) NOT in this PDF.
- Code/data repo: stated "will be made available on GitHub prior to publication" — NOT yet posted (searched HopkinsIDD + GitHub repo search, nothing). So per-country table is NOT downloadable; membership below is read off **Fig 3 heatmap boxes** (PDF p.13) — authoritative within the paper but visual.

## Two analyses, two clusterings
**Phylogeographic (phy):** 4 units. **HMM:** 3 units. Both collapse to the SAME 2 macro-clusters:
Western (phyW1+phyW2 / hmmW) vs Central+Eastern+Southern (phyCE+phyS / hmmCE+hmmS).

### Phylogeographic units (Fig 3A/3B)
- **phyW1:** Liberia, Cote d'Ivoire, Guinea, Guinea-Bissau, Mali, Mauritania, Senegal, Sierra Leone, Benin
- **phyW2:** Burkina Faso, Togo, Nigeria, Cameroon, Ghana, Chad, Equatorial Guinea, Niger, Rep. of the Congo, Central African Rep., Sao Tome (+ Gabon)
- **phyCE:** Somalia, Ethiopia, Djibouti, Rwanda, Sudan, South Sudan, Uganda, DRC, Burundi, Kenya, Tanzania, Zambia, **Angola**
- **phyS:** Comoros, Madagascar, Malawi, Mozambique, Zimbabwe, South Africa, Eswatini, Lesotho, Botswana, Namibia

### HMM units (Fig 3C/3D) — the preferred model (full incidence 1970-2023)
- **hmmW:** all Western + Central-West (phyW1 ∪ phyW2 merged into one)
- **hmmCE:** Somalia, Ethiopia, Djibouti, Rwanda, Sudan, South Sudan, Uganda, DRC, Burundi, Kenya, Tanzania, **Zambia**
- **hmmS:** **Angola**, Comoros, Madagascar, Malawi, Mozambique, Zimbabwe, South Africa, Eswatini, Lesotho, Botswana, Namibia
- Only phy→HMM difference (per text + confirmed in Fig 3D boxes): Angola, Madagascar, Comoros assigned to S (hmmS) rather than CE. **Zambia stays in CE in BOTH.**

## Key quantitative anchors (correct these in our docs)
- within:between transition rate (phylo) = **13.8x** (95% HPD 10.8, 17.4)
- lineage-movement within:between unit (HMM) = **8.5x** (95% CI 6.9, 10.7)  ← our report's "8.5x" is CORRECT, it's the HMM figure
- same-unit countries 10.7x more likely to see a subsequent outbreak within 1 yr; arrival 3 yr sooner
- CE↔S coupling > either↔W: intro in hmmCE 4.5x more likely to seed hmmS than hmmW
- distance: +1000 km → 88% drop in supported transition prob (OR 0.12)
- Sudan & South Sudan GEOGRAPHICALLY COMBINED in HMM (pre/post-2014 disentangling problem) — so SSD's unit = Sudan's by construction, both CE.
- DRC east/west known semi-independent (their own limitation note, ref 23 Taty/Bompangue) — country-level masks this.

## Reconciliation with our regional-clustering report
- **East-Great-Lakes (COD/BDI/RWA/UGA/KEN/TZA) and Southern (MOZ/MWI/ZMB/ZWE/ZAF) are in the SAME macro-cluster but DIFFERENT units** (hmmCE vs hmmS). This **confirms** our two-region split at the unit level.
- **Horn (SSD/ETH/SOM + Djibouti) falls in hmmCE — the SAME unit as the Great Lakes.** This CONTRADICTS the "Horn is partly Red-Sea-facing / its own sub-region" framing: at country-level connectivity, DiPrete puts ETH/SOM/DJI/SSD firmly with COD/KEN/TZA. So our "include the Horn" (Option B) is supported as ONE unit, not a separate hub. (Caveat: their Djibouti inclusion in CE softens our ERI/DJI "Red-Sea exclude.")
- **East-vs-South boundary:** ZAMBIA is CE (not S) in both analyses — relevant since ZMB borders the Great Lakes. MWI/MOZ/ZWE are S. So the boundary runs ZMB(CE) | MWI/MOZ/ZWE(S). TZA is the strongest CE→S seeder (0.7 expected cross-unit outbreaks/yr into S vs KEN's 0.2) = quantitative backing for "TZA is the bridge to the southern corridor."
