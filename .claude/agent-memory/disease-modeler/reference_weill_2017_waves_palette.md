---
name: weill-2017-waves-palette
description: Weill et al. 2017 Science cholera 7PET African introduction waves (T1-T12) groupings + Fig 2 T-wave color palette + Fig 3/4 Wave1/2/3 palette, with OA retrieval route
metadata:
  type: reference
---

# Weill et al. 2017 (Science) — 7PET African intro waves T1-T12 + palette

"Genomic history of the seventh pandemic of cholera in Africa," Science 358:785-789,
doi:10.1126/science.aad5901, PMID 29123067. 1070 V. cholerae El Tor genomes, 45 African countries,
1966-2014. Full memo (country->group table + palettes) at
`MOSAIC-pkg/claude/regional_clustering/source_palettes_and_groupings.md`.

## Retrieval (science.org paywalled; NO PMC full text)
- Unpaywall on the DOI gives two OA repos. **Cambridge Apollo has the published 6-page PDF**:
  `https://www.repository.cam.ac.uk/bitstreams/461b9f03-81bf-4b20-971c-7896eab81722/download`
  (resolve via `doi.org/10.17863/cam.36741`). Bari/uniba mirror also OA.

## Grouping scheme
- **12 introduction events T1-T12** of the 7PET lineage into Africa. **T2 is an EXPORT (Africa->Peru
  1990s), NOT an African intro** — exclude from any African choropleth.
- Temporal waves (Fig 3/4 legend): **Wave 1 = T1-T6** (1970s), **Wave 2 = T7-T8**, **Wave 3 = T9-T12**
  (post-2000, MDR-carrying).
- Two macro-theatres (abstract): **West Africa** vs **East/Southern Africa**. Only 2 cross-theatre
  exchanges: Angola<->Mozambique (1970s); Great Lakes -> W-DRC/CAR via Congo River (2011-12).
- Quoted anchors: T1 = 1970 Ogawa W-Africa origin (pan-African, reached Angola 1971, Beira/MOZ 1973);
  T3 = 1970 Ethiopia Inaba; T5 = 1994 Rwandan refugee camps; T7 = 1974 Portuguese-Guinea troops;
  T8 = ZAF 2001-02 + ZWE 2008-09; T11 = mostly Mozambique. Fig 3 = T1-T12 x UN-subregion bubble grid
  (the regional-assignment source); Fig 2 = decade route maps (the T-wave arrow color source).
- Country->wave is partly INFERRED from Fig 2 point positions (Weill reports dominance by UN
  subregion, not a clean 1-country-1-wave table). Theatre 2-class is solid; Central-Africa
  (TCD/CAF/COG/COD/GAB/GNQ) is the fuzzy seam.

## Palettes (sampled from figure PDFs, hue-verified)
Fig 2 T-wave arrows (bold; RECOMMENDED): T1 navy #17355B, T3 red #E42625, T4 sky-cyan #17C5E9,
T5 magenta #D42392, T6 green #0C823E, T7 orange #F0814F, T8 rose #F4415E, T9 teal #71C4C8,
T10 dark-olive #47422E, T11 gold #F4BC1B, T12 magenta-pink #DD2590. NB T5≈T12 (both magenta, never
co-occur in a panel); T9 teal != T4 sky-cyan (T9 desaturated). No T2 color.
Fig 3/4 Wave legend (softer, approx): Wave1 steel-blue ~#3B6FA8, Wave2 orange #F8AD5E,
Wave3 red/salmon ~#D9342B.

## Cross-ref to DiPrete
Both agree West vs East/Southern. DiPrete hmmCE (Great Lakes+Horn+ZMB) ~ Weill T3/T4/T5/T6/T10;
hmmS (AGO+southern belt+islands) ~ Weill T8/T11(+T1 tail). Seam: Weill puts Congo-basin
(CAF/COG/COD) East/Southern via Congo-River; DiPrete hmmW absorbs Central-West. See
`[[diprete-2025-transmission-units]]`.
