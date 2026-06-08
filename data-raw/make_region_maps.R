# =============================================================================
# make_region_maps.R — Generate the static iso_code -> region cluster maps that
# est_suitability(architecture = "lstm_v2_hierarchical_film") selects via
# arch_control$region_map. Emits:
#
#   inst/extdata/region_map_snf_k5.csv      (production DEFAULT region map)
#   inst/extdata/region_map_snf_k4.csv      (toggle)
#   inst/extdata/region_map_seasonal_v1.csv (toggle)
#
# Two other maps are NOT static CSVs:
#   "csv"      -> the live `region` column of the suitability CSV (4 WHO admin
#                 regions; B4 reproduction baseline).
#   "hydro_v1" -> MOSAIC_REGION_MAP_V1, a named constant defined in
#                 R/region_maps_suitability.R.
#
# Each map is restricted to MOSAIC::iso_codes_mosaic (the MOSAIC-40) and is
# asserted EXHAUSTIVE over the 40 (no _unmapped_ region). Provenance below.
#
# Run:  Rscript data-raw/make_region_maps.R
# =============================================================================

suppressMessages({
     MOSAIC::set_root_directory("/Users/johngiles/MOSAIC")
     library(MOSAIC)
})

mosaic_40 <- sort(MOSAIC::iso_codes_mosaic)
stopifnot(length(mosaic_40) == 40L)

# ---- SNF k=5 (production DEFAULT) -------------------------------------------
# Provenance: Similarity Network Fusion (Wang et al. 2014) of four views —
# DTW on monthly log cases, Euclidean on a 15-feature static matrix, the
# Objective-Lists Country Similarity Index, and symmetrized OAG flight flows —
# fused (K=6 NN, alpha=0.5, 20 iters) then Ward, cut at K=5. Transcribed from
# MOSAIC-Mozambique/output/clustering/region_map_snf_k5.R (auto-generated
# 2026-06-07). MDG/SDN in the source 42 are dropped (not in the MOSAIC-40).
#
# CAVEAT (plan_v034_RECOMMENDED.md §6 gate 4): snf_k5 is the production-default
# region map but is UNVALIDATED as a FiLM region map — the SNF partition is
# validated only as a clustering (NMI/silhouette), never run through lstm_v2. It
# ships on the best-motivated hypothesis and MUST be validated head-to-head vs
# "csv" (the B4 reproduction baseline) in the acceptance run; if it does not beat
# "csv" on the primary metric, REVERT the production default to "csv".
snf_k5 <- c(
     AGO="snf_1", BWA="snf_1", MOZ="snf_1", MWI="snf_1", NAM="snf_1",
     SWZ="snf_1", ZAF="snf_1", ZMB="snf_1", ZWE="snf_1",
     BDI="snf_2", COD="snf_2", ERI="snf_2", ETH="snf_2", KEN="snf_2",
     RWA="snf_2", SOM="snf_2", SSD="snf_2", TZA="snf_2", UGA="snf_2",
     BEN="snf_3", CIV="snf_3", GHA="snf_3", LBR="snf_3", NGA="snf_3",
     SLE="snf_3", TGO="snf_3",
     BFA="snf_4", GIN="snf_4", GMB="snf_4", GNB="snf_4", MLI="snf_4",
     MRT="snf_4", NER="snf_4", SEN="snf_4", TCD="snf_4",
     CAF="snf_5", CMR="snf_5", COG="snf_5", GAB="snf_5", GNQ="snf_5"
)

# ---- SNF k=4 (toggle) -------------------------------------------------------
# Same SNF fusion, K=4 cut: merges West-coast + Sahel and moves COD/ERI.
# Transcribed from .../region_map_snf_k4.R.
snf_k4 <- c(
     AGO="snf_1", BWA="snf_1", MOZ="snf_1", MWI="snf_1", NAM="snf_1",
     SWZ="snf_1", ZAF="snf_1", ZMB="snf_1", ZWE="snf_1",
     BDI="snf_2", ETH="snf_2", KEN="snf_2", RWA="snf_2", SOM="snf_2",
     SSD="snf_2", TZA="snf_2", UGA="snf_2",
     BEN="snf_3", BFA="snf_3", CIV="snf_3", ERI="snf_3", GHA="snf_3",
     GIN="snf_3", GMB="snf_3", GNB="snf_3", LBR="snf_3", MLI="snf_3",
     MRT="snf_3", NER="snf_3", NGA="snf_3", SEN="snf_3", SLE="snf_3",
     TCD="snf_3", TGO="snf_3",
     CAF="snf_4", CMR="snf_4", COD="snf_4", COG="snf_4", GAB="snf_4", GNQ="snf_4"
)

# ---- Seasonal v1 (toggle) ---------------------------------------------------
# Provenance: this is the REVIEWED, COMMITTED partition transcribed verbatim
# from plan_v034_RECOMMENDED.md §6.4 (it is NOT re-derived live here — the
# memberships below are the recorded output of a precipitation-seasonality
# clustering: est_seasonal_dynamics() Fourier-fitted precip -> Ward.D2, k=4 by
# peak-month phase). The clusters group by precipitation PHASE (affine-normalized
# curves -> level stripped); the "Aug" clusters mix West-coast and Horn/Sahel
# countries that merely share an August peak (e.g. ETH/SSD land in c3 via the
# kiremt / Sahelian-band rains, not West-coast cholera ecology). Kept as a
# candidate but NOT the default because phase-pooling is blind to the LASER decay
# channel (psi-level sensitive). k=4 is the committed choice; a silhouette/gap
# re-check (plan §6 gate 4 k=4 note) is deferred to the acceptance run and would
# require re-running the underlying clustering, not this transcription.
seasonal_v1 <- c(
     # c1 — Jan / austral-summer peak
     AGO="seasonal_1", BWA="seasonal_1", MOZ="seasonal_1", MWI="seasonal_1",
     NAM="seasonal_1", SWZ="seasonal_1", TZA="seasonal_1", ZAF="seasonal_1",
     ZMB="seasonal_1", ZWE="seasonal_1",
     # c2 — Sep / equatorial bimodal
     BDI="seasonal_2", COD="seasonal_2", COG="seasonal_2", GAB="seasonal_2",
     GNQ="seasonal_2", KEN="seasonal_2", RWA="seasonal_2", SOM="seasonal_2",
     UGA="seasonal_2",
     # c3 — Aug peak (West-coast monsoon + Horn kiremt/Sahel-band)
     BEN="seasonal_3", CAF="seasonal_3", CIV="seasonal_3", CMR="seasonal_3",
     ETH="seasonal_3", GHA="seasonal_3", GIN="seasonal_3", LBR="seasonal_3",
     NGA="seasonal_3", SLE="seasonal_3", SSD="seasonal_3", TGO="seasonal_3",
     # c4 — Aug peak (Sahel)
     BFA="seasonal_4", ERI="seasonal_4", GMB="seasonal_4", GNB="seasonal_4",
     MLI="seasonal_4", MRT="seasonal_4", NER="seasonal_4", SEN="seasonal_4",
     TCD="seasonal_4"
)

# ---- write + assert exhaustive ---------------------------------------------
write_region_map <- function(map, name) {
     missing <- setdiff(mosaic_40, names(map))
     if (length(missing) > 0L)
          stop(sprintf("region map '%s' is NOT exhaustive over the MOSAIC-40; missing: %s",
                       name, paste(missing, collapse = ", ")))
     extra <- setdiff(names(map), mosaic_40)
     m <- map[mosaic_40]                         # restrict to the 40, in canonical order
     if (any(is.na(m)))
          stop(sprintf("region map '%s' produced NA after restriction to the 40", name))
     df <- data.frame(iso_code = names(m), region = unname(m),
                      stringsAsFactors = FALSE)
     out <- file.path("inst", "extdata", sprintf("region_map_%s.csv", name))
     utils::write.csv(df, out, row.names = FALSE)
     message(sprintf("Wrote %s: %d countries, %d zones%s",
                     out, nrow(df), length(unique(df$region)),
                     if (length(extra)) sprintf(" (dropped %d not in MOSAIC-40: %s)",
                                                length(extra), paste(extra, collapse = ", ")) else ""))
}

write_region_map(snf_k5,      "snf_k5")
write_region_map(snf_k4,      "snf_k4")
write_region_map(seasonal_v1, "seasonal_v1")

message("Done.")
