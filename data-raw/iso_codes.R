# All Africa (54 countries)
iso_codes_africa <- c(
     "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
     "COM", "COG", "COD", "CIV", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH",
     "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG",
     "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA",
     "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TGO", "TUN",
     "UGA", "TZA", "ZMB", "ZWE"
)

# North Africa (6 countries)
iso_codes_africa_north <- c("DZA", "EGY", "LBY", "MAR", "SDN", "TUN")

# West Africa (16 countries)
iso_codes_africa_west <- c("BEN", "BFA", "CPV", "CIV", "GMB", "GHA", "GIN",
                           "GNB", "LBR", "MLI", "MRT", "NER", "NGA", "SEN",
                           "SLE", "TGO")

# East Africa (14 countries)
iso_codes_africa_east <- c("BDI", "COM", "DJI", "ERI", "ETH", "KEN", "MDG",
                           "MWI", "RWA", "SYC", "SOM", "SSD", "TZA", "UGA")

# Central Africa (8 countries)
iso_codes_africa_central <- c("AGO", "CAF", "TCD", "COG", "COD", "GNQ", "GAB", "STP")

# Southern Africa (7 countries)
iso_codes_africa_south <- c("BWA", "LSO", "NAM", "ZAF", "SWZ", "ZMB", "ZWE")

# WHO AFRO Region (47 countries)
iso_codes_who_afro <- c(
     "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
     "COM", "COG", "COD", "CIV", "DJI", "GNQ", "ERI", "SWZ", "ETH", "GAB",
     "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "MDG", "MWI", "MLI",
     "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC",
     "SLE", "SOM", "ZAF", "SSD", "SDN", "TGO", "UGA", "TZA", "ZMB", "ZWE"
)

# Sub-Saharan Africa (48 countries)
iso_codes_ssa <- c(
     "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD",
     "COM", "COG", "COD", "CIV", "DJI", "GNQ", "ERI", "SWZ", "ETH",
     "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "MDG",
     "MWI", "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA",
     "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TGO",
     "UGA", "TZA", "ZMB", "ZWE"
)

# MOSAIC modeling framework countries (40 countries)
iso_codes_mosaic <- c(
     "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CAF", "TCD",
     "COG", "COD", "CIV", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB",
     "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "MWI", "MLI", "MRT",
     "MOZ", "NAM", "NER", "NGA", "RWA", "SEN", "SLE",
     "SOM", "ZAF", "SSD", "TGO", "UGA", "TZA", "ZMB", "ZWE"
)


# Use the usethis::use_data() function to save each ISO code vector

usethis::use_data(iso_codes_africa, overwrite = TRUE)
usethis::use_data(iso_codes_africa_north, overwrite = TRUE)
usethis::use_data(iso_codes_africa_west, overwrite = TRUE)
usethis::use_data(iso_codes_africa_east, overwrite = TRUE)
usethis::use_data(iso_codes_africa_central, overwrite = TRUE)
usethis::use_data(iso_codes_africa_south, overwrite = TRUE)
usethis::use_data(iso_codes_who_afro, overwrite = TRUE)
usethis::use_data(iso_codes_ssa, overwrite = TRUE)
usethis::use_data(iso_codes_mosaic, overwrite = TRUE)
