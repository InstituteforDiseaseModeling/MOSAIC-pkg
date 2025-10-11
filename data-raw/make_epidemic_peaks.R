library(MOSAIC)

# Set up paths - critical for finding input files
# Set root to parent directory containing all MOSAIC repos
MOSAIC::set_root_directory("~/MOSAIC")
PATHS <- MOSAIC::get_paths()

epidemic_peaks <- est_epidemic_peaks(PATHS)
head(epidemic_peaks)

plot_epidemic_peaks(PATHS)

usethis::use_data(epidemic_peaks, overwrite = TRUE)
