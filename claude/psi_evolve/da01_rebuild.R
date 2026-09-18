# DA-01 panel rebuild under corrected ISO-8601 week labelling (%G + %V).
# Recipe follows MOSAIC-pkg/model/README_psi_provenance.md (the AI-enhanced
# production build that config_default's psi was fit on).
suppressMessages(pkgload::load_all("/Users/johngiles/MOSAIC/MOSAIC-pkg", quiet = TRUE))
set_root_directory("/Users/johngiles/MOSAIC")
PATHS <- get_paths()
t0 <- Sys.time(); step <- function(m) message("\n===== ", m, "  [", format(Sys.time()), "] =====")

step("1/4 process_open_meteo_data(force=TRUE)  -- re-label climate weeks %G+%V")
process_open_meteo_data(PATHS, force = TRUE)

step("2/4 process_AI_cholera_data")
process_AI_cholera_data(PATHS)

step("3/4 process_cholera_surveillance_data(include_ai=TRUE)  -- re-label survey weeks")
process_cholera_surveillance_data(PATHS, include_ai = TRUE)

step("4/4 compile_suitability_data  -- rebuilds the canonical LSTM panel")
compile_suitability_data(PATHS, cutoff = NULL, use_epidemic_peaks = TRUE,
                         date_start = "2000-01-01", date_stop = NULL,
                         forecast_mode = TRUE, forecast_horizon = 9,
                         include_lags = TRUE)

message("\n===== DONE in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min =====")
f <- file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_country_weekly_suitability_data.csv")
d <- utils::read.csv(f, stringsAsFactors = FALSE)
message("panel rows: ", nrow(d),
        " | duplicate (iso_code,date): ", sum(duplicated(d[, c("iso_code","date")])))
