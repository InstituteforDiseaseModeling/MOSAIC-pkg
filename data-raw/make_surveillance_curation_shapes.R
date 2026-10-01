# Builds inst/extdata/surveillance_curation_shapes.csv: the cumulative-count
# anchors that shape curated WHO windows (surveillance_curation.csv, shape =
# "cumulative"). Each anchor is the cumulative count through the end of `date`;
# process_WHO_weekly_data() spreads the report in proportion to the curve's
# increments over each WHO week (Sunday to Saturday).
#
# ZAF-2023-AAR. Source: WHO, Multi-country outbreak of cholera, External
# situation report #5 (4 August 2023), Figure 5 (right): South Africa suspected
# and confirmed cholera cases per SYMPTOM ONSET date as of 15 July 2023 (data
# WHO, NICD, Gauteng Department of Health). The bar chart is digitized from the
# figure's embedded raster: bar heights against the y gridlines (10 cases each),
# days against the weekly x tick marks. Checks: 1,271 cases (the sitrep text
# gives 1,274 as of 9 July) and every bar height within 0.25 case of a whole
# count. Requires poppler's `pdfimages` and the jpeg package (data-raw only, not
# package dependencies).

sitrep5_url <- paste0("https://www.who.int/docs/default-source/coronaviruse/situation-reports/",
                      "20230803_multi-country_outbreak-of-cholera_sitrep-5.pdf?sfvrsn=7bcfafb5_4&download=true")
stopifnot(nzchar(Sys.which("pdfimages")), requireNamespace("jpeg", quietly = TRUE))
tmp <- tempfile("sitrep5_"); dir.create(tmp)
pdf <- file.path(tmp, "sitrep5.pdf")
utils::download.file(sitrep5_url, pdf, mode = "wb", quiet = TRUE)
system2("pdfimages", c("-j", "-f", "8", "-l", "8", shQuote(pdf), shQuote(file.path(tmp, "fig"))))
img <- jpeg::readJPEG(file.path(tmp, "fig-000.jpg"))
stopifnot(identical(dim(img), c(828L, 1729L, 3L)))

R <- img[, , 1]; G <- img[, , 2]; B <- img[, , 3]
grey <- (R + G + B) / 3
bar  <- (R > 0.75 & G > 0.3 & G < 0.75 & B < 0.4) |     # suspected (orange)
        (R < 0.45 & B > 0.5 & G > 0.25 & G < 0.7)       # confirmed (blue)

# y axis: gridlines at 70, 60, ..., 10 cases and the x-axis line at 0, found in a
# bar-free column band (March 2023) as rows darker than the background
dark_rows <- which(rowMeans(grey[, 1120:1180]) < 0.985)
dark_rows <- dark_rows[dark_rows >= 60 & dark_rows <= 421]
grid_y <- tapply(dark_rows, cumsum(c(1, diff(dark_rows) > 1)), mean)
stopifnot(length(grid_y) == 8L)
fy <- stats::lm(seq(70, 0, by = -10) ~ grid_y)
stopifnot(max(abs(stats::resid(fy))) < 0.2)

# x axis: 22 weekly tick marks (05 Feb ... 02 Jul 2023) just below the axis line
tick_cols <- which(grey[423, 960:1700] < 0.85) + 959L
tick_x <- tapply(tick_cols, cumsum(c(1, diff(tick_cols) > 1)), mean)
stopifnot(length(tick_x) == 22L)
fx <- stats::lm(tick_x ~ as.numeric(seq(as.Date("2023-02-05"), by = 7, length.out = 22L)))
stopifnot(max(abs(stats::resid(fx))) < 0.5)

# Bars are centred on their day; the height is read from the median top row of the
# three central columns (the top coloured row lies half a row below the bar edge)
days <- seq(as.Date("2023-02-01"), as.Date("2023-07-05"), by = 1)
height <- vapply(days, function(day) {
     xc <- stats::coef(fx)[1] + stats::coef(fx)[2] * as.numeric(day)
     cols <- round(xc) + (-1:1)
     cols <- cols[cols > 968 & cols <= 1700]
     tops <- vapply(cols, function(x) {
          y <- which(bar[60:418, x]) + 59L
          if (length(y)) min(y) else NA_real_
     }, numeric(1))
     if (all(is.na(tops))) return(0)
     unname(stats::coef(fy)[1] + stats::coef(fy)[2] * (stats::median(tops, na.rm = TRUE) - 0.5))
}, numeric(1))
cases <- round(height)
stopifnot(all(abs(height - cases)[cases > 0] < 0.25), sum(cases) == 1271)

# WHO weeks run Sunday to Saturday (MMWR), so each anchor is a Saturday
week_end <- days + (6L - as.POSIXlt(days)$wday)
weekly <- tapply(cases, week_end, sum)
ends <- seq(min(as.Date(names(weekly))) - 7L, max(as.Date(names(weekly))), by = 7)
weekly_all <- stats::setNames(rep(0, length(ends)), as.character(ends))
weekly_all[names(weekly)] <- weekly
lab <- function(saturday) {
     sunday <- saturday - 6L
     n <- as.integer(weekly_all[as.character(saturday)])
     sprintf("WHO %d-W%02d (%s - %s): %d %s", lubridate::epiyear(sunday), lubridate::epiweek(sunday),
             format(sunday, "%d %b"), format(saturday, "%d %b"), n, if (n == 1L) "onset" else "onsets")
}
zaf <- data.frame(id = "ZAF-2023-AAR", date = format(ends), cumulative_cases = cumsum(weekly_all),
                  note = c("start: no onset before Sunday 29 January 2023",
                           vapply(ends[-1], lab, character(1))),
                  stringsAsFactors = FALSE)

out <- file.path("inst", "extdata", "surveillance_curation_shapes.csv")
utils::write.csv(zaf, out, row.names = FALSE)
message("Wrote ", nrow(zaf), " anchors to ", out, " (", sum(cases), " cases)")
