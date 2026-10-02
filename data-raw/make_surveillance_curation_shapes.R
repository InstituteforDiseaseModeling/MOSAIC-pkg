# Builds inst/extdata/surveillance_curation_shapes.csv: the cumulative-count
# anchors that shape curated WHO windows (surveillance_curation.csv, shape =
# "cumulative"). Each anchor is a cumulative count through the end of `date`;
# process_WHO_weekly_data() spreads the report in proportion to each curve's
# increments over the WHO weeks, which run Monday to Sunday. A row carries a case
# count, a death count or both; an id without death anchors has its deaths follow
# the case curve.
#
# ZAF-2023-AAR, cases. Source: WHO, Multi-country outbreak of cholera, External
# situation report #5 (4 August 2023), Figure 5 (right): South Africa suspected
# and confirmed cholera cases per SYMPTOM ONSET date as of 15 July 2023 (data
# WHO, NICD, Gauteng Department of Health). The bar chart is digitized from the
# figure's embedded raster: bar heights against the y gridlines (10 cases each),
# days against the weekly x tick marks. Checks: 1,271 cases (the Department of
# Health's 1,073 suspected and 198 confirmed of 4 July; the sitrep gives 1,274 as
# of 9 July) and every bar height within 0.25 case of a whole count.
# WHO's other weekly rows are dated by report week, and the model reports cases a
# single, global delay after symptom onset, so the onset dates are moved 2 days
# later: the onset-to-notification lag at which the onset curve's cumulative
# shape best matches the notification-date curve of situation report #4
# (6 July 2023, Figure 2, as of 15 June; sum of squared differences of the
# normalized cumulative curves over 20 April - 10 June: 0.143 at 0 days, 0.040 at
# 1, 0.013 at 2, 0.060 at 3). The imported case of the Department of Health's
# statement of 25 July 2023 (symptoms 14 July after travel from Karachi,
# admitted 18 July, confirmed 24 July; the 199th confirmed case) is added by the
# same rule, on 16 July.
#
# ZAF-2023-AAR, deaths: the cumulative death counts the authorities reported, by
# report date (no shift); see `zaf_deaths` below for each source. The Gauteng
# Department of Health counted Hammanskraal deaths in May; the national count is
# that plus the Benoni death of February and, from 25 May, the Free State death at
# Parys hospital (the national toll was 24 on 27 May); the first Mpumalanga death
# (30 May) falls after the 28 May anchor. WHO's 44 deaths as of 9 July (sitrep #5)
# trails the Department of Health's 47 as of 4 July and is not used; WHO AFRO's
# after-action review also gives 47 by 31 July.
#
# Requires poppler's `pdfimages` and the jpeg package (data-raw only, not
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

# Report dates: onset + 2 days; the imported Karachi case (onset 14 July) likewise
onset_to_report <- 2L
report <- data.frame(date = c(days, as.Date("2023-07-14")) + onset_to_report,
                     cases = c(cases, 1))

# WHO weeks run Monday to Sunday: each anchor is the Sunday that ends a week
week_end <- report$date + (7L - as.POSIXlt(report$date)$wday) %% 7L
weekly <- tapply(report$cases, week_end, sum)
ends <- seq(min(as.Date(names(weekly))) - 7L, max(as.Date(names(weekly))), by = 7)
weekly_all <- stats::setNames(rep(0, length(ends)), as.character(ends))
weekly_all[names(weekly)] <- weekly
stopifnot(sum(weekly_all) == 1272)
week_label <- function(sunday) {
     monday <- sunday - 6L
     sprintf("WHO %d-W%02d (%s - %s)", lubridate::epiyear(monday - 1L), lubridate::epiweek(monday - 1L),
             format(monday, "%d %b"), format(sunday, "%d %b"))
}
case_note <- function(sunday) {
     n <- as.integer(weekly_all[as.character(sunday)])
     sprintf("%s: %d %s (onset + 2 days)%s", week_label(sunday), n, if (n == 1L) "case" else "cases",
             if (sunday == as.Date("2023-07-16")) ", the imported case of 14 July" else "")
}
zaf_cases <- data.frame(date = ends, cumulative_cases = cumsum(weekly_all),
                        note = c("start: no case before Monday 30 January 2023", vapply(ends[-1], case_note, "")))

# Report-dated cumulative deaths (national)
zaf_deaths <- data.frame(
     date = as.Date(c("2023-01-29", "2023-02-22", "2023-02-23", "2023-05-15", "2023-05-21",
                      "2023-05-22", "2023-05-24", "2023-05-27", "2023-05-28", "2023-06-06",
                      "2023-06-15", "2023-06-25", "2023-07-04")),
     cumulative_deaths = c(0, 0, 1, 1, 11, 16, 21, 24, 25, 31, 38, 43, 47),
     source = c("start",
                "no death before the first",
                "first death (Benoni, no travel history), announced by the Minister of Health on 23 February (News24, SAnews)",
                "1 death as of 15 May (WHO situation report #3)",
                "Gauteng DoH: 10 Hammanskraal deaths as of 21 May (Inside Metros, 22 May), + Benoni",
                "Gauteng DoH: 15 Hammanskraal deaths on 22 May (Daily Maverick), + Benoni",
                "Gauteng DoH: 20 Hammanskraal deaths as of 24 May (SAnews, 25 May), + Benoni",
                "national: 24 deaths, the official toll of 27 May (NDoH, The Citizen 27 May)",
                "Gauteng DoH: 23 Hammanskraal deaths, announced 28 May (SAnews, 29 May), + Benoni + the Free State death of 25 May at Parys hospital (News24 25 May, OFM 9 June)",
                "national: 31 deaths, 1 February - 6 June (NDoH, SAnews 8 June)",
                "national: 38 deaths as of 15 June (WHO situation report #4)",
                "national: 43 deaths at the 25 June report (NDoH 5 July: 47, with 4 recorded since 25 June)",
                "national: 47 deaths as of 4 July (NDoH media statement of 5 July)"))
stopifnot(!is.unsorted(zaf_deaths$cumulative_deaths), max(zaf_deaths$cumulative_deaths) == 47)

zaf <- merge(zaf_cases, zaf_deaths, by = "date", all = TRUE)
zaf$note <- ifelse(is.na(zaf$note), paste0("deaths: ", zaf$source),
                   ifelse(is.na(zaf$source) | zaf$source == "start", zaf$note,
                          paste0(zaf$note, "; deaths: ", zaf$source)))
zaf <- data.frame(id = "ZAF-2023-AAR", date = format(zaf$date), cumulative_cases = zaf$cumulative_cases,
                  cumulative_deaths = zaf$cumulative_deaths, note = zaf$note, stringsAsFactors = FALSE)

out <- file.path("inst", "extdata", "surveillance_curation_shapes.csv")
utils::write.csv(zaf, out, row.names = FALSE, na = "")
message("Wrote ", nrow(zaf), " anchors to ", out, ": ", max(zaf$cumulative_cases, na.rm = TRUE), " cases, ",
        max(zaf$cumulative_deaths, na.rm = TRUE), " deaths")
