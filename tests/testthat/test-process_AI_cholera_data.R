test_that("process_AI_cholera_data applies keep-and-weight filter, reformats, strips CRLF", {
  tmp  <- tempfile("aitest"); dir.create(tmp)
  repo <- file.path(tmp, "repo")
  outd <- file.path(tmp, "out")
  ddir <- file.path(repo, "data", "MOZ"); dir.create(ddir, recursive = TRUE)

  ws <- seq(as.Date("2020-06-01"), by = "week", length.out = 5)  # Mondays
  df <- data.frame(
    iso_code  = "MOZ",
    year      = 2020,
    iso_week  = as.integer(format(ws, "%V")),
    week_start = as.character(ws),
    week_end   = as.character(ws + 6),
    sCh       = c(10, 5, 0, 3, 99),
    deaths    = c(1, 0, 0, 0, 9),
    source    = c("WHO", "JHU", "AI", "AI", "JHU"),  # collection-method tag (should be ignored)
    confidence_weight = c(0.9, 0.5, 0.8, 0.5, 0.5),
    disaggregation_method = c("observed", "fourier_country_k2", "documented_zero",
                              "assumed_zero", "fourier_regional_East Africa_k5"),
    stringsAsFactors = FALSE
  )
  # Write with CRLF line endings to mimic the real AI repo files.
  con <- file(file.path(ddir, "cholera_weekly_MOZ.csv"), "wb")
  write.table(df, con, row.names = FALSE, sep = ",", eol = "\r\n", qmethod = "double")
  close(con)

  PATHS <- list(AI_CHOLERA_REPO = repo, DATA_AI_WEEKLY = outd)
  res <- process_AI_cholera_data(PATHS)

  # assumed_zero dropped (row 4); the other 4 kept
  expect_equal(nrow(res), 4L)
  expect_false(any(res$disaggregation_method == "assumed_zero"))
  expect_true(all(c("observed", "documented_zero", "fourier_country_k2",
                    "fourier_regional_East Africa_k5") %in% res$disaggregation_method))

  # CRLF stripped from the last column
  expect_false(any(grepl("\r", res$disaggregation_method)))

  # schema matches WHO/JHU + the 2 AI columns; collection-method `source` NOT emitted
  expect_setequal(names(res),
    c("iso_code", "country", "year", "week", "cases", "deaths",
      "date_start", "date_stop", "month", "confidence_weight", "disaggregation_method"))
  expect_false("source" %in% names(res))

  # column mapping
  expect_true(is.numeric(res$confidence_weight))
  expect_true(all(res$confidence_weight > 0 & res$confidence_weight <= 1))
  expect_equal(res$cases[res$week == as.integer(format(ws[1], "%V"))], 10)  # sCh -> cases
  expect_true(all(res$country == "Mozambique"))
  expect_s3_class(res$date_start, "Date")

  # output file written
  expect_true(file.exists(file.path(outd, "cholera_country_weekly_processed.csv")))
})

test_that("process_AI_cholera_data errors when the repo data dir is missing", {
  PATHS <- list(AI_CHOLERA_REPO = tempfile("nope"), DATA_AI_WEEKLY = tempfile("out"))
  expect_error(process_AI_cholera_data(PATHS), "not found")
})
