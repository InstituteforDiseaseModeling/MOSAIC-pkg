test_that("download_climate_data saves correct parquet files for 2 countries (parallel)", {
    tmp <- withr::local_tempdir()
    PATHS <- list(
        DATA_SHAPEFILES = tmp,
        DATA_RAW        = tmp,
        DATA_CLIMATE    = file.path(tmp, "processed")
    )
    dir.create(file.path(tmp, "climate"), recursive = TRUE)

    mock_df <- data.frame(
        date          = as.Date(c("2020-01-01", "2020-01-02")),
        latitude      = 1.0,
        longitude     = 36.0,
        climate_model = "MRI_AGCM3_2_S",
        variable_name = "temperature_2m_mean",
        value         = c(25.0, 26.0)
    )

    local_mocked_bindings(
        convert_iso_to_country  = function(x) x,
        generate_country_grid_n = function(shp, n_points)
            sf::st_as_sf(data.frame(x = 36.0, y = 1.0), coords = c("x", "y"), crs = 4326),
        get_climate_future      = function(...) mock_df,
        .package = "MOSAIC"
    )
    local_mocked_bindings(
        st_read = function(...) sf::st_sf(
            geometry = sf::st_sfc(sf::st_polygon(
                list(matrix(c(33, 34, 34, 33, 33, -1, -1, 1, 1, -1), ncol = 2)))),
            crs = 4326),
        .package = "sf"
    )

    download_climate_data(
        PATHS             = PATHS,
        iso_codes         = c("KEN", "NGA"),
        n_points          = 1,
        date_start        = "2020-01-01",
        date_stop         = "2020-01-02",
        climate_models    = "MRI_AGCM3_2_S",
        climate_variables = "temperature_2m_mean",
        api_key           = "test_key"
    )

    files <- list.files(file.path(tmp, "climate"), pattern = "\\.parquet$")
    expect_length(files, 2)
    expect_true(any(grepl("KEN", files)))
    expect_true(any(grepl("NGA", files)))
    expect_true(any(grepl("temperature_2m_mean", files)))
})

test_that("download_climate_data saves parquets with region_id for shapes mode", {
    tmp <- withr::local_tempdir()
    PATHS <- list(DATA_SHAPEFILES = tmp, DATA_RAW = tmp, DATA_CLIMATE = file.path(tmp, "processed"))
    dir.create(file.path(tmp, "climate"), recursive = TRUE)

    mock_df <- data.frame(
        date          = as.Date("2020-01-01"),
        latitude      = 1.0,
        longitude     = 36.0,
        climate_model = "MRI_AGCM3_2_S",
        variable_name = "temperature_2m_mean",
        value         = 25.0
    )
    poly <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(33, 34, 34, 33, 33, -1, -1, 1, 1, -1), ncol = 2)))),
        crs = 4326)

    local_mocked_bindings(
        generate_country_grid_n = function(shp, n_points)
            sf::st_as_sf(data.frame(x = 36.0, y = 1.0), coords = c("x", "y"), crs = 4326),
        get_climate_future      = function(...) mock_df,
        .package = "MOSAIC"
    )

    download_climate_data(
        PATHS             = PATHS,
        shapes            = list(myregion = poly),
        n_points          = 1,
        date_start        = "2020-01-01",
        date_stop         = "2020-01-01",
        climate_models    = "MRI_AGCM3_2_S",
        climate_variables = "temperature_2m_mean",
        api_key           = "test_key"
    )

    files <- list.files(file.path(tmp, "climate"), pattern = "\\.parquet$")
    expect_length(files, 1)
    expect_true(grepl("myregion", files[1]))
    df <- arrow::read_parquet(file.path(tmp, "climate", files[1]))
    expect_true("region_id" %in% names(df))
    expect_false("iso_code" %in% names(df))
    expect_equal(df$region_id[1], "myregion")
})

test_that("download_climate_data saves grouped parquets with region_id for points mode", {
    tmp <- withr::local_tempdir()
    PATHS <- list(DATA_SHAPEFILES = tmp, DATA_RAW = tmp, DATA_CLIMATE = file.path(tmp, "processed"))
    dir.create(file.path(tmp, "climate"), recursive = TRUE)

    mock_df <- data.frame(
        date          = as.Date("2020-01-01"),
        latitude      = 1.0,
        longitude     = 36.0,
        climate_model = "MRI_AGCM3_2_S",
        variable_name = "temperature_2m_mean",
        value         = 25.0
    )

    local_mocked_bindings(
        get_climate_future = function(...) mock_df,
        .package = "MOSAIC"
    )

    pts <- data.frame(lat = c(1.0, 1.5, 2.0, 2.5), lon = c(36.0, 36.5, 37.0, 37.5),
                      id = c("A", "A", "B", "B"), stringsAsFactors = FALSE)
    download_climate_data(
        PATHS             = PATHS,
        points            = pts,
        date_start        = "2020-01-01",
        date_stop         = "2020-01-01",
        climate_models    = "MRI_AGCM3_2_S",
        climate_variables = "temperature_2m_mean",
        api_key           = "test_key"
    )

    files <- list.files(file.path(tmp, "climate"), pattern = "\\.parquet$")
    expect_length(files, 2)
    expect_true(any(grepl("_A\\.parquet$", files)))
    expect_true(any(grepl("_B\\.parquet$", files)))
    df <- arrow::read_parquet(file.path(tmp, "climate", files[grepl("_A\\.parquet$", files)]))
    expect_true("region_id" %in% names(df))
    expect_false("iso_code" %in% names(df))
})

test_that("download_climate_data saves single parquet with id=custom for ungrouped points", {
    tmp <- withr::local_tempdir()
    PATHS <- list(DATA_SHAPEFILES = tmp, DATA_RAW = tmp, DATA_CLIMATE = file.path(tmp, "processed"))
    dir.create(file.path(tmp, "climate"), recursive = TRUE)

    mock_df <- data.frame(
        date          = as.Date("2020-01-01"),
        latitude      = 1.0,
        longitude     = 36.0,
        climate_model = "MRI_AGCM3_2_S",
        variable_name = "temperature_2m_mean",
        value         = 25.0
    )

    local_mocked_bindings(
        get_climate_future = function(...) mock_df,
        .package = "MOSAIC"
    )

    pts <- data.frame(lat = c(1.0, 1.5), lon = c(36.0, 36.5))
    download_climate_data(
        PATHS             = PATHS,
        points            = pts,
        date_start        = "2020-01-01",
        date_stop         = "2020-01-01",
        climate_models    = "MRI_AGCM3_2_S",
        climate_variables = "temperature_2m_mean",
        api_key           = "test_key"
    )

    files <- list.files(file.path(tmp, "climate"), pattern = "\\.parquet$")
    expect_length(files, 1)
    expect_true(grepl("custom", files[1]))
})

test_that("download_climate_data errors when multiple input modes supplied", {
    PATHS <- list(DATA_SHAPEFILES = ".", DATA_RAW = ".", DATA_CLIMATE = ".")
    pts <- data.frame(lat = 1.0, lon = 36.0)
    expect_error(
        download_climate_data(PATHS, iso_codes = "KEN", points = pts,
                              date_start = "2020-01-01", date_stop = "2020-01-01",
                              climate_models = "MRI_AGCM3_2_S",
                              climate_variables = "temperature_2m_mean",
                              api_key = "test_key"),
        "Exactly one"
    )
})

test_that("download_climate_data warns when free API quota would be exceeded", {
    tmp <- withr::local_tempdir()
    PATHS <- list(DATA_SHAPEFILES = tmp, DATA_RAW = tmp, DATA_CLIMATE = tmp)

    expect_warning(
        tryCatch(
            download_climate_data(
                PATHS             = PATHS,
                iso_codes         = rep("KEN", 54),
                n_points          = 5,
                date_start        = "1950-01-01",
                date_stop         = "2050-01-01",
                climate_models    = c("CMCC_CM2_VHR4", "FGOALS_f3_H", "HiRAM_SIT_HR",
                                      "MRI_AGCM3_2_S", "EC_Earth3P_HR", "MPI_ESM1_2_XR",
                                      "NICAM16_8S"),
                climate_variables = c("temperature_2m_mean", "precipitation_sum"),
                api_key           = "free"
            ),
            error = function(e) NULL
        ),
        regexp = "free API limit"
    )
})
