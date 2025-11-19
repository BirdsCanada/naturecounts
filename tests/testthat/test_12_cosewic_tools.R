test_that("prep_spatial()", {
  expect_silent(s <- prep_spatial(bcch, p = 1, crs = "ESRI:102001"))
  expect_s3_class(s, "sf")
  expect_named(s, c("record_id", "geometry", "prop_include"))
  expect_equal(nrow(s), nrow(bcch))
  expect_equal(format(sf::st_crs(s)), "Canada_Albers_Equal_Area_Conic")
})

test_that("prep_spatial() diff cols", {
  b <- dplyr::rename(bcch, sp = species_id, rec = record_id)
  expect_silent(s <- prep_spatial(b, p = 1, extra = "rec", crs = 3347))
  expect_s3_class(s, "sf")
  expect_named(s, c("rec", "geometry", "prop_include"))
  expect_equal(nrow(s), nrow(b))
  expect_equal(format(sf::st_crs(s)), "NAD83 / Statistics Canada Lambert")
})

test_that("prep_spatial() projected", {
  b <- dplyr::rename(bcch, sp = species_id, rec = record_id)
  expect_error(
    prep_spatial(b, p = 1, crs = 4326),
    "CRS is unprojected, area calculations should use a projected CRS"
  )
})

test_that("filter_spatial() filters", {
  b <- prep_spatial(bcch, p = 1, crs = "ESRI:102001")

  expect_silent(b1 <- filter_spatial(b, p = 1))
  expect_equal(b, b1)
  expect_equal(b1$prop_include[1], 1)

  expect_silent(b2 <- filter_spatial(b, p = 0.95))
  expect_gt(nrow(b), nrow(b2))
  expect_equal(b2$prop_include[1], 0.95)
})

test_that("cosewic_eoo()", {
  # Lambert
  df <- prep_spatial(bcch, p = 0.95, crs = 3347)
  expect_silent(e <- cosewic_eoo(df, spatial = FALSE, clip = NULL))
  expect_s3_class(e, "data.frame")
  expect_named(e, c("eoo", "prop_include"))
  expect_equal(e[["eoo"]], units::set_units(1243.421, "km2"), tolerance = 0.001)

  expect_silent(e <- cosewic_eoo(df, spatial = TRUE, clip = NULL))
  expect_s3_class(e, "sf")
  expect_equal(nrow(e), 1)
  expect_equal(as.character(sf::st_geometry_type(e)), "POLYGON")

  df <- prep_spatial(bcch, p = 1, crs = 3347)
  expect_silent(e <- cosewic_eoo(df, spatial = FALSE, clip = NULL))
  expect_equal(e[["eoo"]], units::set_units(4861.251, "km2"), tolerance = 0.001)

  # Albers
  df <- prep_spatial(bcch, p = 0.95, crs = "ESRI:102001")
  expect_silent(e <- cosewic_eoo(df, spatial = FALSE, clip = NULL))
  expect_s3_class(e, "data.frame")
  expect_named(e, c("eoo", "prop_include"))
  expect_equal(e[["eoo"]], units::set_units(1209.179, "km2"), tolerance = 0.001)

  expect_silent(e <- cosewic_eoo(df, spatial = TRUE, clip = NULL))
  expect_s3_class(e, "sf")
  expect_equal(nrow(e), 1)
  expect_equal(as.character(sf::st_geometry_type(e)), "POLYGON")

  df <- prep_spatial(bcch, p = 1, crs = "ESRI:102001")
  expect_silent(e <- cosewic_eoo(df, spatial = FALSE, clip = NULL))
  expect_equal(e[["eoo"]], units::set_units(4728.589, "km2"), tolerance = 0.001)
})

test_that("cosewic_eoo() diff cols", {
  # Lambert
  df <- dplyr::rename(bcch, sp = species_id, rec = record_id) %>%
    prep_spatial(p = 0.95, extra = "rec", crs = 3347)
  expect_silent(e <- cosewic_eoo(df, spatial = FALSE, clip = NULL))
  expect_s3_class(e, "data.frame")
  expect_named(e, c("eoo", "prop_include"))
  expect_equal(e[["eoo"]], units::set_units(1243.421, "km2"), tolerance = 0.001)

  expect_silent(e <- cosewic_eoo(df, spatial = TRUE, clip = NULL))
  expect_s3_class(e, "sf")
  expect_equal(nrow(e), 1)
  expect_equal(as.character(sf::st_geometry_type(e)), "POLYGON")

  df <- dplyr::rename(bcch, sp = species_id, rec = record_id) %>%
    prep_spatial(p = 1, extra = "rec", crs = 3347)
  expect_silent(e <- cosewic_eoo(df, spatial = FALSE, clip = NULL))
  expect_equal(e[["eoo"]], units::set_units(4861.251, "km2"), tolerance = 0.001)
})

test_that("cosewic_eoo() no cols", {
  # Lambert
  df <- dplyr::select(bcch, -"species_id") %>%
    dplyr::mutate(record_id = dplyr::row_number()) %>%
    prep_spatial(p = 0.95, crs = 3347)
  expect_silent(e <- cosewic_eoo(df, spatial = FALSE, clip = NULL))
  expect_s3_class(e, "data.frame")
  expect_named(e, c("eoo", "prop_include"))
  expect_equal(e[["eoo"]], units::set_units(1243.421, "km2"), tolerance = 0.001)

  expect_silent(e <- cosewic_eoo(df, spatial = TRUE, clip = NULL))
  expect_s3_class(e, "sf")
  expect_equal(nrow(e), 1)
  expect_equal(as.character(sf::st_geometry_type(e)), "POLYGON")

  df <- dplyr::select(bcch, -"species_id") %>%
    dplyr::mutate(record_id = dplyr::row_number()) %>%
    prep_spatial(p = 1, crs = 3347)
  expect_silent(e <- cosewic_eoo(df, spatial = FALSE, clip = NULL))
  expect_equal(e[["eoo"]], units::set_units(4861.251, "km2"), tolerance = 0.001)
})


test_that("cosewic_iao()", {
  # Lambert
  df <- prep_spatial(bcch, p = 1, crs = 3347)
  expect_silent(
    a <- cosewic_iao(
      df,
      cell_size = units::set_units(2, "km"),
      record = "record_id",
      spatial = FALSE,
      crs = 3347
    )
  )
  expect_s3_class(a, "data.frame")
  expect_equal(
    a,
    dplyr::tibble(
      min_record = 1,
      max_record = 36,
      median_record = 1,
      grid_size_km = units::set_units(2, "km"),
      n_occupied = 31,
      iao = units::set_units(124, "km2"),
      prop_include = 1
    )
  )

  expect_silent(
    a <- cosewic_iao(
      df,
      cell_size = units::set_units(2, "km"),
      record = "record_id",
      spatial = TRUE,
      crs = 3347
    )
  )
  expect_s3_class(a, "sf")
  expect_equal(sum(a$n_records), nrow(bcch))
  expect_equal(nrow(a), 450)
  expect_equal(unique(as.character(sf::st_geometry_type(a))), "POLYGON")
  expect_snapshot_value(a, style = "json2")

  # Albers
  df <- prep_spatial(bcch, p = 1, crs = "ESRI:102001")
  expect_silent(
    a <- cosewic_iao(
      df,
      cell_size = units::set_units(2, "km"),
      record = "record_id",
      spatial = FALSE,
      crs = "ESRI:102001"
    )
  )
  expect_s3_class(a, "data.frame")
  expect_equal(
    a,
    dplyr::tibble(
      min_record = 1,
      max_record = 35,
      median_record = 1,
      grid_size_km = units::set_units(2, "km"),
      n_occupied = 33,
      iao = units::set_units(132, "km2"),
      prop_include = 1
    )
  )

  expect_silent(
    a <- cosewic_iao(
      df,
      cell_size = units::set_units(2, "km"),
      record = "record_id",
      spatial = TRUE,
      crs = "ESRI:102001"
    )
  )
  expect_s3_class(a, "sf")
  expect_equal(sum(a$n_records), nrow(bcch))
  expect_equal(nrow(a), 475)
  expect_equal(unique(as.character(sf::st_geometry_type(a))), "POLYGON")
  expect_snapshot_value(a, style = "json2")
})

test_that("cosewic_iao() diff cols", {
  # Lambert
  df <- dplyr::rename(bcch, sp = species_id, rec = record_id) %>%
    prep_spatial(p = 1, extra = "rec", crs = 3347)
  expect_silent(
    a <- cosewic_iao(
      df,
      cell_size = units::set_units(2, "km"),
      record = "rec",
      spatial = FALSE,
      crs = 3347
    )
  )
  expect_s3_class(a, "data.frame")
  expect_equal(
    a,
    dplyr::tibble(
      min_record = 1,
      max_record = 36,
      median_record = 1,
      grid_size_km = units::set_units(2, "km"),
      n_occupied = 31,
      iao = units::set_units(124, "km2"),
      prop_include = 1
    )
  )

  expect_silent(
    a <- cosewic_iao(
      df,
      cell_size = units::set_units(2, "km"),
      record = "rec",
      spatial = TRUE,
      crs = 3347
    )
  )
  expect_s3_class(a, "sf")
  expect_equal(sum(a$n_records), nrow(bcch))
  expect_equal(nrow(a), 450)
  expect_equal(unique(as.character(sf::st_geometry_type(a))), "POLYGON")
  expect_snapshot_value(a, style = "json2")
})

test_that("cosewic_iao() no cols", {
  df <- dplyr::select(bcch, -"species_id") %>%
    dplyr::mutate(record_id = dplyr::row_number()) %>%
    prep_spatial(p = 1, crs = 3347)
  expect_silent(
    a <- cosewic_iao(
      df,
      cell_size = units::set_units(2, "km"),
      record = "record_id",
      spatial = FALSE,
      crs = 3347
    )
  )
  expect_s3_class(a, "data.frame")
  expect_equal(
    a,
    dplyr::tibble(
      min_record = 1,
      max_record = 36,
      median_record = 1,
      grid_size_km = units::set_units(2, "km"),
      n_occupied = 31,
      iao = units::set_units(124, "km2"),
      prop_include = 1
    )
  )

  expect_silent(
    a <- cosewic_iao(
      df,
      cell_size = units::set_units(2, "km"),
      record = "record_id",
      spatial = TRUE,
      crs = 3347
    )
  )
  expect_s3_class(a, "sf")
  expect_equal(sum(a$n_records), nrow(bcch))
  expect_equal(nrow(a), 450)
  expect_equal(unique(as.character(sf::st_geometry_type(a))), "POLYGON")
  expect_snapshot_value(a, style = "json2")
})

test_that("cosewic_iao() custom IAO grid", {
  grid <- sf::st_read(
    system.file(
      "extdata",
      "iao_bcch_grid.gpkg",
      package = "naturecounts"
    ),
    quiet = TRUE
  )
  df <- prep_spatial(bcch, p = 1, crs = "ESRI:102001")
  expect_message(
    a <- cosewic_iao(
      df,
      record = "record_id",
      spatial = FALSE,
      crs = "ESRI:102001",
      grid = grid
    ),
    "User\\-provided grid has cell size of 2 \\[km\\]"
  )
})

test_that("cosewic_ranges()", {
  # Expect message about change in defaults
  expect_message(cosewic_ranges(bcch), "now uses `prop_include = 1`")

  # Lambert
  expect_silent(r <- cosewic_ranges(bcch, crs = 3347))
  expect_type(r, "list")
  expect_named(r, c("iao", "eoo"))

  expect_silent(
    r <- cosewic_ranges(
      bcch,
      prop_include = 0.95,
      spatial = FALSE,
      crs = 3347
    )
  )
  expect_s3_class(r, "data.frame")
  expect_equal(
    r,
    dplyr::tibble(
      species_id = 14280L,
      n_records_total = 152,
      min_record = 1,
      max_record = 36,
      median_record = 1,
      grid_size_km = units::set_units(2, "km"),
      n_occupied = 24,
      iao = units::set_units(96, "km2"),
      eoo = units::set_units(1243.421, "km2"),
      prop_include = 0.95
    ),
    tolerance = 0.001
  )

  expect_error(
    cosewic_ranges(dplyr::select(bcch, -"latitude")),
    "`coord_lat` and `coord_lon` must be columns in `df_db`"
  )
  expect_error(
    cosewic_ranges(dplyr::mutate(bcch, latitude = collection)),
    "`coord_lat` and `coord_lon` must be numeric"
  )

  expect_message(
    r <- cosewic_ranges(bcch[1, ], spatial = FALSE),
    "EOO is less than IAO"
  )
  expect_equal(r$iao, r$eoo)

  # Albers
  expect_silent(r <- cosewic_ranges(bcch, crs = "ESRI:102001"))
  expect_type(r, "list")
  expect_named(r, c("iao", "eoo"))

  expect_silent(
    r <- cosewic_ranges(
      bcch,
      prop_include = 0.95,
      spatial = FALSE,
      crs = "ESRI:102001"
    )
  )
  expect_s3_class(r, "data.frame")
  expect_equal(
    r,
    dplyr::tibble(
      species_id = 14280L,
      n_records_total = 152,
      min_record = 1,
      max_record = 35,
      median_record = 1,
      grid_size_km = units::set_units(2, "km"),
      n_occupied = 26,
      iao = units::set_units(104, "km2"),
      eoo = units::set_units(1209.179, "km2"),
      prop_include = 0.95
    ),
    tolerance = 0.001
  )

  expect_error(
    cosewic_ranges(dplyr::select(bcch, -"latitude")),
    "`coord_lat` and `coord_lon` must be columns in `df_db`"
  )
  expect_error(
    cosewic_ranges(dplyr::mutate(bcch, latitude = collection)),
    "`coord_lat` and `coord_lon` must be numeric"
  )

  expect_message(
    r <- cosewic_ranges(bcch[1, ], spatial = FALSE),
    "EOO is less than IAO"
  )
  expect_equal(r$iao, r$eoo)
})

test_that("cosewic_ranges() diff cols", {
  # Lambert
  b <- dplyr::rename(bcch, sp = species_id, rec = record_id)
  expect_silent(
    r <- cosewic_ranges(b, record = "rec", group = "sp", crs = 3347)
  )
  expect_type(r, "list")
  expect_named(r, c("iao", "eoo"))

  expect_silent(
    r <- cosewic_ranges(
      b,
      prop_include = 0.95,
      record = "rec",
      group = "sp",
      spatial = FALSE,
      crs = 3347
    )
  )
  expect_s3_class(r, "data.frame")
  expect_equal(
    r,
    dplyr::tibble(
      sp = 14280L,
      n_records_total = 152,
      min_record = 1,
      max_record = 36,
      median_record = 1,
      grid_size_km = units::set_units(2, "km"),
      n_occupied = 24,
      iao = units::set_units(96, "km2"),
      eoo = units::set_units(1243.421, "km2"),
      prop_include = 0.95
    ),
    tolerance = 0.001
  )

  expect_error(
    cosewic_ranges(
      dplyr::select(b, -"latitude"),
      record = "rec",
      group = "sp"
    ),
    "`coord_lat` and `coord_lon` must be columns in `df_db`"
  )
  expect_error(
    cosewic_ranges(
      dplyr::mutate(b, latitude = collection),
      record = "rec",
      group = "sp"
    ),
    "`coord_lat` and `coord_lon` must be numeric"
  )

  expect_message(
    r <- cosewic_ranges(
      b[1, ],
      spatial = FALSE,
      record = "rec",
      group = "sp"
    ),
    "EOO is less than IAO"
  )
  expect_equal(r$iao, r$eoo)
})

test_that("cosewic_ranges() no cols", {
  b <- dplyr::select(bcch, -"species_id", -"record_id")

  expect_warning(
    r <- cosewic_ranges(b, crs = 3347),
    "Column \"species_id\""
  ) %>%
    expect_warning("Column \"record_id\"")
  expect_silent(
    r <- cosewic_ranges(b, record = NULL, group = NULL, crs = 3347)
  )
  expect_type(r, "list")
  expect_named(r, c("iao", "eoo"))

  expect_silent(
    r <- cosewic_ranges(
      b,
      prop_include = 0.95,
      record = NULL,
      group = NULL,
      spatial = FALSE,
      crs = 3347
    )
  )
  expect_s3_class(r, "data.frame")
  expect_equal(
    r,
    dplyr::tibble(
      n_records_total = 152,
      min_record = 1,
      max_record = 36,
      median_record = 1,
      grid_size_km = units::set_units(2, "km"),
      n_occupied = 24,
      iao = units::set_units(96, "km2"),
      eoo = units::set_units(1243.421, "km2"),
      prop_include = 0.95
    ),
    tolerance = 0.001
  )

  expect_error(
    cosewic_ranges(
      dplyr::select(b, -"latitude"),
      record = NULL,
      group = NULL
    ),
    "`coord_lat` and `coord_lon` must be columns in `df_db`"
  )
  expect_error(
    cosewic_ranges(
      dplyr::mutate(b, latitude = collection),
      record = NULL,
      group = NULL
    ),
    "`coord_lat` and `coord_lon` must be numeric"
  )

  expect_message(
    r <- cosewic_ranges(b[1, ], spatial = FALSE, record = NULL, group = NULL),
    "EOO is less than IAO"
  )
  expect_equal(r$iao, r$eoo)
})

test_that("cosewic_ranges() filter_unique", {
  # 95% EOO
  expect_silent(
    r1 <- cosewic_ranges(
      bcch,
      prop_include = 0.95,
      spatial = FALSE,
      crs = 3347
    )
  )
  expect_warning(
    r2 <- cosewic_ranges(
      bcch,
      prop_include = 0.95,
      spatial = FALSE,
      filter_unique = TRUE,
      crs = 3347
    ),
    "This may bias which observations are filtered"
  )

  expect_gt(r1$max_record, r2$max_record)
  expect_equal(
    r1[, c("grid_size_km", "prop_include")],
    r2[, c("grid_size_km", "prop_include")]
  )
  expect_false(r1$eoo == r2$eoo)
  expect_false(r1$iao == r2$iao)

  expect_silent(
    r1 <- cosewic_ranges(
      bcch,
      prop_include = 0.95,
      spatial = FALSE,
      crs = 3347
    )
  )
  expect_warning(
    r2 <- cosewic_ranges(
      bcch,
      prop_include = 0.95,
      filter_unique = TRUE,
      spatial = FALSE,
      crs = 3347
    ),
    "This may bias which observations"
  )

  # Full EOO
  expect_silent(
    r1 <- cosewic_ranges(bcch, spatial = FALSE, crs = 3347)
  )
  expect_warning(
    r2 <- cosewic_ranges(
      bcch,
      filter_unique = TRUE,
      spatial = FALSE,
      crs = 3347
    ),
    "Filtering"
  )
  expect_gt(r1$max_record, r2$max_record)
  expect_equal(
    r1[, c("grid_size_km", "prop_include")],
    r2[, c("grid_size_km", "prop_include")]
  )
  expect_true(r1$eoo == r2$eoo)
  expect_true(r1$iao == r2$iao)
})


test_that("cosewic_ranges() eoo clip", {
  ON <- rnaturalearth::ne_states("Canada") %>%
    dplyr::filter(postal == "ON")
  mult <- rbind(bcch, hofi)

  expect_silent(r0 <- cosewic_ranges(mult, crs = 3347))
  expect_silent(r1 <- cosewic_ranges(mult, eoo_clip = ON, crs = 3347))
  expect_true(all(r0$eoo$eoo > r1$eoo$eoo))
})

test_that("cosewic_ranges() either", {
  expect_silent(s0 <- cosewic_ranges(bcch))
  expect_silent(s1 <- cosewic_ranges(bcch, which = "eoo"))
  expect_silent(s2 <- cosewic_ranges(bcch, which = "iao"))
  expect_equal(s0$eoo, s1$eoo)
  expect_equal(s0$iao, s2$iao)
})

test_that("cosewic_ranges() errors/warnings if using unprojected CRS", {
  expect_error(
    cosewic_ranges(bcch, crs = 4326),
    "CRS is unprojected, area calculations should use a projected CRS"
  )
})


test_that("cosewic_ranges() custom IAO grid", {
  grid <- sf::st_read(
    system.file(
      "extdata",
      "iao_bcch_grid.gpkg",
      package = "naturecounts"
    ),
    quiet = TRUE
  )
  expect_message(
    a <- cosewic_ranges(bcch, prop_include = 0.95, iao_grid = grid),
    "User\\-provided grid has cell size of 2 \\[km\\]"
  )
  expect_type(a, "list")
  expect_named(a, c("iao", "eoo"))
  expect_s3_class(a$iao, "sf")

  # Error when grid is wrong CRS
  expect_error(
    cosewic_ranges(bcch, prop_include = 0.95, crs = 3347, iao_grid = grid),
    "`crs` must match the CRS of `iao\\_grid`"
  )
})


test_that("cosewic_plot()", {
  expect_silent(r1 <- cosewic_ranges(bcch, prop_include = 0.95, crs = 3347))
  expect_silent(g1 <- cosewic_plot(r1))
  expect_s3_class(g1, "ggplot")

  expect_silent(g2 <- cosewic_plot(r1, grid = grid_canada(crs = 3347)))
  expect_silent(g3 <- cosewic_plot(r1, points = bcch))
  expect_silent(
    g4 <- cosewic_plot(r1, grid = grid_canada(crs = 3347), map = map_canada())
  )
  expect_silent(
    g5 <- cosewic_plot(
      r1,
      crs = 3347,
      grid = grid_canada(crs = 3347),
      map = map_canada(),
      title = "Black-capped Chickadees"
    )
  )

  expect_silent(
    r2 <- cosewic_ranges(rbind(bcch, hofi), prop_include = 0.95, crs = 3347)
  )
  expect_silent(g6 <- cosewic_plot(r2))
  expect_false(inherits(g6, "ggplot"))
  expect_length(g6, 2)
  expect_named(g6, as.character(unique(r2$iao$species_id)))
  expect_s3_class(g6[[1]], "ggplot")
  expect_s3_class(g6[[2]], "ggplot")

  expect_silent(g7 <- cosewic_plot(r1, iao_prop = TRUE))

  skip_on_os(c("windows", "mac"))
  skip_on_ci()
  vdiffr::expect_doppelganger("p_basic", g1)
  vdiffr::expect_doppelganger("p_points", g3)
  vdiffr::expect_doppelganger("p_map", g5)
  vdiffr::expect_doppelganger("p_prop", g7)
})

test_that("cosewic_plot() no cols", {
  b <- dplyr::select(bcch, -"species_id", -"record_id")

  expect_silent(
    r1 <- cosewic_ranges(
      b,
      prop_include = 0.95,
      group = NULL,
      record = NULL,
      crs = 3347
    )
  )
  expect_warning(g0 <- cosewic_plot(r1), "Column \"species_id\" not found")
  expect_silent(g1 <- cosewic_plot(r1, group = NULL))
  expect_s3_class(g1, "ggplot")
  expect_equal(g0, g1)

  expect_silent(
    g2 <- cosewic_plot(r1, grid = grid_canada(crs = 3347), group = NULL)
  )
  expect_silent(g3 <- cosewic_plot(r1, points = bcch, group = NULL))
  expect_silent(
    g4 <- cosewic_plot(
      r1,
      grid = grid_canada(crs = 3347),
      map = map_canada(),
      group = NULL
    )
  )
  expect_silent(
    g5 <- cosewic_plot(
      r1,
      grid = grid_canada(crs = 3347),
      map = map_canada(),
      group = NULL,
      title = "Black-capped Chickadees"
    )
  )

  # Multiple groups as one
  expect_silent(
    r2 <- cosewic_ranges(
      rbind(bcch, hofi),
      prop_include = 0.95,
      group = NULL,
      record = NULL,
      crs = 3347
    )
  )
  expect_silent(g6 <- cosewic_plot(r2, group = NULL))
  expect_s3_class(g6, "ggplot")

  skip_on_os(c("windows", "mac"))
  skip_on_ci()
  vdiffr::expect_doppelganger("p_no_cols_basic", g1)
  vdiffr::expect_doppelganger("p_no_cols_points", g3)
  vdiffr::expect_doppelganger("p_no_cols_map", g5)
  vdiffr::expect_doppelganger("p_no_cols_mult", g6)
})
