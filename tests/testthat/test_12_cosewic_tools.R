test_that("prep_spatial()", {
  expect_silent(s <- prep_spatial(bcch))
  expect_s3_class(s, "sf")
  expect_named(s, c("record_id", "geometry")) 
  expect_equal(nrow(s), nrow(bcch))
  expect_equal(format(sf::st_crs(s)), "NAD83 / Statistics Canada Lambert") 
})

test_that("prep_spatial() diff cols", {
  b <- dplyr::rename(bcch, sp = species_id, rec = record_id)
  expect_silent(s <- prep_spatial(b, extra = "rec"))
  expect_s3_class(s, "sf")
  expect_named(s, c("rec", "geometry")) 
  expect_equal(nrow(s), nrow(b))
  expect_equal(format(sf::st_crs(s)), "NAD83 / Statistics Canada Lambert") 
})

test_that("cosewic_eoo()", {
  df <- prep_spatial(bcch)
  expect_silent(e <- cosewic_eoo(df, p = 0.95, spatial = FALSE))
  expect_s3_class(e, "data.frame")
  expect_named(e, "eoo")
  expect_equal(e[["eoo"]], units::set_units(1243.421, "km2"), tolerance = 0.001)
  
  expect_silent(e <- cosewic_eoo(df, p = 0.95, spatial = TRUE))
  expect_s3_class(e, "sf")
  expect_equal(nrow(e), 1)
  expect_equal(as.character(sf::st_geometry_type(e)), "POLYGON")
  
  expect_silent(e <- cosewic_eoo(df, p = 1, spatial = FALSE))
  expect_equal(e[["eoo"]], units::set_units(4861.251, "km2"), tolerance = 0.001)
})

test_that("cosewic_eoo() diff cols", {
  df <- dplyr::rename(bcch, sp = species_id, rec = record_id) |>
    prep_spatial(extra = "rec")
  expect_silent(e <- cosewic_eoo(df, p = 0.95, spatial = FALSE))
  expect_s3_class(e, "data.frame")
  expect_named(e, "eoo")
  expect_equal(e[["eoo"]], units::set_units(1243.421, "km2"), tolerance = 0.001)
  
  expect_silent(e <- cosewic_eoo(df, p = 0.95, spatial = TRUE))
  expect_s3_class(e, "sf")
  expect_equal(nrow(e), 1)
  expect_equal(as.character(sf::st_geometry_type(e)), "POLYGON")
  
  expect_silent(e <- cosewic_eoo(df, p = 1, spatial = FALSE))
  expect_equal(e[["eoo"]], units::set_units(4861.251, "km2"), tolerance = 0.001)
})

test_that("cosewic_eoo() no cols", {
  df <- dplyr::select(bcch, -"species_id") |>
    dplyr::mutate(record_id = dplyr::row_number()) |>
    prep_spatial()
  expect_silent(e <- cosewic_eoo(df, p = 0.95, spatial = FALSE))
  expect_s3_class(e, "data.frame")
  expect_named(e, "eoo")
  expect_equal(e[["eoo"]], units::set_units(1243.421, "km2"), tolerance = 0.001)
  
  expect_silent(e <- cosewic_eoo(df, p = 0.95, spatial = TRUE))
  expect_s3_class(e, "sf")
  expect_equal(nrow(e), 1)
  expect_equal(as.character(sf::st_geometry_type(e)), "POLYGON")
  
  expect_silent(e <- cosewic_eoo(df, p = 1, spatial = FALSE))
  expect_equal(e[["eoo"]], units::set_units(4861.251, "km2"), tolerance = 0.001)
})


test_that("cosewic_iao()", {
  df <- prep_spatial(bcch)
  expect_silent(a <- cosewic_iao(df, 
                                 cell_size = units::set_units(2, "km"), 
                                 record = "record_id", 
                                 spatial = FALSE))
  expect_s3_class(a, "data.frame")
  expect_equal(a, 
               dplyr::tibble(min_record = 1, max_record = 36, median_record = 1, 
                             grid_size_km = units::set_units(2, "km"), 
                             n_occupied = 31, iao = units::set_units(124, "km2")))
  
  expect_silent(a <- cosewic_iao(df, 
                                 cell_size = units::set_units(2, "km"), 
                                 record = "record_id", 
                                 spatial = TRUE))
  expect_s3_class(a, "sf")
  expect_equal(sum(a$n_records), nrow(bcch))
  expect_equal(nrow(a), 450)
  expect_equal(unique(as.character(sf::st_geometry_type(a))), "POLYGON")
  expect_snapshot_value(a, style = "json2")
})

test_that("cosewic_iao() diff cols", {
  df <- dplyr::rename(bcch, sp = species_id, rec = record_id) |>
    prep_spatial(extra = "rec")
  expect_silent(a <- cosewic_iao(df, 
                                 cell_size = units::set_units(2, "km"), 
                                 record = "rec",
                                 spatial = FALSE))
  expect_s3_class(a, "data.frame")
  expect_equal(a, 
               dplyr::tibble(min_record = 1, max_record = 36, median_record = 1, 
                             grid_size_km = units::set_units(2, "km"), 
                             n_occupied = 31, iao = units::set_units(124, "km2")))
  
  expect_silent(a <- cosewic_iao(df, 
                                 cell_size = units::set_units(2, "km"), 
                                 record = "rec", 
                                 spatial = TRUE))
  expect_s3_class(a, "sf")
  expect_equal(sum(a$n_records), nrow(bcch))
  expect_equal(nrow(a), 450)
  expect_equal(unique(as.character(sf::st_geometry_type(a))), "POLYGON")
  expect_snapshot_value(a, style = "json2")
})

test_that("cosewic_iao() no cols", {
  df <- dplyr::select(bcch, -"species_id") |>
    dplyr::mutate(record_id = dplyr::row_number()) |>
    prep_spatial()
  expect_silent(a <- cosewic_iao(df, 
                                 cell_size = units::set_units(2, "km"), 
                                 record = "record_id",
                                 spatial = FALSE))
  expect_s3_class(a, "data.frame")
  expect_equal(a, 
               dplyr::tibble(min_record = 1, max_record = 36, median_record = 1, 
                             grid_size_km = units::set_units(2, "km"), 
                             n_occupied = 31, iao = units::set_units(124, "km2")))
  
  expect_silent(a <- cosewic_iao(df, 
                                 cell_size = units::set_units(2, "km"), 
                                 record = "record_id", 
                                 spatial = TRUE))
  expect_s3_class(a, "sf")
  expect_equal(sum(a$n_records), nrow(bcch))
  expect_equal(nrow(a), 450)
  expect_equal(unique(as.character(sf::st_geometry_type(a))), "POLYGON")
  expect_snapshot_value(a, style = "json2")
})


test_that("cosewic_ranges()", {
  expect_silent(r <- cosewic_ranges(bcch))
  expect_type(r, "list")
  expect_named(r, c("iao", "eoo"))
  
  expect_silent(r <- cosewic_ranges(bcch, spatial = FALSE))
  expect_s3_class(r, "data.frame")
  expect_equal(
    r, 
    dplyr::tibble(species_id = 14280L,
                  n_records_total = nrow(bcch),
                  min_record = 1, max_record = 36, median_record = 1, 
                  grid_size_km = units::set_units(2, "km"), 
                  n_occupied = 31, iao = units::set_units(124, "km2"),
                  eoo_p95 = units::set_units(1243.421, "km2")), 
    tolerance = 0.001)
              
  expect_error(cosewic_ranges(dplyr::select(bcch, -"latitude")),
               "`coord_lat` and `coord_lon` must be columns in `df_db`")
  expect_error(cosewic_ranges(dplyr::mutate(bcch, latitude = collection)),
               "`coord_lat` and `coord_lon` must be numeric")
  
  expect_message(r <- cosewic_ranges(bcch[1,], spatial = FALSE), 
                 "EOO is less than IAO")
  expect_equal(r$iao, r$eoo_p95)
})

test_that("cosewic_ranges() diff cols", {
  b <- dplyr::rename(bcch, sp = species_id, rec = record_id)
  expect_silent(r <- cosewic_ranges(b, record = "rec", species = "sp"))
  expect_type(r, "list")
  expect_named(r, c("iao", "eoo"))
  
  expect_silent(r <- cosewic_ranges(b, record = "rec", species = "sp", spatial = FALSE))
  expect_s3_class(r, "data.frame")
  expect_equal(
    r, 
    dplyr::tibble(sp = 14280L,
                  n_records_total = nrow(bcch),
                  min_record = 1, max_record = 36, median_record = 1, 
                  grid_size_km = units::set_units(2, "km"), 
                  n_occupied = 31, iao = units::set_units(124, "km2"),
                  eoo_p95 = units::set_units(1243.421, "km2")), 
    tolerance = 0.001)
  
  expect_error(cosewic_ranges(dplyr::select(b, -"latitude"), record = "rec", species = "sp"),
               "`coord_lat` and `coord_lon` must be columns in `df_db`")
  expect_error(cosewic_ranges(dplyr::mutate(b, latitude = collection), record = "rec", species = "sp"),
               "`coord_lat` and `coord_lon` must be numeric")
  
  expect_message(r <- cosewic_ranges(b[1,], spatial = FALSE, record = "rec", species = "sp"), 
                 "EOO is less than IAO")
  expect_equal(r$iao, r$eoo_p95)
})

test_that("cosewic_ranges() no cols", {
  b <- dplyr::select(bcch, -"species_id", -"record_id")
  
  expect_warning(r <- cosewic_ranges(b), "Column \"species_id\"") |>
    expect_warning("Column \"record_id\"")
  expect_silent(r <- cosewic_ranges(b, record = NULL, species = NULL))
  expect_type(r, "list")
  expect_named(r, c("iao", "eoo"))
  
  expect_silent(r <- cosewic_ranges(b, record = NULL, species = NULL, spatial = FALSE))
  expect_s3_class(r, "data.frame")
  expect_equal(
    r, 
    dplyr::tibble(n_records_total = nrow(bcch),
                  min_record = 1, max_record = 36, median_record = 1, 
                  grid_size_km = units::set_units(2, "km"), 
                  n_occupied = 31, iao = units::set_units(124, "km2"),
                  eoo_p95 = units::set_units(1243.421, "km2")), 
    tolerance = 0.001)
  
  expect_error(cosewic_ranges(dplyr::select(b, -"latitude"), record = NULL, species = NULL),
               "`coord_lat` and `coord_lon` must be columns in `df_db`")
  expect_error(cosewic_ranges(dplyr::mutate(b, latitude = collection), record = NULL, species = NULL),
               "`coord_lat` and `coord_lon` must be numeric")
  
  expect_message(r <- cosewic_ranges(b[1,], spatial = FALSE, record = NULL, species = NULL), 
                 "EOO is less than IAO")
  expect_equal(r$iao, r$eoo_p95)
})

test_that("cosewic_ranges() filter_unique", {
  
  # 95% EOO
  expect_silent(r1 <- cosewic_ranges(bcch, spatial = FALSE))
  expect_warning(
    r2 <- cosewic_ranges(bcch, spatial = FALSE, filter_unique = TRUE),
                 "This may bias non-100% EOO calculations")
  
  expect_gt(r1$max_record, r2$max_record)
  expect_equal(r1[,c("grid_size_km", "n_occupied", "iao")],
               r2[,c("grid_size_km", "n_occupied", "iao")])
  expect_false(r1$eoo_p95 == r2$eoo_p95)
  
  expect_silent(r1 <- cosewic_ranges(bcch, spatial = FALSE))
  expect_warning(r2 <- cosewic_ranges(bcch, filter_unique = TRUE, spatial = FALSE),
                 "This may bias non-100% EOO calculations")
  
  # Full EOO
  expect_silent(r1 <- cosewic_ranges(bcch, eoo_p = 1, spatial = FALSE))
  expect_warning(r2 <- cosewic_ranges(bcch, filter_unique = TRUE, eoo_p = 1, spatial = FALSE),
                 "Filtering")
  expect_gt(r1$max_record, r2$max_record)
  expect_equal(r1[,c("grid_size_km", "n_occupied", "iao")],
               r2[,c("grid_size_km", "n_occupied", "iao")])
  expect_true(r1$eoo_p100 == r2$eoo_p100)
  
})


test_that("cosewic_plot()", {
  
  expect_silent(r1 <- cosewic_ranges(bcch))
  expect_silent(g1 <- cosewic_plot(r1))
  expect_s3_class(g1, "ggplot")
  
  expect_silent(g2 <- cosewic_plot(r1, grid = grid_canada()))  
  expect_silent(g3 <- cosewic_plot(r1, points = bcch))
  expect_silent(g4 <- cosewic_plot(r1, grid = grid_canada(), 
                                   map = map_canada()))
  expect_silent(g5 <- cosewic_plot(r1, grid = grid_canada(), 
                                   map = map_canada(), 
                                   title = "Black-capped Chickadees"))  
  
  expect_silent(r2 <- cosewic_ranges(rbind(bcch, hofi)))
  expect_silent(g6 <- cosewic_plot(r2))
  expect_false(inherits(g6, "ggplot"))
  expect_length(g6, 2)
  expect_named(g6, as.character(unique(r2$iao$species_id)))
  expect_s3_class(g6[[1]], "ggplot")
  expect_s3_class(g6[[2]], "ggplot")
  
  skip_on_os(c("windows", "mac"))
  vdiffr::expect_doppelganger("p_basic", g1)
  vdiffr::expect_doppelganger("p_points", g3)
  vdiffr::expect_doppelganger("p_map", g5)
})

test_that("cosewic_plot() no cols", {
  b <- dplyr::select(bcch, -"species_id", -"record_id")
  
  expect_silent(r1 <- cosewic_ranges(b, species = NULL, record = NULL))
  expect_warning(g0 <- cosewic_plot(r1), "Column \"species_id\" not found")
  expect_silent(g1 <- cosewic_plot(r1, species = NULL))
  expect_s3_class(g1, "ggplot")
  expect_equal(g0, g1)
  
  
  expect_silent(g2 <- cosewic_plot(r1, grid = grid_canada(), species = NULL))  
  expect_silent(g3 <- cosewic_plot(r1, points = bcch, species = NULL))
  expect_silent(g4 <- cosewic_plot(r1, grid = grid_canada(), 
                                   map = map_canada(), species = NULL))
  expect_silent(g5 <- cosewic_plot(r1, grid = grid_canada(), 
                                   map = map_canada(), species = NULL,
                                   title = "Black-capped Chickadees"))  
  
  # Multiple species as one
  expect_silent(r2 <- cosewic_ranges(rbind(bcch, hofi), species = NULL, record = NULL))
  expect_silent(g6 <- cosewic_plot(r2, species = NULL))
  expect_s3_class(g6, "ggplot")
  
  skip_on_os(c("windows", "mac"))
  vdiffr::expect_doppelganger("p_no_cols_basic", g1)
  vdiffr::expect_doppelganger("p_no_cols_points", g3)
  vdiffr::expect_doppelganger("p_no_cols_map", g5)
  vdiffr::expect_doppelganger("p_no_cols_mult", g6)
})

