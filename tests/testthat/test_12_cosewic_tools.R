test_that("prep_spatial()", {
  expect_silent(s <- prep_spatial(bcch))
  expect_s3_class(s, "sf")
  expect_named(s, c("record_id", "geometry")) 
  expect_equal(nrow(s), nrow(bcch))
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


test_that("cosewic_iao()", {
  df <- prep_spatial(bcch)
  expect_silent(a <- cosewic_iao(df, 
                                 cell_size = units::set_units(2, "km"), 
                                 record_id = "record_id", 
                                 spatial = FALSE))
  expect_s3_class(a, "data.frame")
  expect_equal(a, 
               dplyr::tibble(min_record = 1, max_record = 36, median_record = 1, 
                             grid_size_km = units::set_units(2, "km"), 
                             n_occupied = 31, iao = units::set_units(124, "km2")))
  
  expect_silent(a <- cosewic_iao(df, 
                                 cell_size = units::set_units(2, "km"), 
                                 record_id = "record_id", 
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
})
