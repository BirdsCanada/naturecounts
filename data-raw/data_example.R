# Get Example Data ------------------------------------------------------------

# Create example databases
bcch <- nc_data_dl(
  request_id = 152543,
  fields_set = "minimum",
  username = "sample"
)
usethis::use_data(bcch, internal = FALSE, overwrite = TRUE)

write.csv(
  dplyr::select(
    bcch,
    "id" = "record_id",
    "lat" = "latitude",
    "lon" = "longitude",
    n = "ObservationCount"
  ),
  file.path(system.file("extdata", package = "naturecounts"), "bcch.csv"),
  row.names = FALSE
)
hofi <- nc_data_dl(
  species = 20350,
  fields_set = "minimum",
  username = "sample",
  info = "pkg_data"
)
usethis::use_data(hofi, internal = FALSE, overwrite = TRUE)

pops <- dplyr::bind_rows(
  dplyr::mutate(bcch, population = "Population 1"),
  dplyr::mutate(hofi, population = "Population 2")
) %>%
  dplyr::select(record_id, latitude, longitude, population) %>%
  dplyr::distinct()
usethis::use_data(pops, internal = FALSE, overwrite = TRUE)

unlink(file.path("inst", "extdata", "bcch.nc"))
nc_data_dl(
  request_id = 152543,
  fields_set = "minimum",
  username = "sample",
  sql_db = file.path("inst", "extdata", "bcch")
)

unlink(file.path("inst", "extdata", "hofi.nc"))
nc_data_dl(
  species = 20350,
  fields_set = "minimum",
  username = "sample",
  info = "pkg_data",
  sql_db = file.path("inst", "extdata", "hofi")
)

# Only rerun as necessary
if (FALSE) {
  path <- file.path("inst", "extdata", "iao_bcch_grid.gpkg")
  file.remove(path)
  # Create example Grid
  ext <- sf::st_as_sf(
    bcch,
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>%
    sf::st_bbox() %>%
    sf::st_transform("ESRI:102001")

  grid <- sf::st_read("misc/data/IAO Grid/") %>%
    sf::st_crop(ext) %>%
    dplyr::select("geometry") %>%
    dplyr::mutate(grid_id = dplyr::row_number()) %>%
    sf::st_write(path)
}
