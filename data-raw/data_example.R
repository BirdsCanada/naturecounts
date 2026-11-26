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

# The example IAO grid (iao_bcch_grid.gpkg) is now created in naturetools
# (see naturetools/data-raw/iao_bcch_grid.R)
