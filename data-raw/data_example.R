# Get Example Data ------------------------------------------------------------

# Create example databases
bcch <- nc_data_dl(request_id = 152543, username = "sample")
usethis::use_data(bcch, internal = FALSE, overwrite = TRUE)

hofi <- nc_data_dl(species = 20350, username = "sample", info = "pkg_data")
usethis::use_data(hofi, internal = FALSE, overwrite = TRUE)

unlink(file.path("inst", "extdata", "bcch.nc"))
nc_data_dl(request_id = 152543, username = "sample", 
           sql_db = file.path("inst", "extdata", "bcch"))

unlink(file.path("inst", "extdata", "hofi.nc"))
nc_data_dl(species = 20350, username = "sample", info = "pkg_data",
           sql_db = file.path("inst", "extdata", "hofi"))
