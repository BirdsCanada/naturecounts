# Save user agent as internal object
ua <- httr::user_agent(agent = "https://github.com/BirdStudiesCanada/NatureCountsAPI")

# Save all internal datasets
usethis::use_data(ua, internal = TRUE, overwrite = TRUE, )

# Get Example Data ------------------------------------------------------------

# Create example databases
bcch <- nc_data_dl(collections = "RCBIOTABASE", species = 14280)
usethis::use_data(bcch, internal = FALSE, overwrite = TRUE)

bdow <- nc_data_dl(collections = "RCBIOTABASE", species = 7590)
usethis::use_data(bdow, internal = FALSE, overwrite = TRUE)
