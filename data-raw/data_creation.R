# Save user agent as internal object
ua <- httr::user_agent(agent = "https://github.com/BirdStudiesCanada/NatureCountsAPI")

# API URLs
api <- list(
  "api" = "https://sandbox.birdscanada.org/api",

  "country_codes" = "metadata/country",
  "statprov_codes" = "metadata/statprov",
  "subnat_codes" = "metadata/subnat2",
  "species_authority" = "metadata/species_codes_authority",
  "species_codes" = "metadata/species_codes",
  "species_taxonomy" = "metadata/species",

  "bmde_versions" = "metadata/bmde_versions",
  "bmde_fields" = "metadata/bmde_fields",
  "projects" = "metadata/projects",
  "projects_meta" = "metadata/projects_metadata",

  "collections" = "metadata/collections",

  "data" = "data/get_data",
  "collections_count" = "data/list_collections",
  "permissions" = "data/list_permissions"
  )


# Save all internal datasets
usethis::use_data(ua, api, internal = TRUE, overwrite = TRUE)

# Get Example Data ------------------------------------------------------------

# Create example databases
bcch <- nc_data_dl(collections = "RCBIOTABASE", species = 14280)
usethis::use_data(bcch, internal = FALSE, overwrite = TRUE)

bdow <- nc_data_dl(collections = "RCBIOTABASE", species = 7590)
usethis::use_data(bdow, internal = FALSE, overwrite = TRUE)
