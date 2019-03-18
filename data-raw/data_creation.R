# Save user agent as internal object
ua <- httr::user_agent(agent = "https://github.com/BirdStudiesCanada/NatureCountsAPI")

# API URLs
api <- list(
  "api" = "https://sandbox.birdscanada.org/api",

  "version" = "api_version",

  "country_codes" = "metadata/country",
  "statprov_codes" = "metadata/statprov",
  "subnational2_codes" = "metadata/subnat2",

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
  "permissions" = "data/list_permissions",
  "utm_squares" = "data/utm_squares"
  )

keys <- list(
  "country_codes" = "country_code",
  "statprov_codes" = "statprov_code",
  "sub_national2_codes" = "subnational2_code",
  "species_authority" = "authority",
  #"species_codes" = "species_id2"  # No unique column...
  "species_taxonomy" = "species_id")

queries <- dplyr::tribble(
  ~package_name,  ~api_name,       ~unbox,
  "min_lat",       "minLat",        TRUE,
  "max_lat",       "maxLat",        TRUE,
  "min_long",      "minLong",       TRUE,
  "max_long",      "maxLong",       TRUE,
  "start_year",    "startYear",     TRUE,
  "end_year",      "endYear",       TRUE,
  "start_doy",  "startDay",      TRUE,
  "end_doy",    "endDay",        TRUE,
  "collection",    "collection",    TRUE,
  "collections",   "collections",   FALSE, # for collection counts
  "request_id",    "requestId",     TRUE,
  "utm_square",    "utmSquare",     TRUE,
  "fields_set",    "bmdeVersion",   TRUE,
  "fields",        "fields",        FALSE,
  "species",       "species",       FALSE,
  "country",       "country",       FALSE,
  "statprov",      "statProv",      FALSE,
  "subnational2",  "subnational2",  FALSE)


# Save all internal datasets
usethis::use_data(ua, api, keys, queries, internal = TRUE, overwrite = TRUE)

# Get Example Data ------------------------------------------------------------

# Create example databases
bcch <- nc_data_dl(collections = "RCBIOTABASE", species = 14280)
usethis::use_data(bcch, internal = FALSE, overwrite = TRUE)

bdow <- nc_data_dl(collections = "RCBIOTABASE", species = 7590)
usethis::use_data(bdow, internal = FALSE, overwrite = TRUE)
