# Save user agent as internal object
ua <- httr::user_agent(agent = "https://github.com/birdscanada/naturecounts")

# API URLs
meta_info <- dplyr::tribble(
  ~package_name,         ~api_url,                              ~primary_keys,
  "api",                 "https://birdscanada.org/api", NA,

  "auth",                "/data/authenticate",                  NA,

  "version",             "api_version",                         NA,

  "country_codes",       "metadata/country",                    "country_code",
  "statprov_codes",      "metadata/statprov",                   "statprov_code",
  "subnational2_codes",  "metadata/subnat2",                "subnational2_code",

  "bcr_codes",           "metadata/bcr",                        "bcr",
  "iba_codes",           "metadata/iba_sites",                  "iba_site",
  "utm_squares",         "data/utm_squares",                    NA,

  "species_authority",   "metadata/species_codes_authority",    "authority",
  "species_codes",       "metadata/species_codes",  list("species_code", "authority"),
  "species_taxonomy",    "metadata/species",                    "species_id",

  "bmde_versions",       "metadata/bmde_versions",              "version",
  "bmde_fields",         "metadata/bmde_fields",                "field_name",
  "projects",            "metadata/projects",                   "project_id",
  "projects_meta",       "metadata/projects_metadata",          "project_id",

  "collections",         "metadata/collections",                "collection",
  "breeding_codes",      "metadata/breeding_codes",             "breeding_code",
  "project_protocols",   "metadata/protocols",                  "protocol_id",
  "protocol_types",      "metadata/protocol_type",              "protocol_type",

  "data",                "data/get_data",                       "record_id",
  "collections_count",   "data/list_collections",               "collection",
  "permissions",         "data/list_permissions",               "collection",
  "list_requests",       "data/list_requests",                  NA,
  "release_request_id",  "data/release_request_id",             NA
  )

api <- as.list(meta_info$api_url)
names(api) <- meta_info$package_name

keys <- as.list(meta_info$primary_keys)
names(keys) <- meta_info$package_name

queries <- dplyr::tribble(
  ~package_name,  ~api_name,       ~unbox,
  "min_lat",       "minLat",        TRUE,
  "max_lat",       "maxLat",        TRUE,
  "min_long",      "minLong",       TRUE,
  "max_long",      "maxLong",       TRUE,
  "start_year",    "startYear",     TRUE,
  "end_year",      "endYear",       TRUE,
  "start_doy",     "startDay",      TRUE,
  "end_doy",       "endDay",        TRUE,
  "collection",    "collection",    TRUE,
  "collections",   "collections",   FALSE, # for collection counts
  "request_id",    "requestId",     TRUE,
  "utm_squares",   "utmSquare",     FALSE,
  "bbox_left",     "minLong",       TRUE,
  "bbox_bottom",   "minLat",        TRUE,
  "bbox_right",    "maxLong",       TRUE,
  "bbox_top",      "maxLat",        TRUE,
  "iba",           "iba",           FALSE,
  "bcr",           "bcr",           FALSE,
  "fields_set",    "bmdeVersion",   TRUE,
  "fields",        "fields",        FALSE,
  "species",       "species",       FALSE,
  "country",       "country",       FALSE,
  "statprov",      "statProv",      FALSE,
  "subnational2",  "subNat2",       FALSE,
  "site_type",     "siteType",      TRUE)

# Testing Data ------------------------------------------------------------
test_rc <- nc_data_dl(request_id = 152518, fields_set = "core",
                      username = "sample", info = "sample_data") %>%
  dplyr::filter(CommonName %in% c("Monarch",
                                  "Black Swallowtail",
                                  "Red Admiral"),
                AllSpeciesReported == "Yes") %>%
  dplyr::mutate(presence = as.numeric(ObservationCount > 0)) %>%
  format_dates()

# Field order - Non BMDE fields
field_order <- c("record_id", "collection", "project_id", "protocol_id",
                 "protocol_type", "species_id", "statprov_code", "country_code",
                 "SiteCode", "latitude", "longitude", "bcr", "subnational2_code",
                 "iba_site", "utm_square", "survey_year", "survey_month",
                 "survey_week", "survey_day", "is_sensitive", "bmde_status_level",
                 "sensitive_status_level", "partners_status_level",
                 "last_edited_dt", "last_edited_user_id", "last_indexed_dt",
                 "source_table", "breeding_rank", "is_unconfirmed")

# Save all internal datasets
usethis::use_data(ua, api, keys, queries, test_rc, field_order,
                  internal = TRUE, overwrite = TRUE)

# Get Example Data ------------------------------------------------------------

# Create example databases
bcch <- nc_data_dl(request_id = 152543, username = "sample")
usethis::use_data(bcch, internal = FALSE, overwrite = TRUE)

hofi <- nc_data_dl(species = 20350, username = "sample", info = "pkg_data")
usethis::use_data(hofi, internal = FALSE, overwrite = TRUE)
