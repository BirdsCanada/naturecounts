# Save user agent as internal object
ua <- httr::user_agent(agent = "https://github.com/BirdStudiesCanada/NatureCountsAPI")

# Create empty database table
d <- nc_data_dl(collections = "RCBIOTABASE", species = 14280, start_date = 2015)
nc_dbs <- list("2018-02-22" = d[0,])

# Get database versions
nc_dbs <- readRDS("./data-raw/nc_dbs.rds")
max_version <- names(nc_dbs) %>%
  lubridate::as_date() %>%
  max()

# Save all internal datasets
usethis::use_data(ua, nc_dbs, max_version, internal = TRUE, overwrite = TRUE)

# Get Metadata ------------------------------------------------------------

# Get species authorities
species_authority <- srv_query("metadata", "species_codes_authority") %>%
  parse_results(results = FALSE) %>%
  dplyr::select(authority, authors = TaxonomicAuthorityAuthors,
                year = TaxonomicAuthorityYear,
                version = TaxonomicAuthorityVersion, dplyr::everything())
usethis::use_data(species_authority, internal = FALSE, overwrite = TRUE)

# Get species codes
species_codes <- lapply(species_authority$authority,
                        FUN = function(x) srv_query("metadata", "species_codes",
                                                    query = list(authority = x))) %>%
  lapply(parse_results, results = FALSE) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(species_id2 = dplyr::if_else(is.na(species_id2),
                                             species_id,
                                             species_id2)) %>%
  tidyr::spread(authority, species_code) %>%
  as.data.frame()
usethis::use_data(species_codes, internal = FALSE, overwrite = TRUE)

species_taxonomy <- srv_query("metadata", "species") %>%
  parse_results(results = FALSE)
usethis::use_data(species_taxonomy, internal = FALSE, overwrite = TRUE)

# Get country codes
country_codes <- srv_query("metadata", "country") %>%
  parse_results(results = FALSE) %>%
  dplyr::arrange(country_code)
usethis::use_data(country_codes, internal = FALSE, overwrite = TRUE)

# Get province/state codes
statprov_codes <- srv_query("metadata", "statprov") %>%
  parse_results(results = FALSE) %>%
  dplyr::select(country_code, statprov_code, dplyr::everything()) %>%
  dplyr::arrange(country_code, statprov_code)
usethis::use_data(statprov_codes, internal = FALSE, overwrite = TRUE)

# Get subnational codes
subnat_codes <- srv_query("metadata", "subnat2") %>%
  parse_results(results = FALSE) %>%
  dplyr::select(country_code, statprov_code, subnat_code = subnat2_code,
                subnat_name = subnat2_name) %>%
  dplyr::arrange(country_code, statprov_code, subnat_code)
usethis::use_data(subnat_codes, internal = FALSE, overwrite = TRUE)

# Create example database
bcch <- nc_data_dl(collections = "RCBIOTABASE", species = 14280)
usethis::use_data(bcch, internal = FALSE, overwrite = TRUE)

