# Save user agent as internal object
ua <- httr::user_agent(agent = "https://github.com/BirdStudiesCanada/NatureCountsAPI")

# Create empty database table
d <- nc_data_dl(collections = "RCBIOTABASE", species = "BCCH", start_date = 2015)
readRDS("data-raw/nc_dbs.rds")
nc_dbs <- list("2018-02-22" = d[0,])
saveRDS(nc_dbs, "data-raw/nc_dbs.rds")

# Get database versions
nc_dbs <- readRDS("./data-raw/nc_dbs.rds")
max_version <- names(nc_dbs) %>%
  lubridate::as_date() %>%
  max()

usethis::use_data(ua, nc_dbs, max_version, internal = TRUE, overwrite = TRUE)


# Get species codes
species_codes <- srv_query("metadata", "species_codes",
                           query = list(authority = "BBS")) %>%
  parse_results(results = FALSE) %>%
  dplyr::select(species_code = species_id, species_alpha = species_code) %>%
  dplyr::mutate(species_name = "")
usethis::use_data(species_codes, internal = FALSE, overwrite = TRUE)

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
