# Save user agent as internal object
ua <- httr::user_agent(agent = "https://github.com/BirdStudiesCanada/NatureCountsAPI")

# Get database versions
nc_dbs <- readRDS("./data-raw/nc_dbs.rds")
max_version <- names(nc_dbs) %>%
  lubridate::as_date() %>%
  max()

usethis::use_data(ua, nc_dbs, max_version, internal = TRUE, overwrite = TRUE)


# Get species codes
sp_codes <- srv_query("metadata", "species_codes",
                      query = list(authority = "BBS")) %>%
  parse_results(results = FALSE) %>%
  dplyr::select(species_code = species_id, species_alpha = species_code) %>%
  dplyr::mutate(species_name = "")
usethis::use_data(sp_codes, internal = FALSE, overwrite = TRUE)

# Get province/country codes
country_statprov_codes <- srv_query("metadata", "country") %>%
  parse_results(results = FALSE) %>%
  dplyr::full_join(srv_query("metadata", "statprov") %>%
                     parse_results(results = FALSE),
                   by = "country_code") %>%
  dplyr::select(country_code, country_name, country_name_fr,
                dplyr::contains("country"), # incase we get spanish
                statprov_code, statprov_name, statprov_name_fr,
                statprov_name_es)

usethis::use_data(country_statprov_codes, internal = FALSE, overwrite = TRUE)

# Get province/state codes

usethis::use_data(country_statprov_codes, internal = FALSE, overwrite = TRUE)
