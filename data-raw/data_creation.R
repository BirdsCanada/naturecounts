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
  dplyr::select(species_id, species_code)
usethis::use_data(sp_codes, internal = FALSE, overwrite = TRUE)
