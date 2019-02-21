# Save user agent as internal object
ua <- httr::user_agent(agent = "https://github.com/BirdStudiesCanada/NatureCountsAPI")
usethis::use_data(ua, internal = TRUE)
