#' List of species codes
#'
#' A dataset containing a list of species integer id codes matched to 4-letter
#' codes according to the Birds Studies Canada authority.
#'
#' @format A data frame with 1210 rows and 3 variables:
#' \describe{
#'   \item{species_code}{Numeric species id code for use in filtering data}
#'   \item{species_alpha}{The 4-letter species alpha code}
#'   \item{species_name}{English common name}
#' }
"species_codes"

#' List of country codes
#'
#' A dataset containing a list of country id codes matched to names in English
#' and French
#'
#' @format A data frame with 39 rows and 3 variables:
#' \describe{
#'   \item{country_code}{Letter country code for use in filtering data}
#'   \item{country_name}{Country name in English}
#'   \item{country_name_fr}{Country name in French}
#' }
"country_codes"

#' List of state/province codes
#'
#' A dataset containing a list of state/province id codes matched to names in
#' English, French and Spanish
#'
#' @format A data frame with 94 rows and 5 variables:
#' \describe{
#'   \item{country_code}{Letter country code for use in filtering data}
#'   \item{statprov_code}{Letter state/province code for use in filtering data}
#'   \item{statprov_name}{State/Province name in English}
#'   \item{statprov_name_fr}{State/Province name in French}
#'   \item{statprov_name_es}{State/Province name in Spanish}
#' }
"statprov_codes"