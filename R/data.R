#' List of numeric species codes for birds
#'
#' A dataset containing a list of numeric species id codes matched to
#' alphanumeric species codes according to various authorities.
#'
#' @format A data frame with 15709 rows and 34 variables
"species_codes"

#' List of species authorities for bird species codes
#'
#' A dataset containing a list of authorities for various alphanumeric species
#' codes for birds
#'
#' @format A data frame with 31 rows and 4 variables:
#' \describe{
#'   \item{authority}{Authority}
#'   \item{authors}{Authors of the authority}
#'   \item{year}{Year of the authority}
#'   \item{version}{Version of the authority}
#' }
"species_authority"

#' List of species taxonomy
#'
#' A dataset containing a list of species ids associated with taxonomy.
#'
#' @format A data frame with 35844 rows and 17 variables
"species_taxonomy"

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

#' List of subnational codes
#'
#' A dataset containing a list of subnational id codes matched to names in
#' English.
#'
#' @format A data frame with 3391 rows and 4 variables:
#' \describe{
#'   \item{country_code}{Letter country code for use in filtering data}
#'   \item{statprov_code}{Letter state/province code for use in filtering data}
#'   \item{subnat_code}{Letter subnational code for use in filtering data}
#'   \item{subnat_name}{Subnational area name in English}
#' }
"subnat_codes"

