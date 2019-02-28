
#' Find country, state/province, or species codes
#'
#' Search for the correct codes to identify countries, states/provinces or
#' species. These are then used in the \code{\link{nc_data_dl}()} and
#' \code{\link{nc_count}()} functions.
#'
#' These codes can be browsed directly through the built in objects:
#' \code{\link{country_stat_prov_codes}} and \code{\link{sp_codes}}
#'
#' @param desc Character. The search term to match
#' @param type Character. One of "country", "statprov", or "species". The type
#'   of code to return.
#'
#' @return A data frame with the relevant codes and other information
#'
#' @examples
#'
#' codes_search("Mexico", type = "country")   # MX
#'
#' codes_search("Yucatan", type = "statprov") # Yucatán
#' codes_search("Alberta", type = "statprov") # AB
#'
#' codes_search("BCCH", type = "species")     # 14280
#'
#' \donttest{
#' # Using the codes
#' nc_count(statprov = "AB", species = "14280", startyear = 2010)
#' }
#'
#' @export

codes_search <- function(desc, type = "country") {
  if(!type %in% c("country", "statprov", "species")) {
    stop("'type' must be one of 'country', 'statprov', or 'species'.",
         call. = FALSE)
  }

  if(type == "country") {
    codes <- codes_filter(df = country_statprov_codes,
                          columns = c("country_name", "country_name_fr"),
                          desc = desc,
                          code_column = "country_code")
  } else if(type == "statprov") {
    codes <- codes_filter(df = country_statprov_codes,
                          columns = c("statprov_name", "statprov_name_fr",
                                      "statprov_name_es"),
                          desc = desc,
                          code_column = "statprov_code")
  } else if(type == "species") {
    codes <- codes_filter(df = sp_codes,
                          columns = "species_alpha",
                          desc = desc,
                          code_column = "species_code")
  }
  codes
}

#' Generic code filter
#'
#' This function can be reused for other code data frame to search for terms
#' among several columns and return the corresponding codes.
#'
#' Note that by transforming to "Latin-ASCII" we remove accents before pattern
#' matching, resulting in 'Yucatan' matching 'Yucatán'.
#'
#' @param df Data frame. Data frame with code information
#' @param columns Character vector. Columns in df to be searched
#' @param desc Character. The character string to search for
#' @param code_column Character. The name of the column in df that contains the
#'   code values
#'
#' @return df filtered by desc
#'
#' @keywords internal

codes_filter <- function(df, columns, desc, code_column) {
  desc <- stringi::stri_trans_general(desc, "Latin-ASCII")
  desc <- paste0("(", paste0(desc, collapse = ")|("), ")")

  codes <- df %>%
    tidyr::gather(type, name, dplyr::one_of(columns)) %>%
    dplyr::mutate(name = stringi::stri_trans_general(.data$name, "Latin-ASCII")) %>%
    dplyr::filter(
      stringr::str_detect(.data$name,
                          stringr::regex(desc, ignore_case = TRUE))) %>%
    dplyr::pull(!!code_column) %>%
    unique()

  dplyr::filter(df, !!rlang::sym(code_column) %in% codes)
}


codes_convert <- function(desc, type) {

  # If no conversion needed just return as is
  if(is.null(desc) ||
     (type == "species" & is.numeric(desc)) ||
     (type != "species" & nchar(desc) == 2)) return(desc)

  c <- codes_search(desc, type) %>%
    dplyr::select(paste0(type, "_code"), paste0(type, "_name")) %>%
    dplyr::distinct()

  if(length(desc) != nrow(c)) {
    message("Matched '", paste0(desc, collapse = ", "), "' to ",
            paste0(c[, paste0(type, "_name")], collapse = ", "))
  }
  c[, paste0(type, "_code")]
}
