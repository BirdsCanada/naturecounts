
#' Find country, state/province, or species codes
#'
#' Search for the correct codes to identify countries, states/provinces or
#' species. These are then used in the \code{\link{nc_data_dl}()} and
#' \code{\link{nc_count}()} functions.
#'
#' These codes can be browsed directly through the built in objects:
#' \code{\link{country_codes}}, \code{\link{statprov_codes}} and
#' \code{\link{species_codes}}
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
#' codes_search(c("BCCH", "BDOW"), type = "species")  # 14280 and 7590
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

  df <- get(paste0(type, "_codes"))

  desc <- stringi::stri_trans_general(desc, "Latin-ASCII")
  desc <- paste0("(", paste0(desc, collapse = ")|("), ")")

  codes <- df %>%
    tidyr::gather(type, name, -dplyr::contains("_code")) %>%
    dplyr::mutate(name = stringi::stri_trans_general(.data$name, "Latin-ASCII")) %>%
    dplyr::filter(
      stringr::str_detect(.data$name,
                          stringr::regex(desc, ignore_case = TRUE))) %>%
    dplyr::pull(!!paste0(type, "_code")) %>%
    unique()

  dplyr::filter(df, !!rlang::sym(paste0(type, "_code")) %in% codes)
}

codes_check <- function(desc) {
  # If nothing, return as is
  if(is.null(desc)) return(desc)

  # Get type of code
  type <- deparse(substitute(desc))
  m <- ifelse(type == "species", "numeric", "character")

  # Get code data frames
  df <- get(paste0(type, "_codes"))

  vapply(desc, codes_check_each, type = type, df = df,
         FUN.VALUE = vector(length = 1, mode = m),
         USE.NAMES = FALSE)
}

codes_check_each <- function(desc, type, df) {
  # If a two character code, conver to upper, just in case
  if(type != "species" & stringr::str_detect(desc, "^[:alpha:]{2}$")) {
    desc <- toupper(desc)
  }

  # Convert to numeric (if possible)
  desc <- as_numeric(desc)

  # If code, check that correct
  if((type == "species" & is.numeric(desc)) |
     (type != "species" & stringr::str_detect(desc, "^[:upper:]{2}$"))) {

    if(!desc %in% dplyr::pull(df, paste0(type, "_code"))) {
      stop("'", type, "' code not recognized, see the ", paste0(type, "_codes"),
           " data frame for valid codes", call. = FALSE)
    }
    return(desc)
  }

  # Otherwise try to convert
  codes_convert(desc, type)
}


codes_convert <- function(desc, type) {

  c <- codes_search(desc, type) %>%
    dplyr::select(dplyr::contains("_code"),
                  dplyr::contains("_name"),
                  dplyr::contains("_alpha")) %>%
    dplyr::distinct()

  if(nrow(c) == 0) {
    stop("Unable to match '", desc, "' to any codes in the ",
         paste0(type, "_codes"), " data frame", call. = FALSE)
  } else if(nrow(c) > 1) {
    stop("Matched '", desc, "' to ", nrow(c), " codes: \n",
         capture_df(c), call. = FALSE)
  }
  dplyr::pull(c, paste0(type, "_code"))
}

