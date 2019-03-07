check_collections <- function(c) {
 if(!is.null(c) && !is.character(c)) {
   stop("'collections' must be either NULL (for all collections) ",
        "or a character vector of collection names.", call. = FALSE)
 }
}

check_authority <- function(a) {
  if(!all(a %in% species_authority()$authority)) {
    stop("'authority' must be one or more of the authorities ",
         "specified in the species_authority data frame.", call. = FALSE)
  }
}