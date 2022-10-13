parse_results <- function(r, results = FALSE) {
  if(results) r <- r$results
  dplyr::as_tibble(r)
}

parse_request <- function(request) {
  dplyr::tibble(request) %>%
    dplyr::mutate(
      request_id = names(.data$request),
      request = unname(.data$request),
      collection = purrr::map(.data$request,
                              ~list_to_df(.$collection, type = "collection")),
      filters = purrr::map_chr(.data$request, ~filter_to_str(.$filters)),
      requestOrigin = purrr::map_chr(.data$request, ~null_to_na(.$requestOrigin)),
      requestLabel = purrr::map_chr(.data$request, ~null_to_na(.$requestLabel))) %>%
    tidyr::unnest(cols = "collection") %>%
    dplyr::select("request_id", "requestOrigin", "requestLabel",
                  "collection",
                  "status" = "approved",
                  "nrecords",
                  "filters") %>%
    dplyr::mutate(nrecords = as_numeric(.data$nrecords)) %>%
    dplyr::arrange(.data$request_id) %>%
    as.data.frame()
}


list_to_df <- function(l, type) {
  df <- data.frame()
  for(i in 1:length(l)) {
    df <- dplyr::bind_rows(df, dplyr::mutate(dplyr::as_tibble(l[[i]]),
                                             !!type := names(l)[i]))
  }
  df
}

null_to_na <- function(x) dplyr::if_else(is.null(x), as.character(NA), x)

progress_query <- function(current, max, by) {
  to <- max - current
  to <- ifelse(to > by, current + by, max)
  message("    Records ", current + 1, " to ", to, " / ", max)
}


#' Convert to numeric if possible
#'
#' If possible, converts to numeric, otherwise returns unchanged.
#'
#' @param x Vector of values to be converted
#'
#' @keywords internal

as_numeric <- function(x) {
  x1 <- suppressWarnings(as.numeric(as.character(x)))
  if(any(!is.na(x[is.na(x1)]))) x1[is.na(x1)] <- x[is.na(x1)]
  x1
}

capture_df <- function(x) {
  o <- utils::capture.output(utils::head(as.data.frame(x)))
  if(nrow(x) > 6) o <- c(o, "...")
  paste0(o, collapse = "\n")
}



# Pipe operator -------------------------------
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>% %T>%
#' @usage lhs \%>\% rhs
NULL

# Tidy eval helpers -------------------------
#' Tidy eval helpers
#'
#' @description
#'
#' * \code{\link[rlang]{sym}()} creates a symbol from a string and
#'   \code{\link[rlang]{syms}()} creates a list of symbols from a
#'   character vector.
#'
#' * \code{\link[rlang]{expr}()} and \code{\link[rlang]{quo}()} quote
#'   one expression. `quo()` wraps the quoted expression in a quosure.
#'
#'   The plural variants [rlang::exprs()] and
#'   \code{\link[rlang]{quos}()} return a list of quoted expressions or
#'   quosures.
#'
#' * \code{\link[rlang]{enexpr}()} and \code{\link[rlang]{enquo}()}
#'   capture the expression supplied as argument by the user of the
#'   current function (`enquo()` wraps this expression in a quosure).
#'
#'   \code{\link[rlang]{enexprs}()} and \code{\link[rlang]{enquos}()}
#'   capture multiple expressions supplied as arguments, including
#'   `...`.
#'
#' `exprs()` is not exported to avoid conflicts with `Biobase::exprs()`,
#' therefore one should always use `rlang::exprs()`.
#'
#' To learn more about tidy eval and how to use these tools, visit
#' <http://rlang.r-lib.org> and the [Metaprogramming
#' section](https://adv-r.hadley.nz/meta.html) of [Advanced
#' R](https://adv-r.hadley.nz).
#'
#' @importFrom rlang .data :=
#' @md
#' @name     tidyeval
#' @keywords internal
#' @export   .data
NULL


nc_deprecate <- function(new){
  .Deprecated(msg = paste0(as.character(sys.call(sys.parent()))[1L],
                           " is deprecated, use ", new, " instead"))
}

have_auth <- function(){
  Sys.getenv("naturecounts_steffilazerte2") != ""
}



#' Remove in-memory cache
#' 
#' All server queries are cached for four hours to reduce server load. You can 
#' reset the cache at any time by either restarting your R session or running
#' `nc_remove_cache()`.
#'
#' @return `TRUE` if it worked
#' @export
#'
#' @examples
#' 
#' nc_remove_cache()
#' 
nc_remove_cache <- function() {
  memoise::forget(srv_query)
}