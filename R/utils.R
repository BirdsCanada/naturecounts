parse_results <- function(r, results = FALSE) {
  if(results) r <- r$results
  structure(r, class = "data.frame", row.names = seq(along = r[[1]]))
}

parse_request <- function(request) {
  tibble::tibble(request) %>%
    dplyr::mutate(request_id = names(request),
                  request = unname(request),
                  collections = purrr::map(request, ~list_to_df(.$collections, type = "collections")),
                  filters = purrr::map_chr(request, ~filter_to_str(.$filters)),
                  requestOrigin = purrr::map_chr(request, ~null_to_na(.$requestOrigin)),
                  requestLabel = purrr::map_chr(request, ~null_to_na(.$requestLabel))) %>%
    tidyr::unnest(collections) %>%
    dplyr::select(request_id, requestOrigin, requestLabel, collections, approved, recordCount, filters)
}


list_to_df <- function(l, type) {
  df <- data.frame()
  for(i in 1:length(l)) {
    df <- dplyr::bind_rows(df, dplyr::mutate(tibble::as_tibble(l[[i]]),
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
  o <- utils::capture.output(utils::head(x))
  if(nrow(x) > 6) o <- c(o, "...")
  paste0(o, collapse = "\n")
}




#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


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
