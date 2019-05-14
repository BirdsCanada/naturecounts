# args ------------------
#' Common arguments for `nc_data_dl` and `nc_count`
#'
#' @param collections Character vector. The collection codes from which to
#'   download data. NULL (default) downloads data from all available collections
#' @param project_ids Character/Numeric vector. The `project id`s from which to
#'   download data. First the collections associated with a `project_id` are
#'   determined, and then data is downloaded for each collection. If both
#'   `collections` and `project_ids` are supplied, they are combined.
#' @param species Numeric vector. Numeric species ids (see details)
#' @param years Numeric vector. The start/end years of data to download. Can use
#'   NA for either start or end, or a single value to return data from a single
#'   year.
#' @param doy Character/Numeric vector. The start/end day-of-year to download
#'   (1-366 or dates that can be converted to day of year). Can use NA for
#'   either start or end
#' @param region List. Named list with *one* of the following options:
#'   `country`, `statprov`, `subnational2`, `iba`, `bcr`, `utm_squares`, `bbox`.
#'   See details
#' @param site_type Character vector. The type of site to return (e.g., `IBA`).
#' @param username Character vector. Username for <http://naturecounts.ca>. If
#'   provided, the user will be prompted for a password. If left NULL, only
#'   public collections will be returned.
#' @param request_id Numeric. Specific request id to check or download.
#' @param verbose Logical. Show messages?
#'
#' @section NatureCounts account:
#'   All public data is available without a username/password. However, to
#'   access private/semi-public projects/collections you must [sign
#'   up](https://www.birdscanada.org/birdmon/default/register.jsp) for a free
#'   NatureCounts account and
#'   [register](https://www.birdscanada.org/birdmon/default/projects.jsp) for
#'   the projects you'd like to access. See the `request_id` section for more
#'   information.
#'
#' @section Species ids (`species`):
#'   Numeric species id codes can determined from the functions
#'   \code{\link{species_search}()} or \code{\link{species_code_search}()}. See
#'   also the [species codes]
#'   (https://birdstudiescanada.github.io/naturecounts/articles/species-codes.html)
#'   for more information.
#'
#' @section Day of Year (`doy`):
#'   The format for day of year (`doy`) is fairly flexible and can be a whole
#'   number between 1 and 366 or anything recognized by
#'   \code{\link[lubridate]{lubridate-package}}'s \code{\link[lubridate]{ymd}()}
#'   function. However, it must have the order of year, month, day. Note that
#'   year is ignored when converting to day of year, except that it will result
#'   in a 1 day offset for leap years.
#'
#' @section Regions (`region`):
#'   Regions are defined by codes reflecting the country, state/province,
#'   subnational (level 2), Important Bird Areas (IBA), and Bird Conservation
#'   Regions (BCR) (see [region_search()] for codes). They can also be defined
#'   by providing specific UTM squares to download or a bounding box area which
#'   specifyings the min/max longitude and min/max latitude (`bbox`). See the
#'   article on [regional filters]
#'   (http://BirdStudiesCanada.github.io/naturecounts/articles/region-codes.html)
#'   for more information.
#'
#' @section Data Fields/Columns (`fields_set` and `fields`):
#'   By default data is downloaded with the `minimum` set of fields/columns.
#'   However, for more advanced applications, users may wish to specify which
#'   fields/columns to return. The Bird Monitoring Data Exchange (BMDE) schema
#'   keeps track of variables used to augment observation data. There are
#'   different versions reflecting different collections of variables which can
#'   be specified for download in one of four ways:
#'
#'   1. `fields_set` can be a specific shorthand reflecting a BMDE version:
#'   `core`, `extended` or `minimum` (default). See [meta_bmde_versions()] to see
#'   which BMDE version the shorthand refers to.
#'   2. `fields_set` can be `default` which uses the default BMDE version for a
#'   particular collection (note that if you download more than one collection,
#'   the field sets will expand to cover all fields/columns in the combined
#'   collections)
#'   3. `fields_set` can be the exact BMDE version. See [meta_bmde_versions()]
#'   for options.
#'   4. `fields_set` can be `custom` and the `fields` argument can be a
#'   character vector specifying the exact fields/columns to return. See
#'   [meta_bmde_fields()]) for potential `fields` values.
#'
#'   Note that in all cases there are a set of fields/columns that are *always*
#'   returned, no matter what `fields_set` is used.
#'
#' @section `request_id`'s:
#' There are two types of data requests: ones made through the api (i.e. this
#' `naturecounts` R package) and those made through the online [Web Request
#' Form](https://www.birdscanada.org/birdmon/default/searchquery.jsp). Each
#' request generates a request id which identifies the filter set and
#' collections requested.
#'
#' Requests made through this package via the [nc_data_dl()] function only
#' return data the user has access to. If data was successfully downloaded, the
#' user can see the request details with the [nc_requests()] function.
#'
#' Requests made through the online [Web Request
#' Form](https://www.birdscanada.org/birdmon/default/searchquery.jsp) may
#' include collections to which the user does not have access. [nc_requests()]
#' will list the requests along with the approval status (either 'approved' or
#' 'pending').
#'
#' Any approved request id can be downloaded by suppling the `request_id` to
#' [nc_data_dl()] (e.g., `nc_data_dl(request_id = 152000, username = "USER")`).
#'
#'
#' @keywords internal
#' @name args
NULL
