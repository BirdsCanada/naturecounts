% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{nc_count}
\alias{nc_count}
\title{Download information about NatureCounts collections}
\usage{
nc_count(
  collections = NULL,
  project_ids = NULL,
  species = NULL,
  years = NULL,
  doy = NULL,
  region = NULL,
  site_type = NULL,
  show = "available",
  username = NULL,
  timeout = 120,
  verbose = TRUE
)
}
\arguments{
\item{collections}{Character vector. The collection codes from which to
download data. NULL (default) downloads data from all available collections}

\item{project_ids}{Character/Numeric vector. The \verb{project id}s from which to
download data. First the collections associated with a \code{project_id} are
determined, and then data is downloaded for each collection. If both
\code{collections} and \code{project_ids} are supplied, they are combined.}

\item{species}{Numeric vector. Numeric species ids (see details)}

\item{years}{Numeric vector. The start/end years of data to download. Can use
NA for either start or end, or a single value to return data from a single
year.}

\item{doy}{Character/Numeric vector. The start/end day-of-year to download
(1-366 or dates that can be converted to day of year). Can use NA for
either start or end}

\item{region}{List. Named list with \emph{one} of the following options:
\code{country}, \code{statprov}, \code{subnational2}, \code{iba}, \code{bcr}, \code{utm_squares}, \code{bbox}.
See details}

\item{site_type}{Character vector. The type of site to return (e.g., \code{IBA}).}

\item{show}{Character. Either "all" or "available". "all" returns counts from
all data sources. "available" only returns counts for data available for
the username provided. If no username is provided, defaults to "all".}

\item{username}{Character vector. Username for \url{http://naturecounts.ca}. If
provided, the user will be prompted for a password. If left NULL, only
public collections will be returned.}

\item{timeout}{Numeric. Number of seconds before connecting to the server
times out.}

\item{verbose}{Logical. Show messages?}
}
\value{
Data frame
}
\description{
Download the number of records available for different collections filtered
by location (if provided). If authorization is provided, the collections are
filtered to only those available to the user (unless using \code{show = "all"}).
Without authorization all collections are returned.
}
\details{
The \code{akn_level} column describes the level of data access for that collection
(see \href{https://naturecounts.ca/nc/default/nc_access_levels.jsp}{descriptions online}).
The \code{access} column describes the accessibility of a collection for a given
username (or no access if no username supplied). See the section on Access
and \code{request_id}s for more details.
}
\section{NatureCounts account}{

All public data is available with a username/password
(\href{https://www.naturecounts.ca/nc/default/register.jsp}{sign up}
for a free NatureCounts account). However, to access private/semi-public
projects/collections you must request access. See the Access and
\code{request_id}s section for more information.
}

\section{Species ids (\code{species})}{

Numeric species id codes can determined from the functions
\code{\link[=search_species]{search_species()}} or \code{\link[=search_species_code]{search_species_code()}}. See also the article on
\href{https://birdscanada.github.io/naturecounts/articles/species-codes.html}{species codes}
for more information.
}

\section{Day of Year (\code{doy})}{

The format for day of year (\code{doy}) is fairly flexible and can be a whole
number between 1 and 366 or anything recognized by
\code{\link[lubridate]{lubridate-package}}'s \code{\link[lubridate]{ymd}()}
function. However, it must have the order of year, month, day. Note that
year is ignored when converting to day of year, except that it will result
in a 1 day offset for leap years.
}

\section{Regions (\code{region})}{

Regions are defined by codes reflecting the country, state/province,
subnational (level 2), Important Bird Areas (IBA), and Bird Conservation
Regions (BCR) (see \code{\link[=search_region]{search_region()}} for codes). They can also be defined
by providing specific UTM squares to download or a bounding box area which
specifies the min/max longitude and min/max latitude (\code{bbox}). See the
article on \href{http://birdscanada.github.io/naturecounts/articles/region-codes.html}{regional filters}
for more information.
}

\section{Access and \code{request_id}s}{


Access to a data collection is either available as "full" or "by request".
Use \code{nc_count(username = "USER", show = "all")}, to see the accessibility of
collections.

"Full" access means that data can be immediately requested directly through
the \code{naturecounts} R package. "By request" means that a request must be
\href{https://naturecounts.ca/nc/default/searchquery.jsp}{submitted online} and
approved before the data can be downloaded through \code{naturecounts}.

This means that there are two types of data requests: ones made through this
\code{naturecounts} R package (API requests) and those made through the online
\href{https://naturecounts.ca/nc/default/searchquery.jsp}{Web Request Form} (Web
requests). Every request (from either method) generates a \code{request_id} which
identifies the filter set and collections requested. Details of all of
requests can be reviewed with the \code{\link[=nc_requests]{nc_requests()}} function.

To download data with "full" access, users can either specify filters, or if
they are repeating a download, can use the \code{request_id} from \code{\link[=nc_requests]{nc_requests()}}.

Otherwise, if the user doesn't have "full" access, they must supply an
approved \code{request_id} to the \code{\link[=nc_data_dl]{nc_data_dl()}} function (e.g.,
\code{nc_data_dl(request_id = 152000, username = "USER")}). Use \code{\link[=nc_requests]{nc_requests()}} to
see \code{request_id}s, filters, and approval status.

Requests for "full" access to additional collections can be made online
through the \href{https://naturecounts.ca/nc/default/searchquery.jsp}{Web Request Form} by
checking the "Full access?" box in Step 2 of the form.
}

\examples{

# Count all publicly available records:
\donttest{
nc_count()
}

# Count publicly available records for Manitoba, Canada
\donttest{
nc_count(region = list(statprov = "MB"))
}

# Count all records for all collections user "sample" has access to
\dontrun{
nc_count(username = "sample")
}

# Count records with house finches in Ontario
search_species("house finch")
nc_count(species = 20350, region = list(statprov = "ON"), username = "sample")

# Count all records available in the Christmas Bird Count and Breeding Bird
# Survey collections (regardless of user permissions)
nc_count(collections = c("CBC", "BBS"), show = "all", username = "sample")


}
\seealso{
\code{\link[=nc_requests]{nc_requests()}}
}
