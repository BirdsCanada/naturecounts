# Check on status of data requests

List pending or completed data requests for a given user.

## Usage

``` r
nc_requests(request_id = NULL, type = "web", username)
```

## Arguments

- request_id:

  Numeric. Specific request id to check or download.

- type:

  Character One of "web", "api", or "all" specifying which types of
  request to return (defaults to "web").

- username:

  Character vector. Username for <http://naturecounts.ca>. If provided,
  the user will be prompted for a password. If left NULL, only public
  collections will be returned.

## Value

data frame

## Access and `request_id`s

Access to a data collection is either available as "full" or "by
request". Use `nc_count(username = "USER", show = "all")`, to see the
accessibility of collections.

"Full" access means that data can be immediately downloaded directly
through the `naturecounts` R package. "By request" means that a request
must be [submitted
online](https://naturecounts.ca/nc/default/searchquery.jsp) and approved
before the data can be downloaded through `naturecounts`.

This means that there are two types of data requests: ones made through
this `naturecounts` R package (API requests) and those made through the
online [Web Request
Form](https://naturecounts.ca/nc/default/searchquery.jsp) (Web
requests). Every request (from either method) generates a `request_id`
which identifies the filter set and collections requested. Details of
all of requests can be reviewed with the `nc_requests()` function.

To download data with "full" access, users can either specify filters,
or if they are repeating a download, can use the `request_id` from
`nc_requests()`.

Otherwise, if the user doesn't have "full" access, they must supply an
approved `request_id` to the
[`nc_data_dl()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_data_dl.md)
function (e.g., `nc_data_dl(request_id = 152000, username = "USER")`).
Use `nc_requests()` to see `request_id`s, filters, and approval status.

Requests for "full" access to additional collections can be made online
through the [Web Request
Form](https://naturecounts.ca/nc/default/searchquery.jsp) by checking
the "Full access?" box in Step 2 of the form.

## Examples

``` r
nc_requests(username = "sample")
#> [1] request_id     request_origin request_label  collection     status        
#> [6] nrecords       filter        
#> <0 rows> (or 0-length row.names)
nc_requests(request_id = 152446, username = "sample")
#> [1] request_id     request_origin request_label  collection     status        
#> [6] nrecords       filter        
#> <0 rows> (or 0-length row.names)
```
