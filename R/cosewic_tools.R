#' Calculate COSEWIC IAO and EOO
#'
#' The COSEWIC Index of Area of Occupancy (IAO; also called Area of Occupancy,
#' AOO by the IUCN) and Extent of Occurrence (EOO; IUCN as well) are metrics
#' used to support status assessments for potentially endangered species.
#'
#' Note that the while the IUCN calls this metric AOO, in COSEWIC, AOO is
#' actually a different measure, the *biological* area of occupancy. See the
#' "Distribution" section in '[Instructions for preparing COSEWIC status
#' reports](https://cosewic.ca/index.php/en/instructions-preparing-status-reports.html)'
#' for more details.
#'
#' By default ranges are calculated using all points (`prop_include = 1`)
#' However, if you're working on rough data or want to do a rough first pass,
#' you may wish to use `prop_include = 0.95` to include only 95% of points
#' (based on distance to the centroid). This will ensure outlier observations
#' will not artificially inflate the EOO. Although the IAO is less sensitive to
#' outliers, to maintain consistency in the data the same observations are used
#' in both range calculations.
#'
#' For a final COSEWIC assessment report, however, it is likely better to
#' carefully explore the data to ensure there are no outliers and then use the
#' full data set (i.e. use the default of `prop_include = 1`).
#'
#' The IAO is calculated by first assessing large grids (10x large than the
#' specified size). Only then are smaller grids created within large grid cells
#' containing observations. This speeds up the process by avoiding the creation
#' of grids in areas where there are no observations. This means that the plots
#' and spatial objects may not have grids over large areas lacking observations.
#' See examples.
#'
#' Details on how IAO and EOO are calculated and used
#'
#' - COSEWIC - [Guidelines for use of the Index of Area of Occupancy in COSEWIC
#'   Assessments](https://cosewic.ca/index.php/en/reports/preparing-status-reports/guidelines-index-area-occupancy.html)
#' - COSEWIC - [Instructions for preparing COSEWIC status
#' reports](https://cosewic.ca/index.php/en/instructions-preparing-status-reports.html)
#' - COSEWIC - [Table 2 COSEWIC quantitative criteria and guidelines for the
#'   status assessment of Wildlife
#'   Species](https://cosewic.ca/index.php/en/assessment-process/cosewic-assessment-process-categories-and-guidelines/quantitative-criteria.html)
#'
#' @param df_db Either data frame or a connection to database with
#'   `naturecounts` table.
#' @param record Character. Name of the column containing record identification.
#' @param coord_lon Character. Name of the column containing longitude.
#' @param coord_lat Character. Name of the column containing latitude.
#' @param group Character. Name of the column containing group identification.
#'   By default this is `species_id` in NatureCounts data.
#' @param prop_include Numeric. The proportion of points to include in the range
#'   calculations (applies to both IAO and EOO calculations). This proportion of
#'   points closest to the centroid of the data are retained. Defaults to 1 for
#'   100% of points. Note that you may wish to use 0.95 to omit potential
#'   outlier points.
#' @param iao_grid_size_km Numeric. Size of grid (km) to use when calculating
#'   IAO. Default is COSEWIC requirement (2km, meaning 2x2km grids of 4km2). Use
#'   caution if changing.
#' @param eoo_clip sf (Multi)Polygon. A spatial object to clip the EOO to. May
#'   be relevant when calculating EOOs for complex regions (i.e. long curved
#'   areas) to avoid including area which cannot have observations.
#' @param iao_grid sf Polygon. Supply your own IAO grid rather than creating
#'   one. The CRS of this grid must be the same as `crs`.
#' @param filter_unique Logical. Whether to filter observations to unique
#'   locations. Use this only if there are too many data points to work with.
#'   This changes the nature of what an observation is, and may also affect
#'   which observations are omitted if using `prop_include < 1`.
#' @param spatial Logical. Whether to return sf spatial objects showing
#'   calculations. If `TRUE` (default) returns a list spatial data frames, `iao`
#'   and `eoo`. If `FALSE` returns a data frame with IAO and EOO values.
#' @param species Deprecated. Use `groups`.
#' @param eoo_p Deprectated. User `prop_include`.
#'
#' @inheritParams args
#'
#' @return If `spatial = TRUE`, a list with two spatial data frames, `iao` and
#'   `eoo`. Otherwise a data frame.
#'
#'  (Spatial) data frames contain the following columns
#'   - Group column (defined by `group`, defaults to `species_id`)
#'   - `n_records_total` - Total number of records used to create ranges (after
#'     filtering if `prop_include < 1`)
#'   - `prop_include` - The proportion of original points included in these
#'     calculations
#'
#'  Additionally the `iao` data frame contains
#'   - `grid_id` - ID number for grid cells
#'   - `n_records` - Number of records in that grid cell
#'   - `min_record` - Minimum number of records across all cells
#'   - `max_record` - Maximum number of records across all cells
#'   - `median_record` - Median number of records across all cells
#'   - `grid_size_km` - IAO cell size in km (i.e. width)
#'   - `n_occupied` - Across all cells, number of IAO cells with at least one record
#'   - `iao` - IAO value (`grid_size_km`^2 * `n_occupied`)
#'
#'  Additionally the `eoo` data frame contains
#'   - `eoo` - EOO area calculated from the Convex Hull
#'
#' @examples
#' # Using the included, test data on black-capped chickadees
#'
#' r <- cosewic_ranges(bcch)
#' r
#'
#' r <- cosewic_ranges(bcch, spatial = FALSE)
#' r
#'
#' # Calculate for multiple groups
#' mult <- rbind(bcch, hofi)
#' r <- cosewic_ranges(mult)
#' r <- cosewic_ranges(mult, spatial = FALSE)
#'
#' # Consider the Ontario MNR Lambert projection (all observations are in Ontario)
#' r2 <- cosewic_ranges(mult, crs = 3162)
#'
#' # Clip to a specific region
#' @examplesIf requireNamespace("rnaturalearth", quietly = TRUE) & requireNamespace("rnaturalearthhires", quietly = TRUE)
#'
#' library(rnaturalearth)
#' ON <- ne_states("Canada") %>%
#'   dplyr::filter(postal == "ON")
#'
#' r <- cosewic_ranges(mult)
#' cosewic_plot(r, map = ON) # No clip
#'
#' r <- cosewic_ranges(mult, eoo_clip = ON)
#' cosewic_plot(r, map = ON) # With clip
#'
#' # Use a custom IAO grid
#'
#' # Load the demo grid for the bcch data set
#' grid <- sf::st_read(system.file(
#'   "extdata",
#'   "iao_bcch_grid.gpkg",
#'   package = "naturecounts"
#' ))
#' r <- cosewic_ranges(bcch, iao_grid = grid)
#' cosewic_plot(r)
#'
#' # Slight differences when compared to internally created grid,
#' # just due to where the observations line up
#' r <- cosewic_ranges(bcch)
#' cosewic_plot(r)
#'
#' @export

cosewic_ranges <- function(
  df_db,
  record = "record_id",
  coord_lon = "longitude",
  coord_lat = "latitude",
  group = "species_id",
  prop_include = 1,
  iao_grid_size_km = 2,
  iao_grid = NULL,
  eoo_clip = NULL,
  crs = "ESRI:102001",
  which = c("eoo", "iao"),
  filter_unique = FALSE,
  spatial = TRUE,
  species,
  eoo_p
) {
  if (!missing(species)) {
    warning(
      "`species` is deprecated. Please use `group` instead",
      call. = FALSE
    )
    group <- species
  }

  if (!missing(eoo_p)) {
    #fmt: skip
    warning(
      "`eoo_p` is deprecated.\n",
      "`prop_include` now defines the proportion ",
      "of observations included in both IAO and EOO calculations.\n",
      "Setting `prop_include = ", eoo_p, "`",
      call. = FALSE
    )
    prop_include <- eoo_p
  }

  # Checks
  have_pkg_check("sf")
  df <- df_db_check(df_db)
  which_check(which)
  if (prop_include > 1 || prop_include < 0) {
    stop("`prop_include` must be a proportion between 0 and 1", call. = FALSE)
  }

  # Alerts
  rlang::inform(
    paste0(
      "(This message is shown once per session)\n",
      "As of naturecounts v0.5.0 `cosewic_ranges()` now uses `prop_include = 1` ",
      "instead of `eoo_p = 0.95`. \nThis defines the proportion of ",
      "observations used in both IAO and EOO calculations.\n",
      "The default is `prop_include = 1` (include all observations)."
    ),
    .frequency = "once",
    .frequency_id = "eoo_p"
  )

  # Coords
  if (!all(c(coord_lat, coord_lon) %in% names(df))) {
    stop(
      "`coord_lat` and `coord_lon` must be columns in `df_db`",
      call. = FALSE
    )
  } else if (!all(is.numeric(df[[coord_lat]]), is.numeric(df[[coord_lat]]))) {
    stop("`coord_lat` and `coord_lon` must be numeric", call. = FALSE)
  }

  # Clip
  if (
    !is.null(eoo_clip) &&
      !inherits(eoo_clip, "sf") &&
      !all(sf::st_is(eoo_clip, c("POLYGON", "MULTIPOLYGON")))
  ) {
    stop("If provided, `eoo_clip` must be an sf polygon object", call. = FALSE)
  }

  # IAO Grid
  if (
    !is.null(iao_grid) &&
      !inherits(iao_grid, "sf") &&
      !all(sf::st_is(eoo_clip, c("POLYGON", "MULTIPOLYGON")))
  ) {
    stop("If provided, `iao_grid` must be an sf polygon object", call. = FALSE)
  }
  # Check custom grid
  if (!is.null(iao_grid) && sf::st_crs(iao_grid) != sf::st_crs(crs)) {
    stop("`crs` must match the CRS of `iao_grid`", call. = FALSE)
  }

  # Columns
  if (!is.null(group) && !group %in% names(df)) {
    warning(
      "Column \"",
      group,
      "\" not found in `df_db`. ",
      "Treating data as single group.\n",
      "Use `group = NULL` to remove this warning or ",
      "`group = \"COLUMN_NAME\"` to specify the group id column.",
      call. = FALSE
    )
    df[[group]] <- "PLACEHOLDER"
  }
  if (is.null(group)) {
    group <- "species_id"
    df[[group]] <- "PLACEHOLDER"
  }

  if (!is.null(record) && !record %in% names(df)) {
    warning(
      "Column \"",
      record,
      "\" not found in `df_db`. ",
      "Using row number as record id.\n",
      "use `record = NULL` to remove this warning or ",
      "`record = \"COLUMN_NAME\"` to specify the record id column.",
      call. = FALSE
    )
    df[[record]] <- dplyr::row_number(df[[1]])
  }

  if (is.null(record)) {
    record <- "record_id"
    df[[record]] <- dplyr::row_number(df[[1]])
  }

  # Filter to unique locations?
  if (filter_unique) {
    warning(
      "Filtering to unique lat/lon locations (records now equal locations).\n",
      dplyr::if_else(
        prop_include != 1,
        paste0(
          "This may bias which observations are filtered out with `prop_include = ",
          prop_include,
          "`\n"
        ),
        ""
      ),
      "Only do this if the number of observations is too high to process",
      call. = FALSE
    )

    df <- df %>%
      dplyr::select(
        dplyr::all_of(c(group, coord_lon, coord_lat))
      ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(!!record := 1:dplyr::n())
  }

  # Set units
  cell_size <- units::as_units(iao_grid_size_km, "km")

  df_sf <- prep_spatial(
    df,
    coords = c(coord_lon, coord_lat),
    extra = c(record, group),
    crs = crs,
    p = prop_include,
    check_projected = TRUE
  )

  n <- dplyr::count(
    sf::st_drop_geometry(df_sf),
    .data[[group]],
    name = "n_records_total"
  )

  # Calculate
  # Use split to maintain lists which keep the spatial aspect, nested not so much

  ranges <- tidyr::nest(df_sf, .by = dplyr::all_of(group)) %>%
    dplyr::left_join(n, by = group) %>%
    dplyr::relocate(dplyr::all_of(group), "n_records_total")

  if ("iao" %in% which) {
    iao <- dplyr::mutate(
      ranges,
      iao = purrr::map(
        .data[["data"]],
        \(x) {
          cosewic_iao(
            x,
            cell_size,
            record,
            spatial,
            crs = .env$crs,
            grid = iao_grid
          )
        }
      )
    ) %>%
      dplyr::select(-"data") %>%
      tidyr::unnest("iao")
  }

  if ("eoo" %in% which) {
    eoo <- dplyr::mutate(
      ranges,
      eoo = purrr::map(
        .data[["data"]],
        \(x) cosewic_eoo(x, clip = eoo_clip, spatial)
      )
    ) %>%
      dplyr::select(-"data") %>%
      tidyr::unnest("eoo")

    if ("iao" %in% which) {
      # Check eoo size
      i <- iao %>%
        sf::st_drop_geometry() %>%
        dplyr::select(dplyr::all_of(c(group, "iao"))) %>%
        dplyr::distinct()

      if (any(eoo$eoo < unique(i$iao))) {
        s <- unique(eoo[[group]][eoo$eoo < i$iao])
        message(
          "EOO is less than IAO for group ",
          paste0(s, collapse = ", "),
          ".\n",
          "This can occur if there are very few, clustered records.\n",
          "Making EOO equal to IAO.\n(see 'Instructions for preparing COSEWIC ",
          "status reports' in ?cosewic_ranges)"
        )
        eoo$eoo[eoo$eoo < i$iao] <- i$iao
      }
    }

    eoo
  }

  if (all(unique(df[[group]]) == "PLACEHOLDER")) {
    if ("iao" %in% which) {
      iao <- dplyr::select(iao, -dplyr::all_of(group))
    }
    if ("eoo" %in% which) {
      eoo <- dplyr::select(eoo, -dplyr::all_of(group))
    }
    group <- NULL
  }

  if (spatial) {
    ranges <- list()
    if ("iao" %in% which) {
      ranges <- append(ranges, list(iao = sf::st_as_sf(iao)))
    }
    if ("eoo" %in% which) {
      ranges <- append(ranges, list(eoo = sf::st_as_sf(eoo)))
    }
  } else {
    if (all(c("iao", "eoo") %in% which)) {
      ranges <- dplyr::full_join(
        iao,
        eoo,
        by = c(group, "n_records_total", "prop_include")
      ) %>%
        dplyr::relocate("prop_include", .after = dplyr::last_col())
    } else if ("iao" %in% which) {
      ranges <- iao
    } else {
      ranges <- eoo
    }
  }

  ranges
}

# Faster grids https://github.com/r-spatial/sf/issues/1579
cosewic_iao <- function(df_sf, cell_size, record, spatial, crs, grid = NULL) {
  if (is.null(grid)) {
    grid_ca <- grid_canada(buffer = 500, crs = crs)

    # Check if all points in grid
    missing <- !sf::st_within(df_sf, sf::st_union(grid_ca), sparse = FALSE)
    if (any(missing)) {
      ids <- df_sf[[record]][which(missing)]
      message(
        "  Some observations not within the limits of Canada and a 500km buffer",
        "\n  Omitting record(s): ",
        paste0(ids, collapse = ", ")
      )
      df_sf <- dplyr::filter(df_sf, !.data[[record]] %in% ids)
    }

    grid_lg <- grid_filter(grid_ca, df_sf, cell_size = cell_size * 5) %>%
      dplyr::bind_rows()

    grid <- grid_filter(grid_lg, df_sf, cell_size = cell_size) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(grid_id = 1:dplyr::n())
  } else {
    # Prepare custom grid
    grid <- dplyr::mutate(grid, grid_id = dplyr::row_number())

    # Only use range of grid which is required
    grid <- sf::st_crop(sf::st_set_agr(grid, "constant"), sf::st_bbox(df_sf))

    units <- paste0(sf::st_crs(grid)$units_gdal, "^2")
    cell_size <- sf::st_area(grid) %>%
      stats::median() %>%
      units::set_units(units, mode = "standard") %>%
      units::set_units("km2") %>%
      sqrt()
    message("User-provided grid has cell size of ", format(cell_size))
  }

  iao_full <- grid %>%
    sf::st_join(df_sf) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data$grid_id) %>%
    dplyr::summarize(n_records = sum(!is.na(.data[[record]])), .groups = "drop")

  if (sum(iao_full$n_records) != nrow(df_sf)) {
    stop("Records incorrectly assigned to grids", call. = FALSE)
  }

  iao <- iao_full %>%
    dplyr::filter(.data$n_records > 0) %>%
    dplyr::summarize(
      min_record = min(.data$n_records),
      max_record = max(.data$n_records),
      median_record = stats::median(.data$n_records),
      grid_size_km = .env$cell_size,
      n_occupied = dplyr::n(),
      iao = .data$n_occupied * .env$cell_size^2
    ) |>
    dplyr::mutate(prop_include = .env$df_sf$prop_include[1])

  if (spatial) {
    iao <- dplyr::right_join(grid, iao_full, by = "grid_id") %>%
      dplyr::bind_cols(iao)
  }

  iao
}

cosewic_eoo <- function(df_sf, clip, spatial) {
  eoo <- df_sf %>%
    sf::st_cast(to = "POINT") %>%
    sf::st_union() %>%
    sf::st_convex_hull() %>%
    sf::st_as_sf()

  if (!is.null(clip)) {
    clip <- sf::st_transform(clip, sf::st_crs(eoo))
    eoo_clipped <- sf::st_intersection(
      sf::st_set_agr(eoo, "constant"),
      sf::st_set_agr(clip, "constant")
    )
    if (nrow(eoo_clipped) == 0) {
      warning(
        "Clipping EOO results in no EOO, using non-clipped EOO instead",
        call. = FALSE
      )
    } else {
      eoo <- eoo_clipped
    }
  }

  eoo <- eoo %>%
    dplyr::mutate(
      eoo = sf::st_area(eoo),
      eoo = units::set_units(.data$eoo, "km^2"),
      prop_include = .env$df_sf$prop_include[1]
    )

  if (!spatial) {
    eoo <- sf::st_drop_geometry(eoo)
  }

  eoo
}


prep_spatial <- function(
  df,
  coords = c("longitude", "latitude"),
  extra = "record_id",
  crs,
  p,
  check_projected = TRUE
) {
  if (check_projected && sf::st_is_longlat(sf::st_crs(crs))) {
    stop(
      "CRS is unprojected, area calculations should use a projected CRS.",
      call. = FALSE
    )
  }
  if (any(is.na(df[coords]))) {
    n <- which(is.na(df[[coords[1]]]) | is.na(df[[coords[2]]]))
    if (length(n) > 10) {
      n <- paste0(paste0(n[1:10], collapse = ", "), "...")
    } else {
      n <- paste0(n, collapse = ", ")
    }
    warning("Removing missing coordinates in rows: ", n, call. = FALSE)
  }

  df_sf <- df %>%
    tidyr::drop_na(dplyr::all_of(coords)) %>%
    dplyr::select(dplyr::all_of(c(extra, coords))) %>%
    sf::st_as_sf(coords = coords, crs = 4326) %>%
    sf::st_transform(crs) %>%
    sf::st_set_agr("constant")

  df_sf <- filter_spatial(df_sf, p)

  df_sf
}

filter_spatial <- function(df_sf, p) {
  if (p != 1) {
    center <- df_sf %>%
      sf::st_union() %>%
      sf::st_convex_hull() %>%
      sf::st_centroid()

    df_sf <- df_sf %>%
      dplyr::mutate(
        dist = sf::st_distance(.data$geometry, .env$center)[, 1]
      ) %>%
      dplyr::filter(.data$dist <= stats::quantile(.data$dist, .env$p)) %>%
      dplyr::select(-"dist")
  }

  df_sf <- dplyr::mutate(df_sf, prop_include = .env$p)

  df_sf
}

#' Create grid across Canada
#'
#' @param cell_size Numeric. Size of grid (km) to use when creating grid.
#'   If using this grid as input to `cosewic_ranges()`, should use default
#'   COSEWIC grid size of 2.
#' @param buffer Numeric. Extra buffer (km) to add around the outline of Canada
#'   before calculating grid.
#' @param crs Character. CRS for the grid to create.
#'
#' @return sf data frame with polygon grid
#' @export
#'
#' @examples
#'
#' gc <- grid_canada(200)
#' gc_buff <- grid_canada(200, buffer = 0)
#'
#' # Plot to illustrate
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = gc) +
#'   geom_sf(data = map_canada(), fill = NA) +
#'   labs(caption = "200km buffer")
#'
#' ggplot() +
#'   geom_sf(data = gc_buff) +
#'   geom_sf(data = map_canada(), fill = NA) +
#'   labs(caption = "No buffer")

grid_canada <- function(cell_size = 200, buffer = 500, crs = "ESRI:102001") {
  have_pkg_check("sf")

  map_canada(crs = crs) %>%
    sf::st_buffer(units::set_units(buffer, "km")) %>%
    make_grid(cell_size) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(grid_ca_id = 1:dplyr::n(), grid_size = .env$cell_size)
}


#' Filter df by a grid and create a smaller grid
#'
#' @param grid sf polygon grid
#' @param df_sf sf data frame
#' @param cell_size Numeric. Cell size in km
#'
#' @examples
#' # Convert to spatial
#' bcch_sf <- sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326)
#' grid_filter(grid_canada(), bcch_sf, cell_size = 5)
#'
#' @noRd
grid_filter <- function(grid, df_sf, cell_size, verbose = TRUE) {
  if (sf::st_crs(grid) != sf::st_crs(df_sf)) {
    if (verbose) {
      message("Transforming `df_sf` to CRS of `grid`")
    }
    df_sf <- sf::st_transform(df_sf, sf::st_crs(grid))
  }

  sf::st_filter(grid, df_sf) %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    split(.$id) %>%
    purrr::map(~ make_grid(.x, cell_size))
}

make_grid <- function(df_sf, cell_size) {
  if (sf::st_crs(df_sf)$units_gdal == "degree") {
    stop(
      "Cannot create IAO grids with unprojected (geographic) Coordinate Reference Systems",
      call. = FALSE
    )
  }
  cell_size <- units::set_units(cell_size, "km")
  cell_size <- units::set_units(cell_size, "m")
  cell_size <- as.numeric(cell_size)

  df_sf %>%
    sf::st_bbox() %>%
    wk::as_rct() %>%
    wk::grd(dx = cell_size, dy = cell_size) %>%
    sf::st_as_sf()
}

#' Map of Canada
#'
#' Wrapper around `rnaturalearth::ne_countries()` to creates a simple features
#' basic map of Canada with a custom CRS (3347, Statistics Canada Lambert by
#' default).
#'
#' @inheritParams args
#'
#' @return sf data frame
#' @export
#'
#' @examples
#' map_canada()
#'
#' plot(map_canada())
#'
#' library(ggplot2)
#' ggplot(data = map_canada()) + geom_sf()

map_canada <- function(crs = 3347) {
  have_pkg_check("rnaturalearth")
  have_pkg_check("sf")

  # TODO: revert to no suppression once sf migration complete
  suppressPackageStartupMessages({
    rnaturalearth::ne_countries(country = "Canada", returnclass = "sf") %>%
      sf::st_transform(crs = crs)
  })
}


#' Plot COSEWIC IAO and EOO
#'
#' Creates a plot of COSEWIC ranges for illustration and checking. **Note**: If
#' using maptiles from OpenStreetMap ("osm", the default) in a public
#' document/website/etc., you must [attribute
#' OpenStreetMap](https://osmfoundation.org/wiki/Licence/Attribution_Guidelines).
#'
#' @param ranges List. Output of `cosewic_ranges()` with `spatial = TRUE`.
#' @param points Data frame. Optional raw data points to add to the plot (are
#'   not filtered, regardless if a `prop_include < 1` was used in
#'   `cosewic_ranges()`.
#' @param grid sf data frame. Optional grid over which to summarize IAO values
#'   (useful for species with many points over a broad distribution).
#' @param map Character or sf data frame. Underlying base map over which to plot
#'   the values.. "osm" by default to use OpenStreetMap base maps via
#'   `ggspatial::annotation_map_tile()`. Can be one of `rosm::osm.types()`, a sf
#'   polygon base map, or `NULL` for no base map.
#' @param iao_prop Logical. Whether to show IAO as a proportion for
#'   easier plotting of multiple groups (allows collecting legends by
#'   the patchwork package).
#' @param group Character. Name of the column containing group identification.
#'   By default this is `species_id` in NatureCounts data.
#' @param title Character. Optional title to add to the map. Can be a named by
#'  group vector to supply different titles for different groups.
#' @param zoomin Numeric. Zoom adjustment for
#'   `ggspatial::annotation_map_tile()`. Only applies if map defines a map tile
#'   (e.g., "osm")
#' @param arrow_location Character. Location for the North arrow, one of 'tr',
#' 'tl', 'br', or 'bl', for top right, top left, etc. `NULL` omits the arrow.
#' @param scale_location Character. Location for the map scale, one of 'tr',
#' 'tl', 'br', or 'bl', for top right, top left, etc. `NULL` omits the scale.
#' @param species Deprecated. Use `groups`.
#'
#' @inheritParams args
#'
#' @return ggplot2 map
#' @export
#'
#' @examples
#' r <- cosewic_ranges(bcch)
#' cosewic_plot(r)
#' cosewic_plot(r, points = bcch)
#'
#' # Only one or the other
#' cosewic_plot(r, which = "eoo", points = bcch)
#' cosewic_plot(r, which = "iao")
#'
#' # Use a different CRS for the map (only applies if not using map tiles)
#' cosewic_plot(r, crs = 3347) # No change
#' cosewic_plot(r, map = map_canada(), crs = 3347)
#'
#' # Summarize IAO over larger grid
#' cosewic_plot(
#'   r,
#'   grid = grid_canada(50),
#'   map = map_canada(),
#'   title = "Black-capped chickadees"
#' )
#'
#' # Plot multiple groups - separate plots
#' m <- rbind(bcch, hofi)
#' r <- cosewic_ranges(m)
#' p <- cosewic_plot(r)
#' p[[1]]
#' p[[2]]
#'
#' # Plot multiple groups - Use IAO as a proportion for identical legends
#' p <- cosewic_plot(
#'   r,
#'   iao_prop = TRUE,
#'   title = c("14280" = "Black-capped chickadees", "20350" = "House Finches")
#' )
#'
#' # Use patchwork to combine into a single figure
#' if(requireNamespace("patchwork", quietly = TRUE)) {
#'   library(patchwork)
#'   wrap_plots(p) +
#'     plot_layout(guides = "collect")
#' }

cosewic_plot <- function(
  ranges,
  which = c("eoo", "iao"),
  points = NULL,
  grid = NULL,
  map = "osm",
  iao_prop = FALSE,
  crs = NULL,
  group = "species_id",
  title = "",
  zoomin = -1,
  arrow_location = "tr",
  scale_location = "br",
  verbose = TRUE,
  species
) {
  if (!missing(species)) {
    warning(
      "`species` is deprecated. Please use `group` instead",
      call. = FALSE
    )
    group <- species
  }

  have_pkg_check(c("sf", "ggplot2", "ggspatial", "prettymapr", "rosm"))
  which_check(which)

  for (x in which) {
    sf_check(ranges[[x]], name = "ranges")
  }

  # Extract ranges
  if ("iao" %in% which) {
    ranges[["iao"]] <- dplyr::filter(ranges[["iao"]], .data$n_records > 0)
  }

  # Check Group Columns
  cols <- purrr::map(ranges, names) %>% purrr::list_c()
  if (is.null(group)) {
    group <- "species_id"
    ranges$eoo[[group]] <- "PLACEHOLDER"
    ranges$iao[[group]] <- "PLACEHOLDER"
  } else if (!group %in% cols) {
    warning(
      "Column \"",
      group,
      "\" not found in spatial data in `ranges`. ",
      "Treating data as single group.\n",
      "Use `group = NULL` to remove this warning or ",
      "`group = \"COLUMN_NAME\"` to specify the group id column.",
      call. = FALSE
    )
    ranges$eoo[[group]] <- "PLACEHOLDER"
    ranges$iao[[group]] <- "PLACEHOLDER"
  }

  sp <- purrr::map(ranges, group) %>%
    purrr::list_c() %>%
    unique()

  # Check/set titles
  if (length(title) > 1 && !all(names(title) %in% sp)) {
    stop(
      "`title` must be a named vector matching 'group' ids if providing more than one",
      call. = FALSE
    )
  }

  g <- list()

  if (all(title == "") & sp[1] != "PLACEHOLDER") {
    title <- stats::setNames(nm = sp)
  }

  # Split by group (if applicable)
  if ("eoo" %in% which) {
    e <- split(ranges[["eoo"]], ranges[["eoo"]][[group]])
  } else {
    e <- NA
  }

  if ("iao" %in% which) {
    i <- split(ranges[["iao"]], ranges[["iao"]][[group]])
  } else {
    i <- NA
  }

  if (!is.null(points)) {
    points <- split(points, points[[group]])
  } else {
    points <- list(points)
  }

  # Get correct order of titles for the next section.
  if (!is.null(names(title))) {
    if ("eoo" %in% which) {
      title <- title[names(e)]
    } else {
      title <- title[names(i)]
    }
  }

  g <- purrr::pmap(
    list(e, i, points, title),
    \(e, i, points, title) {
      cosewic_plot_indiv(
        e,
        i,
        which,
        points,
        grid,
        map,
        iao_prop,
        title,
        crs,
        zoomin,
        arrow_location,
        scale_location,
        verbose
      )
    }
  )

  if (length(g) == 1) {
    g <- g[[1]]
  }
  g
}


cosewic_plot_indiv <- function(
  e,
  a,
  which,
  points,
  grid,
  map,
  iao_prop,
  title,
  crs,
  zoomin,
  arrow_location,
  scale_location,
  verbose
) {
  if ("iao" %in% which) {
    iao_val <- a$iao[1]
    size_a <- unique(a$grid_size_km)
    records <- paste0(
      a$n_records_total[1],
      " records (",
      a$min_record[1],
      "-",
      a$max_record[1],
      " records per ",
      size_a,
      "x",
      size_a,
      " km grid); ",
      a$prop_include[1] * 100,
      "% of total records"
    )
    if (!is.null(grid)) {
      if (sf::st_crs(a) != sf::st_crs(grid)) {
        a <- sf::st_transform(a, sf::st_crs(grid))
        if (verbose) {
          message("Transforming IAO spatial data to grid CRS for summarizing")
        }
      }
      a <- a %>%
        sf::st_join(grid, ., left = FALSE) %>% # Inner join
        dplyr::group_by(.data$grid_ca_id) %>%
        dplyr::summarize(n_records = sum(.data$n_records))
      size_p <- grid$grid_size[1]
      caption <- paste0(
        records,
        "\nSummarized to display as ",
        size_p,
        "x",
        size_p,
        "km grids"
      )
    } else {
      caption <- records
    }

    if (iao_prop) {
      a <- dplyr::mutate(
        a,
        n_records = .data$n_records / max(.data$n_records, na.rm = TRUE)
      )
      leg_title <- "IAO\nProp. records"
    } else {
      leg_title <- "IAO\nNo. records"
    }
  } else {
    caption <- paste0(
      e$n_records_total[1],
      " records; ",
      e$prop_include[1] * 100,
      "% of total records"
    )
  }

  if ("iao" %in% which) {
    caption <- paste0(caption, "\n", paste("IAO:", format(iao_val)))
  }
  if ("eoo" %in% which) {
    caption <- paste0(
      caption,
      "\n",
      paste("EOO:", format(round(e$eoo[1], 2)))
    )
  }

  caption <- stringr::str_remove_all(caption, "\\[|\\]|\\^")

  g <- ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.caption = ggplot2::element_text(lineheight = 1.25)) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(caption = caption)

  if (!is.null(map)) {
    if (is.character(map) && map %in% rosm::osm.types()) {
      g <- g + ggspatial::annotation_map_tile(type = map, zoomin = zoomin)
    } else if (inherits(map, "sf")) {
      g <- g + ggplot2::geom_sf(data = map, fill = NA)
    } else {
      stop(
        "`map` must either be a polygon shape file or one of ",
        paste0(rosm::osm.types(), collapse = ","),
        call. = FALSE
      )
    }
  }

  if (!is.null(arrow_location)) {
    g <- g +
      ggspatial::annotation_north_arrow(
        style = ggspatial::north_arrow_fancy_orienteering,
        location = arrow_location
      )
  }
  if (!is.null(scale_location)) {
    g <- g +
      ggspatial::annotation_scale(style = "ticks", location = scale_location)
  }

  if ("eoo" %in% which) {
    g <- g +
      ggplot2::geom_sf(
        data = e,
        ggplot2::aes(colour = "EOO"),
        fill = NA,
        size = 1
      ) +
      ggplot2::scale_colour_manual(name = "", values = "grey60")
  }

  if ("iao" %in% which) {
    g <- g +
      ggplot2::geom_sf(
        data = a,
        ggplot2::aes(fill = .data$n_records),
        colour = NA
      ) +
      ggplot2::scale_fill_viridis_c(name = leg_title)
  }

  if (!is.null(points)) {
    points <- prep_spatial(
      points,
      p = 1,
      extra = NULL,
      crs = if (is.null(crs)) sf::st_crs("EPSG:4326") else sf::st_crs(crs),
      check_projected = FALSE
    )
    g <- g + ggplot2::geom_sf(data = points)
  }

  if (!is.null(crs)) {
    if (is.character(map) && sf::st_crs(crs) != sf::st_crs("EPSG:3857")) {
      message(
        "'crs' is only applicable when not using map tiles. ",
        "Map tiles always use CRS of EPSG:3857."
      )
    }
    g <- suppressMessages(g + ggplot2::coord_sf(crs = crs))
  }

  g
}
