#' Calculate COSEWIC IAO and EOO
#'
#' The COSEWIC Index of Area of Occupancy (IAO; also called Area of Occupancy,
#' AOO by the IUCN) and Extent of Occurrence (EOO; IUCN as well) are metrics
#' used to support status assessments for potentially endangered species.
#'
#' Note that the while the IUCN calls this metric AOO, in COSEWIC, AOO is actually
#' a different measure, the *biological* area of occupancy. See the
#' [Distribution](https://cosewic.ca/index.php/en-ca/reports/preparing-status-reports/instructions-preparing-status-reports.html#Distribution)
#' section in 'Instructions for preparing COSEWIC status reports'
#' for more details.
#'
#' By default the EOO is calculated using all points (`eoo_p = 1` for 100% of
#' However, if you're working on rough data or want to do a rough first pass,
#' you may wish to use `eoo_p = 0.95` or 95% of points (based on distance to the
#' centroid). This will ensure outlier observations will not artificially
#' inflate the EOO. However, for a final COSEWIC assessment report, it is likely
#' better to carefully explore the data to ensure there are no outliers and then
#' use the full data set (i.e. use the default of `eoo_p = 1`).
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
#' - COSEWIC - [Guidelines for use of the Index of Area of Occupancy in COSEWIC Assessments](https://www.cosewic.ca/index.php/en-ca/reports/preparing-status-reports/guidelines-index-area-occupancy.html)
#' - COSEWIC - [Instructions for preparing COSEWIC status reports](https://cosewic.ca/index.php/en-ca/reports/preparing-status-reports/instructions-preparing-status-reports.html)
#' - COSEWIC - [Table 2 COSEWIC quantitative criteria and guidelines for the status assessment of Wildlife Species](https://www.cosewic.ca/index.php/en-ca/assessment-process/wildlife-species-assessment-process-categories-guidelines/quantitative-criteria.html)
#'
#' @param df_db Either data frame or a connection to database with
#'   `naturecounts` table.
#' @param record Character. Name of the column containing record
#'   identification.
#' @param coord_lon Character. Name of the column containing longitude.
#' @param coord_lat Character. Name of the column containing latitude.
#' @param species Character. Name of the column containing species
#'   identification.
#' @param iao_grid_size_km Numeric. Size of grid (km) to use when calculating
#'   IAO. Default is COSEWIC requirement (2km, meaning 2x2km grids of 4km2).
#'   Use caution if changing.
#' @param eoo_p Numeric. The percentile to calculate the convex hull over.
#'   Defaults to 1 for a 100% convex hull to match COSEWIC requirements
#'   (includes all points). Note that you may wish to use 0.95 for a 95% convex
#'   hull to  ensure outlier points do not artificially inflate the EOO.
#' @param eoo_clip sf (Multi)Polygon. A spatial object to clip the EOO to. May
#'   be relevant when calculating EOOs for complex regions (i.e. long curved
#'   areas) to avoid including area which cannot have observations.
#' @param iao_grid sf Polygon. Supply your own IAO grid rather than creating
#'   one. The CRS of this grid must be the same as `crs`.
#' @param filter_unique Logical. Whether to filter observations to unique
#'   locations. Use this only if there are too many data points to work with.
#'   This changes the nature of what an observation is, and also may bias
#'   EOO calculations if using less than 100% of points (see `eoo_p`).
#' @param spatial Logical. Whether to return sf spatial objects showing
#'   calculations. If `FALSE` (the default) returns a data frame with the IAO
#'   and EOO values. If `TRUE` returns a list of objects with both the
#'   values and the spatial grid/polygons.
#'
#' @inheritParams args
#'
#' @return Summarized data frame (ranges) or list containing `ranges`, a
#'   summarized data frame, and `spatial`, a list of two spatial data frames.
#'
#' `ranges` contains columns
#'   - `n_records_total` - Total number of records used to create ranges
#'   - `min_record` - Minimum number of records within IAO cells
#'   - `max_record` - Maximum number of records within IAO cells
#'   - `median_record` - Median number of records within IAO cells
#'   - `grid_size_km` - IAO cell size (area is this squared)
#'   - `n_occupied` - Number of IAO cells with at least one record
#'   - `iao` - IAO value (`grid_size_km`^2 * `n_occupied`)
#'   - `eoo_pXX` - EOO area calculated with a convex hull at percentile `eoo_p`
#'     (e.g., 100% or 95%)
#'
#' `spatial` contains spatial data frames
#' - `iao_sf` - Polygons of the IAO grids with the `n_records` per cell
#' - `eoo_sf` - Polygon of the Convex Hull at percentile `eoo_p`
#'
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
#' # Calculate for multiple species
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
  species = "species_id",
  iao_grid_size_km = 2,
  iao_grid = NULL,
  eoo_p = 1,
  eoo_clip = NULL,
  crs = "ESRI:102001",
  which = c("eoo", "iao"),
  filter_unique = FALSE,
  spatial = TRUE
) {
  # Checks
  have_pkg_check("sf")
  df <- df_db_check(df_db)
  which_check(which)

  # Alerts
  rlang::inform(
    paste0(
      "As of naturecounts v0.5.0 `cosewic_ranges()` now uses a default of ",
      "`eoo_p = 1` instead of `eoo_p = 0.95`."
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
  if (!is.null(species) && !species %in% names(df)) {
    warning(
      "Column \"",
      species,
      "\" not found in `df_db`. ",
      "Treating data as single species.\n",
      "Use `species = NULL` to remove this warning or ",
      "`species = \"COLUMN_NAME\"` to specify the species id column.",
      call. = FALSE
    )
    df[[species]] <- "PLACEHOLDER"
  }
  if (is.null(species)) {
    species <- "species_id"
    df[[species]] <- "PLACEHOLDER"
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
        eoo_p != 1,
        "This may bias non-100% EOO calculations\n",
        ""
      ),
      "Only do this if the number of observations is too high to process",
      call. = FALSE
    )

    df <- df %>%
      dplyr::select(
        dplyr::all_of(c(species, coord_lon, coord_lat))
      ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(!!record := 1:dplyr::n())
  }

  # Set units
  cell_size <- units::as_units(iao_grid_size_km, "km")

  df_sf <- prep_spatial(
    df,
    coords = c(coord_lon, coord_lat),
    extra = c(record, species),
    crs = crs,
    check_projected = TRUE
  )

  n <- dplyr::count(df, .data[[species]], name = "n_records_total")

  # Calculate
  # Use split to maintain lists which keep the spatial aspect, nested not so much

  ranges <- tidyr::nest(df_sf, .by = dplyr::all_of(species)) %>%
    dplyr::left_join(n, by = species) %>%
    dplyr::relocate(dplyr::all_of(species), "n_records_total")

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
        \(x) cosewic_eoo(x, p = eoo_p, clip = eoo_clip, spatial)
      )
    ) %>%
      dplyr::select(-"data") %>%
      tidyr::unnest("eoo")

    if ("iao" %in% which) {
      # Check eoo size
      i <- iao %>%
        sf::st_drop_geometry() %>%
        dplyr::select(dplyr::all_of(c(species, "iao"))) %>%
        dplyr::distinct()

      if (any(eoo$eoo < unique(i$iao))) {
        s <- unique(eoo[[species]][eoo$eoo < i$iao])
        message(
          "EOO is less than IAO for species ",
          paste0(s, collapse = ", "),
          ".\n",
          "This can occur if there are very few, clustered records.\n",
          "Making EOO equal to IAO.\n(see 'Instructions for preparing COSEWIC ",
          "status reports' in ?cosewic_ranges)"
        )
        eoo$eoo[eoo$eoo < i$iao] <- i$iao
      }
    }

    # Assemble
    eoo <- dplyr::rename(eoo, !!paste0("eoo_p", eoo_p * 100) := "eoo")
  }

  if (all(unique(df[[species]]) == "PLACEHOLDER")) {
    if ("iao" %in% which) {
      iao <- dplyr::select(iao, -dplyr::all_of(species))
    }
    if ("eoo" %in% which) {
      eoo <- dplyr::select(eoo, -dplyr::all_of(species))
    }
    species <- NULL
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
      ranges <- dplyr::full_join(iao, eoo, by = c(species, "n_records_total"))
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
    )

  if (spatial) {
    iao <- dplyr::right_join(grid, iao_full, by = "grid_id") %>%
      dplyr::bind_cols(iao)
  }

  iao
}

cosewic_eoo <- function(df_sf, p, clip, spatial) {
  center <- df_sf %>%
    sf::st_union() %>%
    sf::st_convex_hull() %>%
    sf::st_centroid()

  eoo <- df_sf %>%
    dplyr::mutate(dist = sf::st_distance(.data$geometry, .env$center)[, 1]) %>%
    dplyr::filter(.data$dist <= stats::quantile(.data$dist, .env$p)) %>%
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
      eoo = units::set_units(.data$eoo, "km^2")
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
  check_projected = TRUE
) {
  if (check_projected && sf::st_is_longlat(sf::st_crs(crs))) {
    stop(
      "CRS is unprojected, area calculations should use a projected CRS.",
      call. = FALSE
    )
  }
  df %>%
    dplyr::select(dplyr::all_of(c(extra, coords))) %>%
    sf::st_as_sf(coords = coords, crs = 4326) %>%
    sf::st_transform(crs) %>%
    sf::st_set_agr("constant")
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
#' Creates a plot of COSEWIC ranges for illustration and checking.
#'
#' @param ranges List. Output of `cosewic_ranges()` with `spatial = TRUE`.
#' @param points Data frame. Optional naturecounts data used to compute ranges.
#'   Raw data points will be added to the plot if provided.
#' @param grid sf data frame. Optional grid over which to summarize IAO values
#'   (useful for species with many points over a broad distribution).
#' @param map Character or sf data frame. Underlying base map over which to plot
#'   the values.. "osm" by default to use OpenStreetMap base maps via
#'   `ggspatial::annotation_map_tile()`. Can be one of `rosm::osm.types()`, a sf
#'   polygon base map, or `NULL` for no base map.
#' @param iao_prop Logical. Whether to show IAO as a proportion for
#'   easier plotting of multiple species (allows collecting legends by
#'   the patchwork package).
#' @param species Character. Name of the column containing species
#'   identification.
#' @param title Character. Optional title to add to the map. Can be a named by
#'  species vector to supply different titles for different species.
#' @param zoomin Numeric. Zoom adjustment for
#'   `ggspatial::annotation_map_tile()`. Only applies if map defines a map tile
#'   (e.g., "osm")
#' @param arrow_location Character. Location for the North arrow, one of 'tr',
#' 'tl', 'br', or 'bl', for top right, top left, etc. `NULL` omits the arrow.
#' @param scale_location Character. Location for the map scale, one of 'tr',
#' 'tl', 'br', or 'bl', for top right, top left, etc. `NULL` omits the scale.
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
#' # Plot multiple species - separate plots
#' m <- rbind(bcch, hofi)
#' r <- cosewic_ranges(m)
#' p <- cosewic_plot(r)
#' p[[1]]
#' p[[2]]
#'
#' # Plot multiple species - Use IAO as a proportion for identical legends
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
  species = "species_id",
  title = "",
  zoomin = -1,
  arrow_location = "tr",
  scale_location = "br",
  verbose = TRUE
) {
  have_pkg_check(c("sf", "ggplot2", "ggspatial", "prettymapr", "rosm"))
  which_check(which)

  for (x in which) {
    sf_check(ranges[[x]], name = "ranges")
  }

  # Extract ranges
  if ("iao" %in% which) {
    ranges[["iao"]] <- dplyr::filter(ranges[["iao"]], .data$n_records > 0)
  }

  # Check Species Columns
  cols <- purrr::map(ranges, names) %>% purrr::list_c()
  if (is.null(species)) {
    species <- "species_id"
    ranges$eoo[[species]] <- "PLACEHOLDER"
    ranges$iao[[species]] <- "PLACEHOLDER"
  } else if (!species %in% cols) {
    warning(
      "Column \"",
      species,
      "\" not found in spatial data in `ranges`. ",
      "Treating data as single species.\n",
      "Use `species = NULL` to remove this warning or ",
      "`species = \"COLUMN_NAME\"` to specify the species id column.",
      call. = FALSE
    )
    ranges$eoo[[species]] <- "PLACEHOLDER"
    ranges$iao[[species]] <- "PLACEHOLDER"
  }

  sp <- purrr::map(ranges, species) %>%
    purrr::list_c() %>%
    unique()

  # Check/set titles
  if (length(title) > 1 && !all(names(title) %in% sp)) {
    stop(
      "`title` must be a named vector matching 'species' if providing more than one",
      call. = FALSE
    )
  }

  g <- list()
  if (all(title == "") & sp[1] != "PLACEHOLDER") {
    title <- stats::setNames(nm = sp)
  }

  # Split by species (if applicable)
  if ("eoo" %in% which) {
    e <- split(ranges[["eoo"]], ranges[["eoo"]][[species]])
  } else {
    e <- NA
  }

  if ("iao" %in% which) {
    i <- split(ranges[["iao"]], ranges[["iao"]][[species]])
  } else {
    i <- NA
  }

  if (!is.null(points)) {
    points <- split(points, points[[species]])
  } else {
    points <- list(points)
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
      " km grid)"
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
    caption <- paste0(e$n_records_total[1], " records")
  }

  if ("iao" %in% which) {
    caption <- paste0(caption, "\n", paste("IAO:", format(iao_val)))
  }
  if ("eoo" %in% which) {
    eoo <- stringr::str_which(names(e), "eoo")
    caption <- paste0(
      caption,
      "\n",
      paste("EOO:", format(round(e[[eoo]][1], 2)))
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
    eoo_legend <- stringr::str_subset(names(e), "eoo") %>%
      stringr::str_replace("_", " ") %>%
      stringr::str_replace("p(\\d{1,3})", "\\1%") %>%
      toupper()

    g <- g +
      ggplot2::geom_sf(
        data = e,
        ggplot2::aes(colour = !!eoo_legend),
        fill = NA,
        size = 1.5
      ) +
      ggplot2::scale_colour_manual(name = "", values = "grey20")
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
