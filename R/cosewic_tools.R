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
#' By default the EOO is calculated only using the inner 95% of points (based on
#' distance to the centroid). This is to ensure that a first-pass of the EOO
#' does not reject a species from consideration if there are any outlier
#' observations. However, for a final COSEWIC assessment report, it is likely
#' better to carefully explore the data to ensure there are no outliers and then
#' use the full data set (i.e. set `eoo_p = 1`).
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
#' @param record_id Character. Name of the column containing record
#'   identification.
#' @param coord_lon Character. Name of the column containing longitude.
#' @param coord_lat Character. Name of the column containing latitude. 
#' @param iao_grid_size_km Numeric. Size of grid (km) to use when calculating
#'   IAO. Default is COSEWIC requirement (2). Use caution if changing.
#' @param eoo_p Numeric. The percentile to calculate the convex hull over.
#'   Defaults to 0.95 for a 95% convex hull to ensure outlier points do not
#'   artificially inflate the EOO. Note that for a final COSEWIC report, this
#'   may not be appropriate. Set to 1 to include all points.
#' @param plot Logical. Whether to return plot
#' @param spatial Logical. Whether to return sf spatial objects showing
#'   calculations.
#'
#' @return Data frame or list containing data frame, and (optionally) plot, 
#' and spatial data frames.
#'
#' @examples
#' 
#' # Using the included, test data on black-capped chickadees
#'
#' bcch # look at the data
#'
#' r <- cosewic_ranges(bcch)
#' r <- cosewic_ranges(bcch, spatial = FALSE)
#' r <- cosewic_ranges(bcch, spatial = FALSE, plot = FALSE)
#'
#' # Use multiple species
#' library(purrr)
#' library(dplyr)
#' library(tidyr)
#' 
#' # Get multiple species
#' mult <- rbind(bcch, hofi)
#' 
#' # Nest
#' mult <- nest(mult,  .by = species_id)
#' mult
#' 
#' # Calculate ranges for nested data
#' r <- mutate(mult, results = map(data, cosewic_ranges, plot = FALSE, spatial = FALSE))
#' r <- unnest(r, results)
#' r
#' 
#' # To also return plot and spatial
#' r <- mutate(mult, results = map(data, cosewic_ranges))
#' r <- unnest_wider(r, results) %>%
#'   unnest(ranges)
#' r
#' 
#' # Take a look at one specifically
#' r$plot[[1]]
#'
#' @export 

cosewic_ranges <- function(df_db, 
                           record_id = "record_id", 
                           coord_lon = "longitude",
                           coord_lat = "latitude",
                           iao_grid_size_km = 2,
                           eoo_p = 0.95,
                           plot = TRUE,
                           spatial = TRUE) {
  
  # Checks
  have_sf_check()
  df <- df_db_check(df_db)
  
  # Check species
  if("species_id" %in% names(df) && dplyr::n_distinct(df$species_id) > 1) {
    stop("Multiple species detected. See examples in ?cosewic_ranges for how ",
         "to run multiple species.", call. = FALSE)
  }
  
  # CHECK COORDS
  if(!all(c(coord_lat, coord_lon) %in% names(df))) {
    stop("`coord_lat` and `coord_lon` must be columns in `df_db`", call. = FALSE)
  } else if (!all(is.numeric(df[[coord_lat]]), is.numeric(df[[coord_lat]]))) {
    stop("`coord_lat` and `coord_lon` must be numeric", call. = FALSE)
  }
  
  # Note on CRS
  # GeoCAT uses Google's projection EPSG:3857, Web Mercator because of backend
  # Probably should use Canadian projection. Currently using 3347 (Stats 
  # Canada).
  
  df_sf <- prep_spatial(df)
  
  # Set units
  cell_size <- units::as_units(iao_grid_size_km, "km")

  # Calculate
  eoo <- cosewic_eoo(df_sf, p = eoo_p)
  iao <- cosewic_iao(df_sf, cell_size, record_id)
  
  # Assemble
  ranges <- dplyr::mutate(iao[["iao"]], eoo = .env$eoo[["eoo"]])
 
  if(ranges$eoo < ranges$iao) {
   message(
   "EOO is less than IAO. This can occur if there are very few, clustered ",
   "records.\nMaking EOO equal to IAO ",
   "(see 'Instructions for preparing COSEWIC status reports'",
   "\nin ?cosewic_ranges)")
    ranges$eoo <- ranges$iao
  }
  
  ranges <- dplyr::rename(ranges, !!paste0("eoo_p", eoo_p*100) := "eoo")
  
  if(plot) {
    a <- dplyr::mutate(iao[["iao_sf"]], 
                       n_records = dplyr::na_if(.data$n_records, 0))
    g <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::geom_sf(data = a, ggplot2::aes(fill = n_records), 
                       colour = "grey") +
      ggplot2::geom_sf(data = eoo[["eoo_sf"]], linewidth = 1.5, fill = NA,
                       ggplot2::aes(colour = "EOO")) +
      ggplot2::geom_sf(data = df_sf) +
      ggplot2::scale_fill_viridis_c(name = "No. records\nper cell",
                                    na.value = NA) +
      ggplot2::scale_colour_manual(name = NULL, values = "black") +
      ggplot2::labs(
        caption = paste0("Filled cells represent IAO; ",
                         "Black outline represents EOO\n",
                         "Grid is ", iao_grid_size_km, "x", iao_grid_size_km, 
                         "km"))
  } else {
    g <- NULL
  } 
  
  if(plot | spatial) ranges <- list("ranges" = ranges)
  if(plot) ranges <- append(ranges, list("plot" = list(g)))
  if(spatial) {
    ranges <- append(
      ranges, 
      list("spatial" = list("iao_sf" = iao[["iao_sf"]],
                            "eoo_sf" = eoo[["eoo_sf"]])))
  }
    
  ranges
}


cosewic_iao <- function(df_sf, cell_size, record_id) {
  
  cell_big <- cell_size * 10
  
  grid_lg <- df_sf %>% 
    sf::st_buffer(cell_size/4) %>%
    sf::st_make_grid(cellsize = cell_big) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(grid_lg_id = 1:dplyr::n())
  
  grid <- sf::st_filter(grid_lg, df_sf) %>%
    split(.$grid_lg_id) %>%
    purrr::map(~sf::st_make_grid(.x, cellsize = cell_size) %>% 
                 sf::st_as_sf()) %>%
    dplyr::bind_rows(.id = "grid_lg_id") %>%
    dplyr::mutate(grid_id = 1:dplyr::n()) 

  iao_sf <- grid %>%
    sf::st_join(df_sf) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data$grid_lg_id, .data$grid_id) %>%
    dplyr::summarize(n_records = sum(!is.na(.data[[record_id]])), .groups = "drop") %>%
    dplyr::ungroup()
  
  if(sum(iao_sf$n_records) != nrow(df_sf)) {
    stop("Records incorrectly assigned to grids", call.= FALSE)
  }
    
  iao <- iao_sf %>%
    dplyr::filter(.data$n_records > 0) %>%
    dplyr::summarize(min_record = min(n_records),
                     max_record = max(n_records),
                     median_record = median(n_records),
                     grid_size_km = .env$cell_size,
                     n_occupied = dplyr::n(), 
                     iao = n_occupied * .env$cell_size^2)
  
  iao_sf <- dplyr::right_join(grid, iao_sf, by = c("grid_lg_id", "grid_id"))
  
  list("iao" = iao,
       "iao_sf" = iao_sf)
}

cosewic_eoo <- function(df_sf, p) {

  center <- df_sf %>%
    dplyr::summarize() %>%
    sf::st_centroid() %>%
    sf::st_as_sfc()
  
  eoo_sf <- df_sf %>%
    dplyr::mutate(dist = sf::st_distance(.data$geometry, .env$center)[, 1]) %>%
    dplyr::filter(dist <= quantile(.data$dist, .env$p)) %>%
    sf::st_cast(to = "POINT") %>%  
    sf::st_union() %>%
    sf::st_convex_hull() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(eoo = sf::st_area(.),
                  eoo = units::set_units(.data$eoo, "km^2"))
  
  list("eoo" = dplyr::pull(eoo_sf, eoo),
       "eoo_sf" = eoo_sf)
}


prep_spatial <- function(df, 
                         coords = c("longitude", "latitude"), 
                         extra = "record_id") {
  df %>%
    dplyr::select(dplyr::all_of(c(.env$extra, .env$coords))) %>%
    sf::st_as_sf(coords = coords, crs = 4326) %>%
    sf::st_transform(3347)
}



