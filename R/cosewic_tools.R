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
#' @param plots Logical. Whether to return plots
#' @param spatial Logical. Whether to return sf spatial objects showing
#'   calculations.
#'
#' @return Data frame or list containing data frame, and (optionally) plots, 
#' and spatial data frames.
#'
#' @examples
#' 
#' r <- cosewic_ranges(bcch)
#' r <- cosewic_ranges(bcch, spatial = FALSE)
#' r <- cosewic_ranges(bcch, spatial = FALSE, plots = FALSE)
#'
#' @export 

cosewic_ranges <- function(df_db, 
                           record_id = "record_id", 
                           coord_lon = "longitude",
                           coord_lat = "latitude",
                           iao_grid_size_km = 2,
                           plots = TRUE,
                           spatial = TRUE) {
  
  # Check units
  if(!requireNamespace("sf", quietly = TRUE)) {
    stop("This function requires the 'sf' package.", 
         "Please install with `install.packages(\"sf\")` first", call. = FALSE)
  } else if(packageVersion("sf") < "1.0-9") {
    stop("This function requires 'sf' version 1.0-9 or higher.",
         "Please update with `install.packages(\"sf\")` first", call. = FALSE)
  }
  
  # Check df_db 
  # SQLite Connections must become dataframes
  if(inherits(df_db, "SQLiteConnection")) {
    
    if(verbose) message(" - Cannot work directly on SQLite database connections, ",
                        "collecting data into a data frame...")
    
    if(!"naturecounts" %in% DBI::dbListTables(df_db)) {
      stop("If 'df_db' is a SQLite database, it must have a 'naturecounts' ",
           "table", call. = FALSE)
    }
    
    df_db <- dplyr::tbl(df_db, "naturecounts") %>%
      dplyr::collect()
  }
  
  # CHECK COORDS
  
  # Note on CRS
  # GeoCAT uses Google's projection EPSG:3857, Web Mercator because of backend
  # Probably should use Canadian projection. Currently using 3347 (Stats 
  # Canada).
  
  df_sf <- df_db %>%
    dplyr::select(dplyr::all_of(c(.env$record_id, .env$coord_lon, .env$coord_lat))) %>%
    sf::st_as_sf(coords = c(coord_lon, coord_lat), crs = 4326)
  
  # Convert to map units
  cell_size <- units::as_units(iao_grid_size_km, "km")
  df_sf_proj <- sf::st_transform(df_sf, 3347)
  
  iao <- cosewic_iao(df_sf_proj, cell_size, record_id)
  eoo <- cosewic_eoo(df_sf_proj)
  
  ranges <- dplyr::mutate(iao[["iao"]], eoo = .env$eoo[["eoo"]])
 
  if(ranges$eoo < ranges$iao) {
   message(
   "EOO is less than IAO. This can occur if there are very few, clustered ",
   "records. Making EOO equal to IAO ",
   "(see 'Instructions for preparing COSEWIC status reports')")
    
    ranges$eoo <- ranges$iao
  }
  
  
  if(plots) {
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
  
  if(plots | spatial) ranges <- list("ranges" = ranges)
  if(plots) ranges <- append(ranges, list("plots" = g))
  if(spatial) {
    ranges <- append(
      ranges, 
      list("spatial" = list("iao_sf" = iao[["iao_sf"]],
                            "eoo_sf" = eoo[["eoo_sf"]])))
  }
    
  ranges
}


cosewic_iao <- function(df_sf_proj, cell_size, record_id) {
  grid <- df_sf_proj %>%
    sf::st_make_grid(cellsize = cell_size) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(grid_id = 1:dplyr::n())
  
  iao_sf <- grid %>%
    sf::st_join(df_sf_proj) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(grid_id) %>%
    dplyr::mutate(n_records = sum(!is.na(.data[[record_id]]))) %>%
    dplyr::ungroup()
    
  iao <- iao_sf %>%
    dplyr::summarize(min_record = min(n_records[n_records > 0]),
                     max_record = max(n_records[n_records > 0]),
                     median_record = median(n_records[n_records > 0]),
                     grid_size_km = .env$cell_size,
                     n_occupied = sum(n_records != 0), 
                     iao = n_occupied * .env$cell_size^2)
  
  iao_sf <- dplyr::right_join(grid, iao_sf, by = "grid_id")
  
  list("iao" = iao,
       "iao_sf" = iao_sf)
}


cosewic_eoo <- function(df_sf_proj) {
  eoo_sf <- df_sf_proj %>%
    sf::st_union() %>%
    sf::st_convex_hull() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(eoo = sf::st_area(.),
                  eoo = units::set_units(.data$eoo, "km^2"))
  
  list("eoo" = dplyr::pull(eoo_sf, eoo),
       "eoo_sf" = eoo_sf)
}




