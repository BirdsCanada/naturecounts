#' Calculate COSEWIC AOO and EOO
#' 
#' Area of occupancy (AOO, also called Index of Area of Occupancy, IAO) and
#' Extent of Occurrence (EOO) are metrics used by IUCN and COSEWIC to support
#' status assessments for potentially endangered species.
#'
#' Details on how AOO and EOO are calculated and used
#' 
#' - COSEWIC - [Guidelines for use of the Index of Area of Occupancy in COSEWIC Assessments](https://www.cosewic.ca/index.php/en-ca/reports/preparing-status-reports/guidelines-index-area-occupancy.html)
#' - COSEWIC - [Table 2 COSEWIC quantitative criteria and guidelines for the status assessment of Wildlife Species](https://www.cosewic.ca/index.php/en-ca/assessment-process/wildlife-species-assessment-process-categories-guidelines/quantitative-criteria.html)
#' - IUCN - [Guidelines for Using the IUCN Red List Categories and Criteria Version 15.1](https://nc.iucnredlist.org/redlist/content/attachment_files/RedListGuidelines.pdf)
#' - IUCN - [IUCN Red List categories and criteria, version 3.1, second edition](https://portals.iucn.org/library/sites/library/files/documents/RL-2001-001-2nd.pdf)
#'
#' @param df_db 
#' @param species_id 
#' @param record_id 
#' @param coords 
#' @param aoo_grid_size_km 
#' @param plots 
#'
#' @return
#' @name cosewic_tools
#'
#' @examples
#' 
#' 
#' r <- cosewic_ranges(bcch)
#' r <- cosewic_ranges(bcch, spatial = FALSE)
#' r <- cosewic_ranges(bcch, spatial = FALSE, plots = FALSE)
#'
#' @export 

cosewic_ranges <- function(df_db, 
                           species_id = "species_id",
                           record_id = "record_id", 
                           coords = c("latitude", "longitude"),
                           aoo_grid_size_km = 2,
                           spatial = TRUE,
                           plots = TRUE) {
  
  # Check units
  if(!requireNamespace("sf", quietly = TRUE)) {
    stop("This function requires the 'sf' package.", 
         "Please install with `install.packages(\"sf\")` first", call. = FALSE)
  } else if(packageVersion("sf") < "1.0-9") {
    stop("This function requires 'sf' version 1.0-9 or higher.",
         "Please update with `install.packages(\"sf\")` first", call. = FALSE)
  }
  
  # Note on CRS
  # GeoCAT uses Google's projection EPSG:3857, Web Mercator because of backend
  # Probably should use Canadian projection. Currently using 3347 (Stats 
  # Canada).
  
  df_sf <- df_db %>%
    dplyr::select(dplyr::all_of(c(.env$species_id, .env$record_id, .env$coords))) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Convert to map units
  cell_size <- units::as_units(aoo_grid_size_km, "km")
  df_sf_proj <- sf::st_transform(df_sf, 3347)
  
  aoo <- cosewic_aoo(df_sf_proj, cell_size)
  eoo <- cosewic_eoo(df_sf_proj)
  
  ranges <- dplyr::mutate(aoo[["aoo"]], eoo = .env$eoo[["eoo"]])
 
  if(ranges$eoo < ranges$aoo) {
   message(
   "EOO is less than AOO. This can occur if there are very few, clustered ",
   "records. Making EOO equal to AOO (see Guidelines for Using the IUCN Red ",
   "List Categories and Criteria version 15.1)")
    
    ranges$eoo <- ranges$aoo
  }
  
  
  if(plots) {
    a <- dplyr::mutate(aoo[["aoo_sf"]], 
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
        caption = "Filled cells represent AOO; Black outline represents EOO")
  } else {
    g <- NULL
  } 
  
  if(plots | spatial) ranges <- list("ranges" = ranges)
  if(plots) ranges <- append(ranges, list("plots" = g))
  if(spatial) {
    ranges <- append(
      ranges, 
      list("spatial" = list("aoo_sf" = aoo[["aoo_sf"]],
                            "eoo_sf" = eoo[["eoo_sf"]])))
  }
    
  ranges
}


cosewic_aoo <- function(df_sf_proj, cell_size) {
  grid <- df_sf_proj %>%
    sf::st_make_grid(cellsize = cell_size) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(grid_id = 1:dplyr::n())
  
  aoo_sf <- grid %>%
    sf::st_join(df_sf_proj) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(grid_id) %>%
    dplyr::mutate(n_records = sum(!is.na(record_id))) %>%
    dplyr::ungroup()
    
  aoo <- aoo_sf %>%
    dplyr::summarize(min_record = min(n_records[n_records > 0]),
                     max_record = max(n_records[n_records > 0]),
                     median_record = median(n_records[n_records > 0]),
                     grid_size_km = .env$cell_size,
                     n_occupied = sum(n_records != 0), 
                     aoo = n_occupied * .env$cell_size^2)
  
  aoo_sf <- dplyr::right_join(grid, aoo_sf, by = "grid_id")
  
  list("aoo" = aoo,
       "aoo_sf" = aoo_sf)
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




