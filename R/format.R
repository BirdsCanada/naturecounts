
nc_format <- function(d) {

  suppressMessages(readr::type_convert(d)) %>%
    dplyr::mutate(date = lubridate::as_date(paste(.data$YearCollected,
                                                  .data$MonthCollected,
                                                  .data$DayCollected)))
}