
nc_format <- function(d) {

  d %>%
    dplyr::mutate(date = lubridate::as_date(paste(.data$YearCollected,
                                                  .data$MonthCollected,
                                                  .data$DayCollected)))

}