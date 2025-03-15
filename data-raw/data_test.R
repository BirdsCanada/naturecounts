# Testing Data ------------------------------------------------------------
test_rc <- nc_data_dl(request_id = 152518, fields_set = "core",
                      username = "sample", info = "sample_data") |>
  dplyr::filter(CommonName %in% c("Monarch",
                                  "Black Swallowtail",
                                  "Red Admiral"),
                AllSpeciesReported == "Yes") |>
  dplyr::mutate(presence = as.numeric(ObservationCount > 0)) |>
  format_dates()

saveRDS(test_rc, file.path(system.file("extdata", package = "naturecounts"), "test_data.rds"))
