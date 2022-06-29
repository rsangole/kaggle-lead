
#' @export
feat_removeNA_add_lags <- function(dat,
                                   moving_avg_periods = c(24, 24 * 3),
                                   lags = c(1, 14),
                                   diffs = c(1, 2)) {
        dat |>
                dplyr::mutate(
                        meter_reading_missing = is.na(meter_reading)
                ) |>
                dplyr::group_by(building_id, site_id) |>
                timetk::tk_augment_slidify(
                        .value = meter_reading,
                        .period = moving_avg_periods,
                        # align = "right",
                        .f = AVERAGE,
                        .partial = TRUE
                ) |>
                timetk::tk_augment_lags(
                        .value = "meter_reading",
                        .lags = lags
                ) |>
                timetk::tk_augment_differences(
                        .value = "meter_reading",
                        .lags = 1,
                        .differences = diffs
                ) |>
                dplyr::filter(!meter_reading_missing) |>
                dplyr::select(-meter_reading_missing)
}
