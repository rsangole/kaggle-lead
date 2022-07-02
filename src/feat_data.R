
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
                        .f = mean,
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
                dplyr::select(-meter_reading_missing) |>
                dplyr::ungroup() |>
                data.table::as.data.table()
}

#' @export
feat_replaceNA_add_lags <- function(dat,
                                    moving_avg_periods = c(24, 24 * 3),
                                    lags = c(1, 14),
                                    diffs = c(1, 2)) {
        dat |>
                dplyr::group_by(building_id, site_id) |>
                timetk::tk_augment_slidify(
                        .value = meter_reading,
                        .period = moving_avg_periods,
                        # align = "right",
                        .f = mean,
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
                dplyr::mutate(
                        meter_reading_missing = is.na(meter_reading),
                        meter_reading = dplyr::case_when(
                                meter_reading_missing == TRUE ~ -99,
                                TRUE ~ meter_reading
                        )
                ) |>
                dplyr::ungroup() |>
                data.table::as.data.table()
}

#' @export
feat_cleanup_primary_use <- function(dat) {
        dat$primary_use <- as.factor(dat$primary_use)
        levels(dat$primary_use) <- janitor::make_clean_names(levels(dat$primary_use))
        dat$primary_use <- as.character(dat$primary_use)
        dat
}

#' @export
feat_cleanup_categoricals <- function(dat) {
        dat[, building_id := as.factor(sprintf("Bld-%d", building_id))]
        dat[, site_id := as.factor(sprintf("Site-%d", site_id))]
        if ("anomaly" %in% names(dat)) {
                dat[, anomaly := as.factor(sprintf("A%d", anomaly))]
        }
        dat
}
