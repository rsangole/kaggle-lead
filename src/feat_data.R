#' @export
feat_make_numeric_features_for_catboost <- function(dat){
  x <- dat |> 
    lazy_dt() |> 
    mutate(
      meter_reading_missing = as.factor(meter_reading_missing)
    ) |> 
    collect() |> 
    as.data.table()
  int_to_dbl_cols <- x[, names(.SD), .SDcols = is.numeric]
  x[, (int_to_dbl_cols) := lapply(.SD, as.double), .SDcols = int_to_dbl_cols]
  lgl_to_dbl_cols <- x[, names(.SD), .SDcols = is.logical]
  x[, (lgl_to_dbl_cols) := lapply(.SD, as.numeric), .SDcols = lgl_to_dbl_cols]
  
  x
}

#' @export
feat_train_test_split <- function(dat,
                                  HOLDOUT_SET_PERCENTAGE) {
  setkey(dat, timestamp)
  dat[, row_id := 1:.N]
  row_id_dat <- dat[, row_id, .(anomaly, building_id)]
  holdout_indices <- row_id_dat[, .(row_id = sample(row_id, size = HOLDOUT_SET_PERCENTAGE * nrow(.SD))),
                                .(anomaly, building_id)]
  holdout_indices[, label := "holdout"]
  holdout_indices[, c("anomaly", "building_id") := NULL]
  
  dat <- merge.data.table(x = dat,
                          y = holdout_indices,
                          by = "row_id",
                          all.x = TRUE)
  dat[is.na(label), label := "train"]
  dat
}

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
        dat[, cloud_coverage_missing := cloud_coverage == 255]
        dat[, year_built_missing := year_built == 255]
        if ("anomaly" %in% names(dat)) {
                dat[, anomaly := as.factor(sprintf("A%d", anomaly))]
        }
        dat
}

# pad_by_time(sdat, "timestamp") |>
#         mutate(meter_reading = ts_impute_vec(meter_reading)) |>
#         plot_stl_diagnostics(timestamp, meter_reading, .interactive = FALSE)
