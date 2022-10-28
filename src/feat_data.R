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
feat_add_lags <- function(dat,
                          moving_avg_periods = c(24, 24 * 3),
                          lags = c(1, 24, 24*7),
                          diffs = c(1, 2)) {
  
    cli::cli_inform("Setting key to {.code timestamp}")
    setkey(dat, timestamp)
    
    cli::cli_inform("Adding {.code lead}, {.code lag}, {.code diff}, and {.code sliding-mean} values")
    
    out <- list()
    for (.bldg in unique(dat$building_id)) {
      x <- dat |>
        dplyr::as_tibble() |> 
        dplyr::filter(building_id == .bldg) |>
        timetk::tk_augment_slidify(
          .value = meter_reading,
          .period = moving_avg_periods,
          .f = mean,
          .partial = TRUE
        ) |>
        timetk::tk_augment_lags(.value = "meter_reading",
                                .lags = lags) |>
        timetk::tk_augment_leads(.value = "meter_reading",
                                 .lags = lags) |>
        timetk::tk_augment_differences(.value = "meter_reading",
                                       .lags = 1,
                                       .differences = diffs) |>
        data.table::as.data.table()
      
      y <- x[,.SD,.SDcols = patterns("meter_reading_")]
      y <- as.list(y[, lapply(.SD, mean, na.rm = TRUE)])
      
      x[is.na(meter_reading_lag1), meter_reading_lag1 := y$meter_reading_lag1]
      x[is.na(meter_reading_lag24), meter_reading_lag24 := y$meter_reading_lag24]
      x[is.na(meter_reading_lag168), meter_reading_lag168 := y$meter_reading_lag168]      
      x[is.na(meter_reading_lag1_diff1), meter_reading_lag1_diff1 := 0]
      x[is.na(meter_reading_lag1_diff2), meter_reading_lag1_diff2 := 0]
      
      out[[.bldg]] <- x
      
    }
    
    data.table::rbindlist(out, use.names = TRUE)
    
}

#' @export
calc_meter_reading_impute <- function(dat) {
  cli::cli_inform("Calculating impute values")
  dat[, .(impute_vals = median(meter_reading, na.rm = TRUE)), .(site_id)]
  
}

#' @export
feat_impute_missing_meter_reading <- function(dat, impute_vals) {
  cli::cli_inform("Imputing missing meter-reading")
  dat <-
    data.table::merge.data.table(dat, impute_vals, by = c("site_id"))
  dat[is.na(meter_reading), meter_reading := impute_vals]
  dat[, impute_vals := NULL]
  dat
}

#' @export
calc_trimmed_sd_trainset <- function(dat) {
  cli::cli_inform("Calculating trimmed std dev")
  dat[, q5 := quantile(meter_reading, probs = 0.05, na.rm = TRUE), 
      .(site_id)]
  dat[, q95 := quantile(meter_reading, probs = 0.95, na.rm = TRUE), 
      .(site_id)]
  
  dat[meter_reading > q5 &
        meter_reading < q95, 
      .(trimmed_sd_meter_reading = sd(meter_reading, na.rm = TRUE)), 
      .(site_id)]
}

#' @export
feat_add_trimmed_sd <- function(dat, trimmed_sd_dt) {
  cli::cli_inform("Adding {.code trimmed std dev}")
  dat <- data.table::merge.data.table(
    dat,
    trimmed_sd_dt,
    by = c("site_id")
  )
  dat
}

#' @export
feat_cleanup_primary_use <- function(dat) {
  dat$primary_use <- as.factor(dat$primary_use)
  levels(dat$primary_use) <- janitor::make_clean_names(levels(dat$primary_use))
  dat$primary_use <- as.character(dat$primary_use)
  dat
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

#' @export
feat_cleanup_vars <- function(dat){
  dat[, cloud_coverage_missing := as.numeric(cloud_coverage == 255)]
  dat[, year_built_missing := as.numeric(year_built == 255)]
  # dat[, building_id := as.factor(building_id)]
  dat[, label := as.factor(label)]
  dat[, primary_use := as.factor(primary_use)]
  dat[, site_id := as.factor(site_id)]
  dat
}

#' @export
drop_vars <- function(dat){
  dat |> 
    dplyr::select(
      # zero variance vars
      -year, 
      -gte_meter,
      # other vars
      -building_weekday_hour,
      -building_weekday,
      -building_month,
      -building_hour,
      -building_meter,
      -weekday_hour,
      # high corr vars
      -contains("_temperature_min"),
      -contains("_temperature_max")
    )
}
