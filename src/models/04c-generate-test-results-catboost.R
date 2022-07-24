library(arrow)
library(data.table)

# Read the test dataset
test_dat <- open_dataset("data/arrow-stratifiedsampling") |>
  dplyr::filter(label == "test") |>
  dplyr::select(-anomaly) |>
  dplyr::collect() |>
  as.data.table()
test_dat[]
test_dat[, building_id := as.numeric(gsub("Bld-", "", building_id))]
test_dat[, site_id := as.numeric(gsub("Site-", "", site_id))]
test_dat[, cloud_coverage_missing := as.factor(cloud_coverage == 255)]
test_dat[, year_built_missing := as.factor(year_built == 255)]
test_dat[, meter_reading_missing := as.factor(meter_reading_missing)]
test_dat[, primary_use := as.factor(primary_use)]
int_to_dbl_cols <- test_dat[, names(.SD), .SDcols = is.numeric]
test_dat[, (int_to_dbl_cols) := lapply(.SD, as.double), .SDcols = int_to_dbl_cols]

# Read the model
lrn_tgtencode_over_catboost <- qs::qread("data/results/stratified-sampling-approach/lrn_tgtencode_over_catboost.qs")

# Handle the missing values case
results_missing_meter_reading <- test_dat[meter_reading_missing == TRUE, "row_id"]
results_missing_meter_reading[, anomaly := 0]
results_missing_meter_reading

test_dat_to_predict <- test_dat[meter_reading_missing == FALSE]
test_dat_to_predict

# Predict non missing values
test_predictions <- lrn_tgtencode_over_catboost$predict_newdata(test_dat_to_predict)
pred_dat <- as.data.table(test_predictions)[, .(anomaly = prob.A1)]
pred_dat <- cbind(test_dat_to_predict[, "row_id"], pred_dat)
# pred_dat[, .N, anomaly]
pred_dat[]

# Combine
prediction <- rbindlist(list(pred_dat, results_missing_meter_reading))
prediction[, row_id := as.integer(row_id)]
prediction[, .N, anomaly]

fwrite(prediction, "data/results/stratified-sampling-approach/lrn_tgtencode_over_catboost-probs.csv")

lattice::densityplot(~anomaly, prediction, plot.points = FALSE)
