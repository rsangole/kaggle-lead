library(arrow)
library(data.table)

# Read the test dataset
test_dat <- open_dataset("data/arrow-stratifiedsampling") |>
    dplyr::filter(label == "test") |>
    dplyr::select(-anomaly) |>
    dplyr::collect() |>
    as.data.table()
test_dat[]

# Read the model
lrn_pca_encode_over_rf_largemtry <- qs::qread("data/results/stratified-sampling-approach/lrn_pca_encode_over_rf_largemtry.qs")

# Handle the missing values case
results_missing_meter_reading <- test_dat[meter_reading_missing == TRUE, "row_id"]
results_missing_meter_reading[, anomaly := 0]
results_missing_meter_reading

test_dat_to_predict <- test_dat[meter_reading_missing == FALSE]
test_dat_to_predict

# Predict non missing values
test_predictions1 <- lrn_pca_encode_over_rf_largemtry$predict_newdata(test_dat_to_predict[1:1e6,])
test_predictions2 <- lrn_pca_encode_over_rf_largemtry$predict_newdata(test_dat_to_predict[(1e6+1):.N,])
pred_dat <- rbindlist(
 list(

   as.data.table(test_predictions1)[, .(anomaly = prob.A1)],
    as.data.table(test_predictions2)[, .(anomaly = prob.A1)][, row_ids := row_ids + 1e6]
 )
)

# pred_dat[, anomaly := as.numeric(substr(anomaly,2,3))]
pred_dat <- cbind(test_dat_to_predict[, "row_id"], pred_dat)
pred_dat[, .N, anomaly]

# Combine
prediction <- rbindlist(list(pred_dat, results_missing_meter_reading))
prediction[, .N, anomaly]

fwrite(prediction, "data/results/stratified-sampling-approach/lrn_pca_encode_over_rf_largemtry-probs.csv")
