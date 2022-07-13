library(arrow)
library(data.table)

# Read the test dataset
test_dat <- open_dataset("data/arrow-stratifiedsampling") |>
    dplyr::filter(label == "test") |>
    dplyr::select(-anomaly) |>
    dplyr::collect() |>
    as.data.table()
test_dat[]

# Handle the missing values case
results_missing_meter_reading <- test_dat[meter_reading_missing == TRUE, "row_id"]
results_missing_meter_reading[, anomaly := 0]
results_missing_meter_reading

test_dat_to_predict <- test_dat[meter_reading_missing == FALSE]
test_dat_to_predict

# Predict non missing values
test_predictions <- lrn_pca_encode_over_lightgbm$predict_newdata(test_dat_to_predict)
pred_dat <- as.data.table(test_predictions)[, .(anomaly = prob.A1)]
# pred_dat[, anomaly := as.numeric(substr(anomaly,2,3))]
pred_dat <- cbind(test_dat_to_predict[, "row_id"], pred_dat)
pred_dat[, .N, anomaly]

# Combine
prediction <- rbindlist(list(pred_dat, results_missing_meter_reading))
prediction[, .N, anomaly]

fwrite(prediction, "data/results/stratified-sampling-approach/lrn_pca_encode_over_lightgbm-probs.csv")
