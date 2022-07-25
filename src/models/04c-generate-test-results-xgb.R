library(arrow)
library(mlr3)
library(data.table)

# Read the test dataset
test_dat <- open_dataset("data/arrow-stratifiedsampling") |>
  dplyr::filter(label == "test") |>
  dplyr::select(-anomaly) |>
  dplyr::collect() |>
  as.data.table()
test_dat[]


models <- fs::dir_ls("/home/data/results/stratified-sampling-approach/xgb/", glob = "*.qs")
models

make_predictions <- function(model_file, test_dat_to_predict) {
  model <- qs::qread(model_file)
  preds <- model$predict_newdata(test_dat_to_predict)
  pred_dat <- as.data.table(preds)[, .(anomaly = prob.A1)]
  pred_dat <- cbind(test_dat_to_predict[, "row_id"], pred_dat)
  pred_dat
}

predict_and_save <- function(model_file,
                             results_missing_meter_reading,
                             test_dat_to_predict,
                             outglob = "",
                             outpath = "data/results/stratified-sampling-approach/xgb/test_predictions") {
  outname <- fs::path_file(model_file) |> fs::path_ext_remove()
  pred_dat <- make_predictions(model_file, test_dat_to_predict)
  # Combine
  prediction <- rbindlist(list(pred_dat, results_missing_meter_reading))
  fwrite(
    prediction,
    fs::path(outpath, paste0(outglob, outname), ext = "csv")
  )
}

# Handle the missing values case
results_missing_meter_reading <- test_dat[meter_reading_missing == TRUE, "row_id"]
results_missing_meter_reading[, anomaly := 0]
results_missing_meter_reading

test_dat_to_predict <- test_dat[meter_reading_missing == FALSE]
test_dat_to_predict

# Predict non missing values
lapply(
  models,
  predict_and_save,
  results_missing_meter_reading = results_missing_meter_reading,
  test_dat_to_predict = test_dat_to_predict
)
