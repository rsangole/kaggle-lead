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
# test_predictions...

# Combine
# prediction <- rbindlist(list(test_predictions, results_missing_meter_reading))
#
