data_preprocessor <- list(
        tar_target(
                inputfile_train_features,
                "data/data-raw/train_features.csv",
                format = "file"
        ),
        tar_target(
                train_features,
                read_csv_arrow(inputfile_train_features) |>
                        as.data.table(),
                format = "fst_dt"
        ),
        tar_target(
                cleaned_train_features,
                train_features |>
                        feat_cleanup_primary_use() |>
                        feat_cleanup_categoricals() |>
                        feat_train_test_split(HOLDOUT_SET_PERCENTAGE) |> 
                        feat_replaceNA_add_lags(),
                format = "fst_dt"
        ),
        tar_target(
                inputfile_test_features,
                "data/data-raw/test_features.csv",
                format = "file"
        ),
        tar_target(
                test_features,
                read_csv_arrow(inputfile_test_features) |>
                        as.data.table(),
                format = "fst_dt"
        ),
        tar_target(
                cleaned_test_features,
                test_features |>
                        feat_cleanup_primary_use() |>
                        feat_cleanup_categoricals() |>
                        setkey(timestamp) |> 
                        feat_replaceNA_add_lags() |> 
                        dplyr::mutate(label = "test") |> 
                        dplyr::group_by(label, primary_use, site_id, building_id),
                format = "parquet"
        ),
        tar_target(
                outfile_train_test_features,
                {
                        path = here::here("data/arrow-stratifiedsampling/")
                        cleaned_train_features |>
                                bind_rows(cleaned_test_features) |> 
                                dplyr::group_by(label, 
                                                primary_use, 
                                                site_id, 
                                                building_id) |> 
                                write_dataset(path = path, format = "parquet")
                        path
                }
        )
)
