commmons <- list(
  # Data Sets ----
  tar_target(dat,
             {
               x <- open_dataset(outfile_train_test_features) |>
                 dplyr::select(
                   -"building_hour",
                   -"building_meter",
                   -"building_month",
                   -"building_weekday",
                   -"building_weekday_hour",
                   -"weekday_hour",
                   -"row_id",
                   -"timestamp"
                 ) |>
                 dplyr::filter(label == "train",
                               meter_reading_missing == FALSE) |>
                 dplyr::collect() |>
                 as.data.table()
               x <- x[complete.cases(x)]
               x[, cloud_coverage_missing := cloud_coverage == 255]
               x[, year_built_missing := year_built == 255]
               x[, building_id := as.factor(building_id)]
               x[, label := as.factor(label)]
               x[, primary_use := as.factor(primary_use)]
               x[, site_id := as.factor(site_id)]
               x
             },
             format = "fst_dt"),
  
  tar_target(
    dat_bid_numeric,
    dat |>
      lazy_dt() |>
      mutate(building_id = as.numeric(gsub(
        "Bld-", "", building_id
      )),
      site_id = as.numeric(gsub("Site-", "", site_id))) |>
      collect(),
    format = "fst_dt"
  ),
  
  # Tasks ----
  tar_target(task_all_factors,
             {
               tsk <- as_task_classif(dat,
                                       target = "anomaly",
                                       positive = "A1",
                                       id = "all_factors")
               tsk$select(setdiff(tsk$feature_names, "label")) # dropping 'Label'
               tsk
             }),
  tar_target(task_bid_numeric,
             {
               tsk <- as_task_classif(dat_bid_numeric,
                                       target = "anomaly",
                                       positive = "A1",
                                       id = "bid_numeric")
               tsk$select(setdiff(tsk$feature_names, "label")) # dropping 'Label'
               tsk
             }),
  # * building_id removed, site_id as numeric
  tar_target(task_no_bid,
             {
               tsk <- as_task_classif(dat_bid_numeric,
                                       target = "anomaly",
                                       positive = "A1",
                                       id = "no_bid")
               tsk$select(setdiff(tsk$feature_names, c("label", "building_id")))
               tsk
             }),
  # * building_id removed, site_id as categorical
  tar_target(task_no_bid_site_cat,
             {
               tsk <- as_task_classif(dat,
                                      target = "anomaly",
                                      positive = "A1",
                                      id = "no_bid_site_cat")
               tsk$select(setdiff(tsk$feature_names, c("label", "building_id")))
               tsk
             }),
  # Resampling ----
  tar_target(cv3,
             {
               cv3 <- rsmp("cv", folds = 3)
               cv3$instantiate(task_all_factors)
             }),
  # Measures ----
  tar_target(measure,
             list(
               msr("classif.fbeta"),
               msr("classif.auc", 
                   predict_sets = "train", 
                   id = "auc_train"),
               msr("classif.auc", id = "auc_test"),
               msr("classif.ppv"),
               msr("classif.fpr"),
               msr("time_both"),
               msr("classif.bacc")
             )),
  # Pipeline Commons ----

  # * Encode Factors
  tar_target(po_encode,
             {
               po_encode <- po("encode",
                               method = "treatment",
                               affect_columns = selector_type("factor"))
               po_encode$train(list(task_all_factors))
               po_encode
             }), 
  
  # * One Hot Encoding
  tar_target(po_onehot,
             po("encode", method = "one-hot")),
  
  # * Factor to Numeric
  tar_target(po_to_numeric,
             {
               po_to_numeric <- po("colapply", applicator = as.numeric)
               po_to_numeric$param_set$values$affect_columns <-
                 selector_type("factor")
               po_to_numeric
             }),
  
  # * Building ID to Numeric
  tar_target(po_bid_numeric, {
    po_bid_numeric <- po("colapply", applicator = as.numeric)
    po_bid_numeric$param_set$values$affect_columns <-
      selector_grep("building_id")
    po_bid_numeric
  }),
  
  # * PCA numeric
  tar_target(
    po_pca_temperature,
    po(
      "pca",
      affect_columns = selector_grep("temp"),
      center = TRUE,
      scale. = TRUE
    )
  ),
  # * Target-Encoding
  tar_target(
    po_tgtencode,
    po("encodeimpact",
       affect_columns = selector_type("factor"))
  ),
  # * undersample majority class
  tar_target(
    po_under,
    po(
      "classbalancing",
      id = "undersample",
      adjust = "major",
      reference = "major",
      shuffle = FALSE,
      ratio = 1 / 50
    )
  ),
  # * oversample minority class
  tar_target(
    po_over,
    po(
      "classbalancing",
      id = "oversample",
      adjust = "minor",
      reference = "minor",
      shuffle = FALSE,
      ratio = 10
    )
  )
)
