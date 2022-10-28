commmons <- list(
  # Tasks ----
  tar_target(task_all_factors,
             {
               tsk <- as_task_classif(cleaned_train_features,
                                       target = "anomaly",
                                       positive = "A1",
                                       id = "all_factors")
               tsk$set_col_roles(
                 c("label",
                   "row_id",
                   "timestamp",
                   "building_id"),
                 remove_from = "feature"
               )
               tsk
             }),
  
  # Resampling ----
  tar_target(cv3, rsmp("cv", folds = 3)),
  tar_target(bootstrap, rsmp("bootstrap",
                             repeats = 5)),
  
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
  
  # Pipeline Operators ----
  # * Encode Factors
  tar_target(po_encode,
             po("encode",
                method = "treatment",
                affect_columns = selector_type("factor"))
             ),
             # {
             #   po_encode <- po("encode",
             #                   method = "treatment",
             #                   affect_columns = selector_type("factor"))
             #   po_encode$train(list(task_all_factors))
             #   po_encode
             # }), 
  # * One Hot Encoding
  tar_target(po_onehot,
             po("encode", 
                id = "one_hot",
                method = "one-hot")),
  
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
    po_under_10pc_A0,
    po(
      "classbalancing",
      id = "undersample_10pc_A0",
      adjust = "major",
      reference = "major",
      ratio = 0.1 # keep 10% of A0 class
    )
  ),
  
  # * undersample majority class
  tar_target(
    po_under_20pc_A0,
    po(
      "classbalancing",
      id = "undersample_20pc_A0",
      adjust = "major",
      reference = "major",
      ratio = 0.2
    )
  ),
  
  # * oversample minority class
  tar_target(
    po_over_10x_A1,
    po(
      "classbalancing",
      id = "oversample_10x_A1",
      adjust = "minor",
      reference = "minor",
      ratio = 10
    )
  ),
  
  # * oversample minority class
  tar_target(
    po_over_50x_A1,
    po(
      "classbalancing",
      id = "oversample_50x_A1",
      adjust = "minor",
      reference = "minor",
      ratio = 50
    )
  ),
  
  # * balanced sampling
  tar_target(
    po_over_balance,
    po(
      "classbalancing",
      id = "balanced_sampling",
      adjust = "all",
      reference = "all",
      ratio = 1
    )
  ),
  
  # Pipelines ----
  tar_target(
    pipe_pca_ohot_over_10x_A1,
    po_pca_temperature %>>%
      po_onehot %>>%
      po_over_10x_A1
  ),
  tar_target(
    pipe_pca_ohot_over_50x_A1,
    po_pca_temperature %>>%
      po_onehot %>>%
      po_over_50x_A1
  ),
  tar_target(
    pipe_pca_ohot_balanced,
    po_pca_temperature %>>%
      po_onehot %>>%
      po_over_balance
  ),
  tar_target(
    pipe_pca_ohot_under_20pc_A0,
    po_pca_temperature %>>%
      po_onehot %>>%
      po_under_20pc_A0
  ),
  tar_target(
    pipe_pca_ohot_under_10pc_A0,
    po_pca_temperature %>>%
      po_onehot %>>%
      po_under_10pc_A0
  ),
  tar_target(
    pipe_pca_tgtencode_balanced,
    po_pca_temperature %>>%
      po_tgtencode %>>%
      po_over_balance
  )
)


