catboost <- list(
  # Tasks ----
  tar_target(task_all_factors_catboost,
             {
               tsk <- as_task_classif(feat_make_numeric_features_for_catboost(dat),
                                      target = "anomaly",
                                      positive = "A1",
                                      id = "all_factors")
               tsk$select(setdiff(tsk$feature_names, "label")) # dropping 'Label'
               tsk
             }),
  tar_target(task_bid_numeric_catboost,
             {
               tsk <- as_task_classif(feat_make_numeric_features_for_catboost(dat_bid_numeric),
                                      target = "anomaly",
                                      positive = "A1",
                                      id = "bid_numeric")
               tsk$select(setdiff(tsk$feature_names, "label")) # dropping 'Label'
               tsk
             }),
  # * building_id removed, site_id as numeric
  tar_target(task_no_bid_catboost,
             {
               tsk <- as_task_classif(feat_make_numeric_features_for_catboost(dat_bid_numeric),
                                      target = "anomaly",
                                      positive = "A1",
                                      id = "no_bid")
               tsk$select(setdiff(tsk$feature_names, c("label", "building_id")))
               tsk
             }),
  # * building_id removed, site_id as categorical
  tar_target(task_no_bid_site_cat_catboost,
             {
               tsk <- as_task_classif(feat_make_numeric_features_for_catboost(dat),
                                      target = "anomaly",
                                      positive = "A1",
                                      id = "no_bid_site_cat")
               tsk$select(setdiff(tsk$feature_names, c("label", "building_id")))
               tsk
             }),
  # Base Learners ----
  tar_target(lrn_catboost,
             {
               lrn <- lrn(
                 "classif.catboost",
                 predict_sets = c("train", "test"),
                 predict_type = "prob"
               )
               set_threads(lrn)
               lrn$param_set$values <- list(task_type = "CPU")
               lrn
             }), 
  # Pipeline learners ----
  tar_target(
    lrn_pca_encode_over_catboost,
    {
      lrn <- po_pca_temperature %>>%
        po_encode %>>%
        po_over %>>%
        # po("imputeoor") %>>%
        # po("fixfactors") %>>%
        # po("imputesample") %>>%
        po(lrn_catboost) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_tgtencode_over_catboost,
    {
      lrn <- po_pca_temperature %>>%
        po_tgtencode %>>%
        po_over %>>%
        # po("imputeoor") %>>%
        # po("fixfactors") %>>%
        # po("imputesample") %>>%
        po(lrn_catboost) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_encode_over_catboost,
    {
      lrn <- po_encode %>>%
        po_over %>>%
        # po("imputeoor") %>>%
        # po("fixfactors") %>>%
        # po("imputesample") %>>%
        po(lrn_catboost) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_tgtencode_over_catboost,
    {
      lrn <- po_tgtencode %>>%
        po_over %>>%
        # po("imputeoor") %>>%
        # po("fixfactors") %>>%
        # po("imputesample") %>>%
        po(lrn_catboost) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  # Benchmarking ----
  tar_target(
    design_catboost,
    benchmark_grid(
      tasks = list(
        task_all_factors_catboost,
        task_bid_numeric_catboost,
        task_no_bid_catboost,
        task_no_bid_site_cat_catboost
      ),
      learners = list(
        lrn_catboost,
        lrn_pca_tgtencode_over_catboost,
        lrn_tgtencode_over_catboost
      ),
      resamplings = cv3
    )
  ),
  tar_target(
    bmr_catboost,
    benchmark(design_catboost)
  ),
  tar_target(
    bmr_catboost_perf,
    {
      x <- bmr_catboost$aggregate(measure)
      x[, learner_id := forcats::fct_reorder(learner_id, auc_test)]
      x
    }
  ),
  # Post Processing ----
  tar_target(
    catboost_dotplots,
    dotplot(
      task_id ~ auc_test,
      groups = learner_id,
      bmr_rf_perf,
      pch = 23,
      size = 3,
      auto.key = TRUE
    )
  )
)
