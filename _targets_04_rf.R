rf <- list(
  # Base Learners ----
  tar_target(lrn_rf,
             {
               lrn_rf <- lrn(
                 "classif.ranger",
                 num.trees = 100,
                 predict_sets = c("train", "test"),
                 predict_type = "prob"
               )
               set_threads(lrn_rf)
               lrn_rf
             }), 
  # Pipeline learners ----
  tar_target(
    lrn_pca_encode_over_rf,
    {
      lrn <- po_pca_temperature %>>%
        po_encode %>>%
        po_over %>>%
        po("imputeoor") %>>%
        po("fixfactors") %>>%
        po("imputesample") %>>%
        po(lrn_rf) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_tgtencode_over_rf,
    {
      lrn <- po_pca_temperature %>>%
        po_tgtencode %>>%
        po_over %>>%
        po("imputeoor") %>>%
        po("fixfactors") %>>%
        po("imputesample") %>>%
        po(lrn_rf) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_encode_over_rf,
    {
      lrn <- po_encode %>>%
        po_over %>>%
        po("imputeoor") %>>%
        po("fixfactors") %>>%
        po("imputesample") %>>%
        po(lrn_rf) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_tgtencode_over_rf,
    {
      lrn <- po_tgtencode %>>%
        po_over %>>%
        po("imputeoor") %>>%
        po("fixfactors") %>>%
        po("imputesample") %>>%
        po(lrn_rf) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  # Benchmarking ----
  tar_target(
    design_rf,
    benchmark_grid(
      tasks = list(
        task_all_factors,
        task_bid_numeric,
        task_no_bid,
        task_no_bid_site_cat
      ),
      learners = list(
        lrn_encode_over_rf,
        lrn_tgtencode_over_rf
      ),
      resamplings = cv3
    )
  ),
  tar_target(
    bmr_rf,
    benchmark(design_rf)
  ),
  tar_target(
    bmr_rf_perf,
    {
      x <- bmr_rf$aggregate(measure)
      x[, learner_id := forcats::fct_reorder(learner_id, auc_test)]
      x
    }
  ),
  # Post Processing ----
  tar_target(
    rf_dotplots,
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
