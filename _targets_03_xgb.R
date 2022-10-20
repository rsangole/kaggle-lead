xgb <- list(
  # Base Learners ----
  tar_target(lrn_xgb,
             {
               lrn_xgb <- lrn(
                 "classif.xgboost",
                 predict_sets = c("train", "test"),
                 predict_type = "prob"
               )
               set_threads(lrn_xgb)
               lrn_xgb
             }),
  # Pipeline learners ----
  tar_target(
    lrn_pca_ohot_over_xgb,
    {
      lrn <- po_pca_temperature %>>%
        po_onehot %>>%
        po_over %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_tgtencode_over_xgb,
    {
      lrn <- po_pca_temperature %>>%
        po_tgtencode %>>%
        po_over %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_num_over_xgb,
    {
      lrn <- po_to_numeric %>>%
        po_over %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_bidnum_ohot_over_xgb,
    {
      lrn <- po_pca_temperature %>>%
        po_bid_numeric %>>%
        po_onehot %>>%
        po_over %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_tgtencode_over_xgb,
    {
      lrn <- po_tgtencode %>>%
        po_over %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_ohot_under_xgb,
    {
      lrn <- po_pca_temperature %>>%
        po_onehot %>>%
        po_under %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  # Benchmarking ----
  tar_target(
    design_xgb,
    benchmark_grid(
      tasks = task_all_factors,
      learners = list(
        lrn_pca_tgtencode_over_xgb,
        lrn_num_over_xgb,
        lrn_pca_bidnum_ohot_over_xgb,
        lrn_tgtencode_over_xgb,
        lrn_pca_ohot_under_xgb
      ),
      resamplings = cv3
    )
  ),
  tar_target(
    bmr_xgb,
    benchmark(design_xgb)
  ),
  tar_target(
    bmr_xgb_perf,
    {
      x <- bmr_xgb$aggregate(measure)
      x[, learner_id := forcats::fct_reorder(
        learner_id, auc_test
      )]
      x
    }
  ),
  # Post Processing ----
  tar_target(
    xgb_dotplots,
    dotplot(
      task_id ~ auc_test,
      groups = learner_id,
      bmr_xgb_perf,
      pch = 23,
      size = 3,
      auto.key = TRUE
    )
  )
)
