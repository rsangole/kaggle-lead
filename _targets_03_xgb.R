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
    lrn_pca_ohot_over_10x_A1_xgb,
    {
      lrn <- pipe_pca_ohot_over_10x_A1 %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_ohot_over_50x_A1_xgb,
    {
      lrn <- pipe_pca_ohot_over_50x_A1 %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_ohot_balanced_xgb,
    {
      lrn <- pipe_pca_ohot_balanced %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_ohot_under_20pc_A0_xgb,
    {
      lrn <- pipe_pca_ohot_under_20pc_A0 %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_ohot_under_10pc_A0_xgb,
    {
      lrn <- pipe_pca_ohot_under_10pc_A0 %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  tar_target(
    lrn_pca_tgtencode_balanced_xgb,
    {
      lrn <- pipe_pca_tgtencode_balanced %>>%
        po(lrn_xgb) |>
        as_learner()
      lrn$predict_sets <- c("train", "test")
      set_threads(lrn)
      lrn
    }
  ),
  # tar_target(
  #   lrn_num_over_xgb,
  #   {
  #     lrn <- po_to_numeric %>>%
  #       po_over %>>%
  #       po(lrn_xgb) |>
  #       as_learner()
  #     lrn$predict_sets <- c("train", "test")
  #     set_threads(lrn)
  #     lrn
  #   }
  # ),
  # tar_target(
  #   lrn_pca_bidnum_ohot_over_xgb,
  #   {
  #     lrn <- po_pca_temperature %>>%
  #       po_bid_numeric %>>%
  #       po_onehot %>>%
  #       po_over %>>%
  #       po(lrn_xgb) |>
  #       as_learner()
  #     lrn$predict_sets <- c("train", "test")
  #     set_threads(lrn)
  #     lrn
  #   }
  # ),
  # tar_target(
  #   lrn_tgtencode_over_xgb,
  #   {
  #     lrn <- po_tgtencode %>>%
  #       po_over %>>%
  #       po(lrn_xgb) |>
  #       as_learner()
  #     lrn$predict_sets <- c("train", "test")
  #     set_threads(lrn)
  #     lrn
  #   }
  # ),
  # tar_target(
  #   lrn_pca_ohot_under_xgb,
  #   {
  #     lrn <- po_pca_temperature %>>%
  #       po_onehot %>>%
  #       po_under %>>%
  #       po(lrn_xgb) |>
  #       as_learner()
  #     lrn$predict_sets <- c("train", "test")
  #     set_threads(lrn)
  #     lrn
  #   }
  # ),
  # Benchmarking ----
  tar_target(
    learner_list,
    list(
      lrn_pca_ohot_balanced_xgb,
      lrn_pca_ohot_over_10x_A1_xgb,
      lrn_pca_ohot_over_50x_A1_xgb,
      lrn_pca_ohot_under_10pc_A0_xgb,
      lrn_pca_ohot_under_20pc_A0_xgb,
      lrn_pca_tgtencode_balanced_xgb
    )
  ),
  tar_target(
    design_xgb,
    benchmark_grid(
      tasks = task_all_factors,
      learners = learner_list,
      resamplings = cv3 #list(cv3, bootstrap)
    ),
    pattern = map(learner_list)
  ), 
  tar_target(
    bmr_xgb,
    benchmark(design_xgb)
  ),
  tar_target(
    bmr_xgb_perf,
    bmr_xgb$aggregate(measure)
  ),
  
  # Post Processing ----
  tar_target(
    xgb_dotplots,
    plot_benchmark_perf(bmr_xgb_perf)
  )
)
