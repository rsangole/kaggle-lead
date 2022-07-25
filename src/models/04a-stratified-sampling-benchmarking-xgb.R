library(mlr3verse)
library(mlr3extralearners)
library(mlr3pipelines)
# install_catboost()
# install_learners('classif.gamboost')
library(data.table)
library(arrow)
library(plotly)
library(lattice)
# box::use(. / src / plotters)

logger <- lgr::get_logger("mlr3")
logger$set_threshold("debug")
tf <- "data/results/stratified-sampling-approach/log.json"
logger$add_appender(lgr::AppenderJson$new(tf), name = "json")

set.seed(42)

ds <- open_dataset("data/arrow-stratifiedsampling")

# Tasks

# * building_id and site_id are factors

dat <- ds |>
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
    dplyr::filter(
        label == "train",
        meter_reading_missing == FALSE
    ) |>
    dplyr::collect() |>
    as.data.table()

dat <- dat[complete.cases(dat)]
dat[, cloud_coverage_missing := cloud_coverage == 255]
dat[, year_built_missing := year_built == 255]
dat[, building_id := as.factor(building_id)]
dat[, label := as.factor(label)]
dat[, primary_use := as.factor(primary_use)]
dat[, site_id := as.factor(site_id)]

task <- as_task_classif(
    dat,
    target = "anomaly",
    positive = "A1",
    id = "all_factors"
)
task$select(setdiff(task$feature_names, "label")) # dropping 'Label'
task

# Resampling Strategy
cv3 <- rsmp("cv", folds = 3)
cv3$instantiate(task)
cv3

# Parameters

# Measures
measure <- list(
    msr("classif.fbeta"),
    msr("classif.auc", predict_sets = "train", id = "auc_train"),
    msr("classif.auc", id = "auc_test"),
    msr("classif.ppv"),
    msr("classif.fpr"),
    msr("time_both"),
    msr("classif.bacc")
)
measure

# Base Leaners

lrn_xgb <- lrn(
    "classif.xgboost",
    predict_sets = c("train", "test"),
    predict_type = "prob"
)
set_threads(lrn_xgb)

# Pipelines

# * One Hot Encoding
po_onehot <- po("encode", method = "one-hot")

# * Factor to Numeric
po_to_numeric <- po("colapply", applicator = as.numeric)
po_to_numeric$param_set$values$affect_columns <- selector_type("factor")

# * Building ID to Numeric
po_bid_numeric <- po("colapply", applicator = as.numeric)
po_bid_numeric$param_set$values$affect_columns <- selector_grep("building_id")

# * PCA numeric
po_pca_temperature <- po(
    "pca",
    affect_columns = selector_grep("temp"),
    center = TRUE,
    scale. = TRUE
)

# # * Encode Factors

# * Target-Encoding
po_tgtencode <- po(
    "encodeimpact",
    affect_columns = selector_type("factor")
)

# * undersample majority class
po_under <- po("classbalancing",
    id = "undersample",
    adjust = "major",
    reference = "major",
    shuffle = FALSE,
    ratio = 1 / 50
)

# * oversample minority class
po_over <- po("classbalancing",
    id = "oversample",
    adjust = "minor",
    reference = "minor",
    shuffle = FALSE,
    ratio = 10
)

# * pipeline learners
po_pca_temperature %>>%
    po_onehot %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_pca_ohot_over_xgb
lrn_pca_ohot_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_pca_ohot_over_xgb)

po_pca_temperature %>>%
    po_tgtencode %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_pca_tgtencode_over_xgb
lrn_pca_tgtencode_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_pca_tgtencode_over_xgb)

po_to_numeric %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_num_over_xgb
lrn_num_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_num_over_xgb)

po_pca_temperature %>>%
    po_bid_numeric %>>%
    po_onehot %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_pca_bidnum_ohot_over_xgb
lrn_pca_bidnum_ohot_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_pca_bidnum_ohot_over_xgb)

po_tgtencode %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_tgtencode_over_xgb
lrn_tgtencode_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_tgtencode_over_xgb)

po_pca_temperature %>>%
    po_onehot %>>%
    po_under %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_pca_ohot_under_xgb
lrn_pca_ohot_under_xgb$predict_sets <- c("train", "test")
set_threads(lrn_pca_ohot_under_xgb)

# Benchmarking
design_xgb <- benchmark_grid(
    tasks = task,
    learners = list(
        lrn_pca_encode_over_xgb,
        lrn_pca_tgtencode_over_xgb,
        lrn_num_over_xgb,
        lrn_pca_bidnum_ohot_over_xgb,
        lrn_tgtencode_over_xgb,
        lrn_pca_ohot_under_xgb
    ),
    resamplings = cv3
)
design_xgb
bmr_xgb <- benchmark(design_xgb)
bmr_xgb$aggregate(measure) -> bmr_xgb_perf
bmr_xgb_perf[, learner_id := forcats::fct_reorder(
    learner_id, auc_test
)]
bmr_xgb_perf
dotplot(
    task_id ~ auc_test,
    groups = learner_id,
    # classif.fbeta +
    # classif.ppv +
    # classif.fpr,
    bmr_xgb_perf,
    pch = 23,
    size = 3,
    auto.key = TRUE
)

dotplot(
    task_id ~ classif.bacc,
    groups = learner_id,
    # classif.fbeta +
    # classif.ppv +
    # classif.fpr,
    bmr_xgb_perf,
    pch = 23,
    size = 3,
    auto.key = TRUE
)

qs::qsave(design_xgb, "data/results/stratified-sampling-approach/design_xgb.qs")
qs::qsave(bmr_xgb, "data/results/stratified-sampling-approach/bmr_xgb.qs")
qs::qsave(bmr_xgb_perf, "data/results/stratified-sampling-approach/bmr_xgb_perf.qs")

# # TUNING --

# Autotuner
# # * define parameter search space for each method
# search_space_under <- ps(undersample.ratio = p_dbl(1 / 50, 1))
# search_space_over <- ps(oversample.ratio = p_dbl(1, 10))
#
# learns <- list(
#     auto_tuner(
#         method = "grid_search",
#         learner = learner_under,
#         resampling = inner_cv3,
#         measure = measure,
#         search_space = search_space_under,
#         resolution = 6
#     ),
#     auto_tuner(
#         method = "grid_search",
#         learner = learner_over,
#         resampling = inner_cv3,
#         measure = measure,
#         search_space = search_space_over,
#         resolution = 6
#     )
# )
# learns
#
# outer_holdout <- rsmp("holdout")
# design <- benchmark_grid(
#     tasks = task,
#     learners = learns,
#     resamplings = outer_holdout
# )
# print(design)
#
# future::plan("multisession")
# bmr = benchmark(design, store_models = TRUE)
# bmr$aggregate(measure)
# autoplot(bmr, measure = measure)
# library(ggplot2)
# bmr_data_learners <- as.data.table(bmr)$learner
# utune_path <- bmr_data_learners[[1]]$model$tuning_instance$archive$data
# utune_gg <- ggplot(utune_path, aes(x = undersample.ratio, y = classif.fbeta)) +
#     geom_point(size = 3) +
#     geom_line() +
#     ylim(0.7, 1)
# otune_path <- bmr_data_learners[[2]]$model$tuning_instance$archive$data
# otune_gg <- ggplot(otune_path, aes(x = oversample.ratio, y = classif.fbeta)) +
#     geom_point(size = 3) +
#     geom_line() +
#     ylim(0.7, 1)
# cowplot::plot_grid(utune_gg, otune_gg)


bmr1 <- qs::qread("data/results/stratified-sampling-approach/bmr.qs")

bmr_xgb$combine(bmr1)

bmr_xgb
