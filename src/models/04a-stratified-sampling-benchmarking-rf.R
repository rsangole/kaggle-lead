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
dat[, .N, site_id]
dat[, .N, primary_use]
dat[, .N, anomaly]
dat[, .N, building_id]
# skimr::skim(dat)

dat <- dat[complete.cases(dat)]
# dat[, row_id := 1:.N]
dat[, building_id := as.factor(building_id)]
dat[, label := as.factor(label)]
dat[, primary_use := as.factor(primary_use)]
dat[, site_id := as.factor(site_id)]
dat[, cloud_coverage_missing := cloud_coverage == 255]
dat[, year_built_missing := year_built == 255]

task <- as_task_classif(dat,
    target = "anomaly",
    positive = "A1",
    id = "all_factors"
)
task$select(setdiff(task$feature_names, "label")) # dropping 'Label'
table(task$truth())
task

# * building_id as numeric (similar numbers are closer together)
dat2 <- ds |>
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
dat2[, building_id := as.numeric(gsub("Bld-", "", building_id))]
dat2[, site_id := as.numeric(gsub("Site-", "", site_id))]
dat2[, cloud_coverage_missing := cloud_coverage == 255]
dat2[, year_built_missing := year_built == 255]
dat2 <- dat2[complete.cases(dat2)]

task_bid_numeric <- as_task_classif(dat2,
    target = "anomaly",
    positive = "A1",
    id = "bid_numeric"

)
task_bid_numeric$select(setdiff(task_bid_numeric$feature_names, "label")) # dropping 'Label'
table(task_bid_numeric$truth())
task_bid_numeric

# * building_id removed, site_id as numeric
task_no_bid <- as_task_classif(dat2,
    target = "anomaly",
    positive = "A1",
    id = "no_bid"
)
task_no_bid$select(setdiff(task_no_bid$feature_names, c("label", "building_id"))) # dropping 'Label'
table(task_no_bid$truth())
task_no_bid

# * building_id removed, site_id as categorical
task_no_bid_site_cat <- as_task_classif(dat,
    target = "anomaly",
    positive = "A1",
    id = "no_bid_site_cat"
)
task_no_bid_site_cat$select(setdiff(task_no_bid_site_cat$feature_names, c("label", "building_id"))) # dropping 'Label'
table(task_no_bid_site_cat$truth())
task_no_bid_site_cat

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

lrn_rf <- lrn(
    "classif.ranger",
    num.trees = 100,
    predict_sets = c("train", "test"),
    predict_type = "prob"
)
set_threads(lrn_rf)

# lrn_glmnet <- lrn(
#     "classif.glmnet",
#     predict_sets = c("train", "test"),
#     predict_type = "prob"
# )

# lrn_xgb <- lrn(
#     "classif.xgboost",
#     predict_sets = c("train", "test"),
#     predict_type = "prob"
# )
# set_threads(lrn_xgb)

# lrn_lightgbm <- lrn(
#     "classif.lightgbm",
#     predict_type = "prob",
#     predict_sets = c("train", "test")
# )
# set_threads(lrn_lightgbm)

# Pipelines

# * PCA numeric
po_pca_temperature <- po(
    "pca",
    affect_columns = selector_grep("temp"),
    center = TRUE,
    scale. = TRUE
)

# * Encode Factors
po_encode <- po(
    "encode",
    method = "treatment",
    affect_columns = selector_type("factor")
)
# po_encode$train(list(task))

# * Target-Encoding
po_tgtencode <- po(
    "encodeimpact",
    affect_columns = selector_type("factor")
)
# po_tgtencode$train(list(task))[[1]]$data()

# * undersample majority class
po_under <- po("classbalancing",
    id = "undersample",
    adjust = "major",
    reference = "major",
    shuffle = FALSE,
    ratio = 1 / 50
)
# table(po_under$train(list(task))$output$truth())

# * oversample minority class
po_over <- po("classbalancing",
    id = "oversample",
    adjust = "minor",
    reference = "minor",
    shuffle = FALSE,
    ratio = 10
)
# table(po_over$train(list(task))$output$truth())

# * pipeline learners

po_pca_temperature %>>%
    po_encode %>>%
    po_over %>>%
    po("imputeoor") %>>%
    po("fixfactors") %>>%
    po("imputesample") %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_pca_encode_over_rf
lrn_pca_encode_over_rf$predict_sets <- c("train", "test")

po_pca_temperature %>>%
    po_tgtencode %>>%
    po_over %>>%
    po("imputeoor") %>>%
    po("fixfactors") %>>%
    po("imputesample") %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_pca_tgtencode_over_rf
lrn_pca_tgtencode_over_rf$predict_sets <- c("train", "test")

po_encode %>>%
    po_over %>>%
    po("imputeoor") %>>%
    po("fixfactors") %>>%
    po("imputesample") %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_encode_over_rf
lrn_encode_over_rf$predict_sets <- c("train", "test")

po_tgtencode %>>%
    po_over %>>%
    po("imputeoor") %>>%
    po("fixfactors") %>>%
    po("imputesample") %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_tgtencode_over_rf
lrn_tgtencode_over_rf$predict_sets <- c("train", "test")

# po_pca_temperature %>>%
#     po_encode %>>%
#     po_under %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_rf) |>
#     as_learner() -> lrn_pca_encode_under_rf
# lrn_pca_encode_under_rf$predict_sets <- c("train", "test")

# po_pca_temperature %>>%
#     po_over %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_rf) |>
#     as_learner() -> lrn_pca_over_rf
# lrn_pca_over_rf$predict_sets <- c("train", "test")

# po_over %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_rf) |>
#     as_learner() -> lrn_over_rf
# lrn_over_rf$predict_sets <- c("train", "test")

# po_pca_temperature %>>%
#     po_encode %>>%
#     po_over %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_xgb) |>
#     as_learner() -> lrn_pca_encode_over_xgb
# lrn_pca_encode_over_xgb$predict_sets <- c("train", "test")

# # po_pca_temperature %>>%
# #     po_encode %>>%
# #     po_over %>>%
# #     po(lrn_glmnet) |>
# #     as_learner() -> lrn_pca_encode_over_glmnet

# po_pca_temperature %>>%
#     po_encode %>>%
#     po_over %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_lightgbm) |>
#     as_learner() -> lrn_pca_encode_over_lightgbm
# lrn_pca_encode_over_lightgbm$predict_sets <- c("train", "test")

# Benchmarking
design2 <- benchmark_grid(
    tasks = list(
        task,
        task_bid_numeric,
        task_no_bid,
        task_no_bid_site_cat
    ),
    learners = list(
        # lrn_pca_tgtencode_over_rf,
        # lrn_pca_encode_over_rf
        lrn_encode_over_rf,
        lrn_tgtencode_over_rf
        # lrn_pca_encode_under_rf,
        # lrn_pca_over_rf,
        # lrn_over_rf,
        # lrn_pca_encode_over_xgb
        # lrn_pca_encode_over_lightgbm
        # lrn_pca_encode_over_glmnet
    ),
    resamplings = cv3
)
design2
bmr2 <- benchmark(design2)
bmr2$aggregate(measure) -> bmr_perf2
bmr_perf2[, learner_id := forcats::fct_reorder(
    learner_id, auc_test
)]
bmr_perf2
dotplot(
    task_id ~ auc_test,
    groups = learner_id,
    # classif.fbeta +
    # classif.ppv +
    # classif.fpr,
    bmr_perf2,
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
    bmr_perf2,
    pch = 23,
    size = 3,
    auto.key = TRUE
)

qs::qsave(design2, "data/results/stratified-sampling-approach/design2.qs")
qs::qsave(bmr2, "data/results/stratified-sampling-approach/bmr2.qs")
qs::qsave(bmr_perf2, "data/results/stratified-sampling-approach/bmr_perf2.qs")

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

bmr2$combine(bmr1)

bmr2
