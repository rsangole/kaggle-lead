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

ds <- open_dataset("data/arrow-stratifiedsampling")
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
        label %in% c("train", "holdout"),
        meter_reading_missing == FALSE
    ) |>
    dplyr::collect() |>
    as.data.table()
dat[, .N, site_id]
dat[, .N, primary_use]
dat[, .N, anomaly]
dat[, .N, building_id]

dat <- dat[complete.cases(dat)]

dat[, building_id := as.factor(building_id)]
dat[, label := as.factor(label)]
dat[, primary_use := as.factor(primary_use)]
dat[, site_id := as.factor(site_id)]

# Task
task <- as_task_classif(dat,
    target = "anomaly",
    positive = "A1"
)
task$select(setdiff(task$feature_names, "label")) # dropping 'Label'
task

# Base Leaners
lrn_rf <- lrn(
    "classif.ranger",
    num.trees = 50,
    predict_sets = c("train", "test"),
    predict_type = "prob"
)
set_threads(lrn_rf)
lrn_xgb <- lrn(
    "classif.xgboost",
    predict_sets = c("train", "test"),
    predict_type = "prob"
)
set_threads(lrn_xgb)

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

# * Target-Encoding
po_tgtencode <- po(
    "encodeimpact",
    affect_columns = selector_type("factor")
)
# po_tgtencode$train(list(task))[[1]]$data()

# * oversample minority class
po_over <- po("classbalancing",
    id = "oversample",
    adjust = "minor",
    reference = "minor",
    shuffle = FALSE,
    ratio = 10
)

# * pipeline learners

# po_pca_temperature %>>%
#     po_encode %>>%
#     po_over %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_rf) |>
#     as_learner() -> lrn_pca_encode_over_rf

# lrn_pca_encode_over_rf$train(task)

# po_pca_temperature %>>%
#     po_encode %>>%
#     po_over %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_xgb) |>
#     as_learner() -> lrn_pca_encode_over_xgb
# lrn_pca_encode_over_xgb$train(task)

po_tgtencode %>>%
    po_over %>>%
    po("imputeoor") %>>%
    po("fixfactors") %>>%
    po("imputesample") %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_tgtencode_over_rf
lrn_tgtencode_over_rf$predict_sets <- c("train", "test")
lrn_tgtencode_over_rf$ train(task)

qs::qsave(lrn_tgtencode_over_rf, "data/results/stratified-sampling-approach/lrn_tgtencode_over_rf.qs")