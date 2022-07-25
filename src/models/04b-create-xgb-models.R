library("mlr3tuning")
library(mlr3verse)
library(mlr3extralearners)
library(mlr3pipelines)
library(data.table)
library(arrow)
library(plotly)
library(lattice)
library(lgr)

logger <- lgr::get_logger("mlr3")
logger$set_threshold("info")

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
dat <- dat[complete.cases(dat)]
dat[, cloud_coverage_missing := cloud_coverage == 255]
dat[, year_built_missing := year_built == 255]
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
lrn_pca_ohot_over_xgb$param_set$values$classif.xgboost.max_depth <- 20

po_pca_temperature %>>%
    po_tgtencode %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_pca_tgtencode_over_xgb
lrn_pca_tgtencode_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_pca_tgtencode_over_xgb)
lrn_pca_tgtencode_over_xgb$param_set$values$classif.xgboost.max_depth <- 20

po_to_numeric %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_num_over_xgb
lrn_num_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_num_over_xgb)
lrn_num_over_xgb$param_set$values$classif.xgboost.max_depth <- 20

po_pca_temperature %>>%
    po_bid_numeric %>>%
    po_onehot %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_pca_bidnum_ohot_over_xgb
lrn_pca_bidnum_ohot_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_pca_bidnum_ohot_over_xgb)
lrn_pca_bidnum_ohot_over_xgb$param_set$values$classif.xgboost.max_depth <- 20

po_tgtencode %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_tgtencode_over_xgb
lrn_tgtencode_over_xgb$predict_sets <- c("train", "test")
set_threads(lrn_tgtencode_over_xgb)
lrn_tgtencode_over_xgb$param_set$values$classif.xgboost.max_depth <- 20

po_pca_temperature %>>%
    po_onehot %>>%
    po_under %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_pca_ohot_under_xgb
lrn_pca_ohot_under_xgb$predict_sets <- c("train", "test")
set_threads(lrn_pca_ohot_under_xgb)
lrn_pca_ohot_under_xgb$param_set$values$classif.xgboost.max_depth <- 20


#  func
train_and_save <- function(tsk,
                           lrnr,
                           outglob,
                           logfile = "xgb-training",
                           outpath = "data/results/stratified-sampling-approach/xgb") {
    fs::dir_create(outpath)
    tf <- fs::path(outpath, logfile, ext = "log")
    lgr$add_appender(AppenderFile$new(tf), name = "file")

    lgr$info("--- New train & save call: %s ---", outglob)
    lgr$info("Training model")
    lrnr$train(tsk)

    lgr$info("Saving learner to qs file")
    qs::qsave(lrnr, fs::path(outpath, paste0(outglob, "-learner"), ext = "qs"))

    lgr$info("Done")
}

train_and_save(task, lrn_pca_bidnum_ohot_over_xgb, "lrn_pca_bidnum_ohot_over_xgb")
train_and_save(task, lrn_pca_tgtencode_over_xgb, "lrn_pca_tgtencode_over_xgb")
train_and_save(task, lrn_num_over_xgb, "lrn_num_over_xgb")
# train_and_save(task, lrn_pca_ohot_over_xgb, "lrn_pca_ohot_over_xgb")
train_and_save(task, lrn_tgtencode_over_xgb, "lrn_tgtencode_over_xgb")
# train_and_save(task, lrn_pca_ohot_under_xgb, "lrn_pca_ohot_under_xgb")
