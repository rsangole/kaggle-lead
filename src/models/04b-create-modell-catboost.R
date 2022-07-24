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

dat[, building_id := as.numeric(gsub("Bld-", "", building_id))]
dat[, site_id := as.numeric(gsub("Site-", "", site_id))]
dat <- dat[complete.cases(dat)]
dat[, cloud_coverage_missing := as.factor(cloud_coverage == 255)]
dat[, year_built_missing := as.factor(year_built == 255)]
dat[, meter_reading_missing := as.factor(meter_reading_missing)]
dat[, primary_use := as.factor(primary_use)]
int_to_dbl_cols <- dat[, names(.SD), .SDcols = is.numeric]
dat[, (int_to_dbl_cols) := lapply(.SD, as.double), .SDcols = int_to_dbl_cols]

task <- as_task_classif(dat,
    target = "anomaly",
    positive = "A1",
    id = "bid_numeric"
)
task$select(setdiff(task$feature_names, "label")) # dropping 'Label'
table(task$truth())
task


# Base Leaners
lrn_catboost <- lrn(
    "classif.catboost",
    predict_type = "prob",
    predict_sets = c("train", "test")
)
set_threads(lrn_catboost)
lrn_catboost$param_set$values <- list(task_type = "CPU")

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

po_tgtencode %>>%
    po_over %>>%
    # po("imputeoor") %>>%
    # po("fixfactors") %>>%
    # po("imputesample") %>>%
    po(lrn_catboost) |>
    as_learner() -> lrn_tgtencode_over_catboost
# lrn_tgtencode_over_catboost$predict_sets <- c("train", "test")
# lrn_tgtencode_over_catboost$param_set$values <- list(classif.catboost.task_type = "GPU")

lrn_tgtencode_over_catboost$train(task)

# po_pca_temperature %>>%
#     po_encode %>>%
#     po_over %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_xgb) |>
#     as_learner() -> lrn_pca_encode_over_xgb
# lrn_pca_encode_over_xgb$train(task)

# po_tgtencode %>>%
#     po_over %>>%
#     po("imputeoor") %>>%
#     po("fixfactors") %>>%
#     po("imputesample") %>>%
#     po(lrn_rf) |>
#     as_learner() -> lrn_tgtencode_over_rf
# lrn_tgtencode_over_rf$predict_sets <- c("train", "test")
# lrn_tgtencode_over_rf$ train(task)

qs::qsave(lrn_tgtencode_over_catboost, "data/results/stratified-sampling-approach/lrn_tgtencode_over_catboost.qs")
