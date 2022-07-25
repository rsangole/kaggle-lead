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
        label %in% c("train"),
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

# evaluation
evals <- trm("evals", n_evals = 30)
evals

# cross validation
cv3 <- rsmp("repeated_cv", folds = 3L, repeats = 1)
cv3$instantiate(task)
cv3

search_space <- ps(
    classif.xgboost.eta = p_dbl(lower = 0.01, upper = 0.3),
    classif.xgboost.colsample_bytree = p_dbl(lower = 0.5, upper = 1),
    classif.xgboost.max_depth = p_int(lower = 3, upper = 10)
)
search_space

# func
tune_and_save <- function(tsk,
                          lrnr,
                          resamp,
                          meas = msr("classif.auc"),
                          ss,
                          term,
                          tuner,
                          outglob,
                          logfile,
                          outpath = "data/results/stratified-sampling-approach") {
    tf <- fs::path(outpath, logfile, ext = "log")
    lgr$add_appender(AppenderFile$new(tf), name = "file")

    lgr$info("--- New tune & save call: %s ---", outglob)

    lgr$info("Creating instance")
    instance <- TuningInstanceSingleCrit$new(
        task = tsk,
        learner = lrnr,
        resampling = resamp,
        measure = meas,
        search_space = ss,
        terminator = term
    )

    lgr$info("Optimizing")
    tuner$optimize(instance)
    dt <- as.data.table(instance$archive)

    lgr$info("Saving outputs to qs files")
    qs::qsave(instance, fs::path(outpath, paste0(outglob, "-instance"), ext = "qs"))
    qs::qsave(
        tibble::lst(
            result_learner_param_vals = instance$result_learner_param_vals,
            result_y = instance$result_y,
            result = dt
        ),
        fs::path(outpath, paste0(outglob, "-result"), ext = "qs")
    )

    lgr$info("Done")
}

tune_and_save(
    tsk = task,
    lrnr = lrn_pca_bidnum_ohot_over_xgb,
    resamp = cv3,
    meas = msr("classif.auc"),
    ss = ps(
        classif.xgboost.colsample_bytree = p_dbl(lower = 0.5, upper = 1),
        classif.xgboost.max_depth = p_int(lower = 10, upper = 20)
    ),
    term = trm("evals", n_evals = 30),
    tuner = tnr("grid_search", resolution = 4),
    outglob = "lrn_pca_bidnum_ohot_over_xgb",
    logfile = "xgb-tuning"
)

tune_and_save(
    tsk = task,
    lrnr = lrn_num_over_xgb,
    resamp = cv3,
    meas = msr("classif.auc"),
    ss = ps(
        classif.xgboost.colsample_bytree = p_dbl(lower = 0.5, upper = 1),
        classif.xgboost.max_depth = p_int(lower = 10, upper = 20)
    ),
    term = trm("evals", n_evals = 30),
    tuner = tnr("grid_search", resolution = 4),
    outglob = "lrn_num_over_xgb",
    logfile = "xgb-tuning"
)
