library("mlr3tuning")
library(mlr3verse)
library(mlr3extralearners)
library(mlr3pipelines)
library(data.table)
library(arrow)
library(plotly)
library(lattice)

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
        label %in% c("train"),
        meter_reading_missing == FALSE
    ) |>
    dplyr::collect() |>
    as.data.table()
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
lrn_rf$param_set$values <- list(importance = "impurity")
set_threads(lrn_rf)

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
    po_encode %>>%
    po_over %>>%
    po("imputeoor") %>>%
    po("fixfactors") %>>%
    po("imputesample") %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_pca_encode_over_rf

evals20 <- trm("evals", n_evals = 20)

cv5 <- rsmp("repeated_cv", folds = 5L, repeats = 1)
cv5$instantiate(task)
cv5

search_space <- ps(
        classif.ranger.num.trees = p_int(lower = 100, upper = 400),
        classif.ranger.mtry = p_int(lower = 7, upper = 13)
)
search_space

instance <- TuningInstanceSingleCrit$new(
        task = task,
        learner = lrn_pca_encode_over_rf,
        resampling = cv5,
        measure = msr("classif.auc"),
        search_space = search_space,
        terminator = evals20
)
instance

tuner <- tnr("grid_search", resolution = 20)
tuner

tuner$optimize(instance)
instance$result_learner_param_vals
instance$result_y
dt <- as.data.table(instance$archive)

qs::qsave(instance, "data/results/stratified-sampling-approach/tuning-instance.qs")

dt

xyplot(classif.auc~classif.ranger.num.trees + classif.ranger.mtry,
data = dt[order(classif.ranger.num.trees)],
type = "o")
