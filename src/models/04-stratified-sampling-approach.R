library(mlr3verse)
library(mlr3extralearners)
library(mlr3pipelines)
# install_catboost()
# install_learners('classif.gamboost')
library(data.table)
library(arrow)
library(plotly)
library(lattice)
box::use(. / src / plotters)

set.seed(42)

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
    dplyr::filter(label == "train") |>
    dplyr::collect() |>
    as.data.table()
dat[]
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

dat[]

# Train/Holdout Splits
# train_set <- dat[label == 'train', row_id]
# test_set <- dat[label == 'holdout', row_id]
# head(train_set); head(test_set)

# Task
task <- as_task_classif(dat,
    target = "anomaly",
    positive = "A1"
)
task$select(setdiff(task$feature_names, "label")) # dropping 'Label'
task

table(task$truth())

# Resampling Strategy
cv3 <- rsmp("cv", folds = 3)
cv3$instantiate(task)
cv3

# Parameters

# Measures
measure <- list(
    msr("classif.fbeta"),
    msr("classif.auc", predict_sets = "train", id = "auc_train"),
    msr("classif.auc", id = "auc_test")
)
measure

# Base Leaners

lrn_rf <- lrn("classif.ranger", num.trees = 50, predict_type = "prob")
set_threads(lrn_rf)

lrn_glmnet <- lrn("classif.glmnet", predict_type = "prob")

lrn_xgb <- lrn("classif.xgboost", predict_type = "prob")
set_threads(lrn_xgb)

lrn_lightgbm <- lrn("classif.lightgbm", predict_type = "prob")
set_threads(lrn_lightgbm)

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
    po(lrn_rf) |>
    as_learner() -> lrn_pca_encode_over_rf

po_pca_temperature %>>%
    po_encode %>>%
    po_under %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_pca_encode_under_rf

po_pca_temperature %>>%
    po_over %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_pca_over_rf

po_over %>>%
    po(lrn_rf) |>
    as_learner() -> lrn_over_rf

po_pca_temperature %>>%
    po_encode %>>%
    po_over %>>%
    po(lrn_xgb) |>
    as_learner() -> lrn_pca_encode_over_xgb

po_pca_temperature %>>%
    po_encode %>>%
    po_over %>>%
    po(lrn_lightgbm) |>
    as_learner() -> lrn_pca_encode_over_lightgbm

# Benchmarking
design <- benchmark_grid(
    tasks = task,
    learners = list(
        lrn_pca_encode_over_rf,
        lrn_pca_encode_under_rf,
        lrn_pca_over_rf,
        lrn_over_rf,
        lrn_pca_encode_over_xgb,
        lrn_pca_encode_over_lightgbm
    ),
    resamplings = cv3
)
design
bmr <- benchmark(design)
bmr$aggregate(measure)


# # TUNING --

# # * combine learner with pipeline graph
# learner_under <- as_learner(po_under %>>% learner)
# learner_under$id <- "undersample.ranger"
# learner_over <- as_learner(po_over %>>% learner)
# learner_over$id <- "oversample.ranger"

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
