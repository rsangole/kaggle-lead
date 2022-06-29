library(mlr3verse)
library(mlr3extralearners)
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
task

table(task$truth())

# Resampling Strategy
inner_cv3 <- rsmp("cv", folds = 3)
inner_cv3

# Learners

# Parameters

# Measures
measure <- msr("classif.fbeta")
measure

# Pipeline
# * undersample majority class
po_under <- po("classbalancing",
    id = "undersample",
    adjust = "major",
    reference = "major",
    shuffle = FALSE,
    ratio = 1 / 50
)
table(po_under$train(list(task))$output$truth())
# * oversample minority class
po_over <- po("classbalancing",
    id = "oversample",
    adjust = "minor",
    reference = "minor",
    shuffle = FALSE,
    ratio = 10
)
table(po_over$train(list(task))$output$truth())

# Autotuner
# * create random forest learner
learner <- lrn("classif.ranger", num.trees = 50)
set_threads(learner)

# * combine learner with pipeline graph
learner_under <- as_learner(po_under %>>% learner)
learner_under$id <- "undersample.ranger"
learner_over <- as_learner(po_over %>>% learner)
learner_over$id <- "oversample.ranger"

# * define parameter search space for each method
search_space_under <- ps(undersample.ratio = p_dbl(1 / 50, 1))
search_space_over <- ps(oversample.ratio = p_dbl(1, 10))

learns <- list(
    auto_tuner(
        method = "grid_search",
        learner = learner_under,
        resampling = inner_cv3,
        measure = measure,
        search_space = search_space_under,
        resolution = 6
    ),
    auto_tuner(
        method = "grid_search",
        learner = learner_over,
        resampling = inner_cv3,
        measure = measure,
        search_space = search_space_over,
        resolution = 6
    )
)
learns

outer_holdout <- rsmp("holdout")
design <- benchmark_grid(
    tasks = task,
    learners = learns,
    resamplings = outer_holdout
)
print(design)

future::plan("multisession")
bmr = benchmark(design, store_models = TRUE)
bmr$aggregate(measure)
autoplot(bmr, measure = measure)
library(ggplot2)
bmr_data_learners <- as.data.table(bmr)$learner
utune_path <- bmr_data_learners[[1]]$model$tuning_instance$archive$data
utune_gg <- ggplot(utune_path, aes(x = undersample.ratio, y = classif.fbeta)) +
    geom_point(size = 3) +
    geom_line() +
    ylim(0.7, 1)
otune_path <- bmr_data_learners[[2]]$model$tuning_instance$archive$data
otune_gg <- ggplot(otune_path, aes(x = oversample.ratio, y = classif.fbeta)) +
    geom_point(size = 3) +
    geom_line() +
    ylim(0.7, 1)
cowplot::plot_grid(utune_gg, otune_gg)
