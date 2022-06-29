# box::use(mlr3[...])
library(mlr3verse)
library(mlr3extralearners)
box::use(mlr3learners[...])
box::use(data.table[...])
box::use(arrow[...])
box::use(plotly[...])
box::use(. / src / plotters)

set.seed(420)

lgr::get_logger("mlr3")$set_threshold("warn")

ds <- open_dataset("data/arrow")
ds

# Let's try for a small dataset

dat <- ds |>
        dplyr::filter(
                label == "train",
                primary_use %in% c("healthcare", "services", "parking")
        ) |>
        dplyr::select(
                -"building_hour",
                -"building_meter",
                -"building_month",
                -"building_weekday",
                -"building_weekday_hour",
                -"weekday_hour",
                -"label"
        ) |>
        dplyr::collect() |>
        as.data.table() |>
        tidyr::drop_na()
dat[, .N, site_id]
dat[, .N, primary_use]
dat[]

plotters$plot_ts(dat)

# Dropping all factor variables for now
# Dropping NA values for now
# Dropping timestamps

task <- as_task_classif(
        dat,
        target = "anomaly",
        positive = "A1"
)
task$set_col_roles("timestamp", "order")
# task$set_col_roles(c("primary_use", "site_id", "building_id"), NULL)

cv5 <- rsmp("repeated_cv", folds = 5L, repeats = 1)
cv5$instantiate(task)
cv5

learner <- lrn("classif.ranger", predict_type = "prob")
learner$param_set$values <- list(importance = "impurity")
learner

rr <- resample(task, learner, cv5, store_models = TRUE)
rr

rr$aggregate()
# rr$aggregate(msr("classif.acc"))
# rr$aggregate(msr("classif.auc"))
# rr$aggregate(msr("classif.fnr"))
# rr$aggregate(msr("classif.fn"))
# rr$aggregate(msr("classif.fp"))
# rr$aggregate(msr("classif.fpr"))
rr$aggregate(msr("classif.ppv")) # TP/(TP+FP) : Usefulness of +ve prediction
rr$aggregate(msr("classif.tpr")) # TP / P : % of anomalies caught
rr$predictions()[[1]] |>
        as.data.table() |>
        filter(truth == "A1") %>%
        .[truth != response, ]


# boxplot of AUC values across the 10 folds
autoplot(rr, measure = msr("classif.auc"))
# ROC curve, averaged over 10 folds
autoplot(rr, type = "roc")

pred <- learner$train(task)$predict(task)
C <- pred$confusion
print(C)
autoplot(pred, type = "prc")

filter <- flt("importance", learner = learner)
filter$calculate(task)
filter |>
        as.data.table() |>
        ggplot(aes(x = forcats::fct_reorder(feature, score), y = score)) +
        geom_col() +
        coord_flip()

design <- benchmark_grid(
        tasks = task,
        learners = lrns(c("classif.ranger", "classif.featureless"),
                predict_type = "prob", predict_sets = c("train", "test")
        ),
        resamplings = rsmps("cv", folds = 3)
)
print(design)

bmr <- benchmark(design)
bmr
measures <- list(
        msr("classif.auc", predict_sets = "train", id = "auc_train"),
        msr("classif.auc", id = "auc_test"),
        msr("classif.ppv", predict_sets = "train"), # TP/(TP+FP) : Usefulness of +ve prediction
        msr("classif.tpr", predict_sets = "train") # TP / P : % of anomalies caught
)

tab <- bmr$aggregate(measures)
print(tab)


autoplot(bmr)
autoplot(bmr, type = "roc")


search_space <- ps(
        num.trees = p_int(lower = 100, upper = 400)
)
search_space

library("mlr3tuning")
evals20 <- trm("evals", n_evals = 20)

instance <- TuningInstanceSingleCrit$new(
        task = task,
        learner = learner,
        resampling = cv5,
        measure = msr("classif.tpr"),
        search_space = search_space,
        terminator = evals20
)
instance

tuner <- tnr("grid_search", resolution = 5)
tuner

tuner$optimize(instance)
instance$result_learner_param_vals
instance$result_y
as.data.table(instance$archive)


# learner <- lrn("classif.ranger", num.trees = 250, min.node.size = 4)
# rr <- resample(task, learner, cv5, store_models = TRUE)

# task$missings()

# # impute categoricals
# # Impute factorial features by adding a new level ".MISSING".
# # Impute numerical features by constant values shifted below the minimum or above the maximum
# po_newlvl <- po("imputeoor")
# task_newlvl <- po_newlvl$train(list(task))[[1]]
# task_newlvl$data()
# task_newlvl$missings()

# poind <- po(
#         "missind",
#         affect_columns = selector_type(c("numeric", "integer")),
#         type = "numeric"
# )
# poind
# poind$state
# graph <- gunion(
#         list(poind, po("imputehist"))
# ) %>>%
#         po("featureunion") %>>%
#         po("imputeoor")
# graph |> plot()

# graph$state

# graph$clone()$train(task)[[1]] -> task_imputed

# graph$state

# task_imputed$missings()
# task_imputed$data()


# # better method -
# # do the imputations for each fold

gunion(list(poind, po("imputehist"))) %>>%
        po("featureunion") %>>%
        po("imputeoor") %>>%
        po("imputesample") %>>%
        po("fixfactors") %>>%
        po(learner) |>
        as_learner() -> graph_learner
graph_learner

# rr <- resample(
#         task,
#         graph_learner,
#         cv5,
#         store_models = TRUE
# )


# # feature engg

# task$col_roles$feature <- c(
#         task$feature_names,
#         c("cabin", "name", "ticket")
# )
# task$data()

# library(stringi)
# po_ftextract <- po(
#         "mutate",
#         mutation = list(
#                 fare_per_per = ~ fare / (parch + sib_sp + 1),
#                 deck = ~ factor(stri_sub(cabin, 1, 1)),
#                 title = ~ factor(stri_match(name, regex = ", (.*)\\.")[, 2]),
#                 surname = ~ factor(stri_match(name, regex = "(.*),")[, 2]),
#                 ticket_prefix = ~ factor(stri_replace_all_fixed(stri_trim(stri_match(ticket, regex = "(.*) ")[, 2]), ".", ""))
#         )
# )
# po_ftextract

# task_eng <- po_ftextract$clone()$train(list(task))[[1]]
# task_eng$data()
# autoplot(task_eng$clone()$select(c("sex", "age")), type = "pairs")


# learner <- lrn("classif.ranger", num.trees = 500, min.node.size = 4)
# graph_final <- po_ftextract %>>%
#         po("collapsefactors", param_vals = list(no_collapse_above_prevalence = 0.03)) %>>%
#         po("select", param_vals = list(selector = selector_invert(selector_type("character")))) %>>%
#         gunion(list(poind, po("imputehist"))) %>>%
#         po("featureunion") %>>%
#         po("imputeoor") %>>%
#         po("imputesample") %>>%
#         po("fixfactors") %>>%
#         po(learner)
# graph_learner <- as_learner(graph_final)

# rr <- resample(task, graph_learner, cv5, store_models = TRUE)

# rr$aggregate(msr("classif.acc"))
