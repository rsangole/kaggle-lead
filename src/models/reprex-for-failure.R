# https://github.com/mlr-org/mlr3pipelines/issues/668

library(mlr3verse)
library(mlr3learners)
library(data.table)

set.seed(420)

data("titanic", package = "mlr3data")
# setDT(titanic)

task <- as_task_classif(titanic, target = "survived", positive = "yes")
task$set_row_roles(892:1309, "holdout")
task$select(setdiff(task$feature_names, c("cabin", "name", "ticket")))

cv3 <- rsmp("cv", folds = 3L)
cv3$instantiate(task)

learner <- lrn("classif.ranger", num.trees = 250, min.node.size = 4)

poind <- po(
        "missind",
        affect_columns = selector_type(c("numeric", "integer")),
        type = "numeric"
)

gunion(list(poind, po("imputehist"))) %>>%
        po("featureunion") %>>%
        po("imputeoor") %>>%
        po("fixfactors") %>>%
        po("imputesample") %>>%
        po(learner) |>
        as_learner() -> graph_learner

rr <- resample(
        task,
        graph_learner,
        cv3,
        store_models = TRUE
)

rr$aggregate(msr("classif.ce"))