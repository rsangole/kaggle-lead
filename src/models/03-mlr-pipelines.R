library(mlr3verse)
library(mlr3learners)
library(data.table)

set.seed(420)

lgr::get_logger("mlr3")$set_threshold("warn")

data("titanic", package = "mlr3data")
setDT(titanic)
# titanic[sample(1:.N, 4), age := NA]

task <- as_task_classif(titanic, target = "survived", positive = "yes")
task$set_row_roles(892:1309, "holdout")
task

task$select(setdiff(task$feature_names, c("cabin", "name", "ticket")))

task

cv3 <- rsmp("cv", folds = 3L)
cv3
cv3$instantiate(task)
cv3

learner <- mlr_learners$get("classif.rpart")
learner

rr <- resample(task, learner, cv3, store_models = TRUE)
rr

rr$aggregate()
rr$aggregate(msr("classif.acc"))

learner <- lrn("classif.ranger", num.trees = 250, min.node.size = 4)
rr <- resample(task, learner, cv3, store_models = TRUE)

task$missings()

# impute categoricals
# Impute factorial features by adding a new level ".MISSING".
# Impute numerical features by constant values shifted below the minimum or above the maximum
po_newlvl <- po("imputeoor")
task_newlvl <- po_newlvl$train(list(task))[[1]]
task_newlvl$data()
task_newlvl$missings()

poind <- po(
        "missind",
        affect_columns = selector_type(c("numeric", "integer")),
        type = "numeric"
)
poind
poind$state
graph <- gunion(
        list(poind, po("imputehist"))
) %>>%
        po("featureunion") %>>%
        po("imputeoor")
graph |> plot()

graph$state

graph$clone()$train(task)[[1]] -> task_imputed

graph$state

task_imputed$missings()
task_imputed$data()


# better method -
# do the imputations for each fold

gunion(list(poind, po("imputehist"))) %>>%
        po("featureunion") %>>%
        po("imputeoor") %>>%
        po("imputesample") %>>%
        po("fixfactors") %>>%
        po(learner) |>
        as_learner() -> graph_learner
graph_learner

rr <- resample(
        task,
        graph_learner,
        cv3,
        store_models = TRUE
)


# feature engg

task$col_roles$feature <- c(
        task$feature_names,
        c("cabin", "name", "ticket")
)
task$data()

library(stringi)
po_ftextract <- po(
        "mutate",
        mutation = list(
                fare_per_per = ~ fare / (parch + sib_sp + 1),
                deck = ~ factor(stri_sub(cabin, 1, 1)),
                title = ~ factor(stri_match(name, regex = ", (.*)\\.")[, 2]),
                surname = ~ factor(stri_match(name, regex = "(.*),")[, 2]),
                ticket_prefix = ~ factor(stri_replace_all_fixed(stri_trim(stri_match(ticket, regex = "(.*) ")[, 2]), ".", ""))
        )
)
po_ftextract

task_eng <- po_ftextract$clone()$train(list(task))[[1]]
task_eng$data()
autoplot(task_eng$clone()$select(c("sex", "age")), type = "pairs")


learner <- lrn("classif.ranger", num.trees = 500, min.node.size = 4)
graph_final <- po_ftextract %>>%
        po("collapsefactors", param_vals = list(no_collapse_above_prevalence = 0.03)) %>>%
        po("select", param_vals = list(selector = selector_invert(selector_type("character")))) %>>%
        gunion(list(poind, po("imputehist"))) %>>%
        po("featureunion") %>>%
        po("imputeoor") %>>%
        po("imputesample") %>>%
        po("fixfactors") %>>%
        po(learner)
graph_learner <- as_learner(graph_final)

rr <- resample(task, graph_learner, cv3, store_models = TRUE)

rr$aggregate(msr("classif.acc"))