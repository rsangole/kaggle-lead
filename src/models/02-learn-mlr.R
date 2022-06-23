library(mlr3)
library(mlr3data)
library(mlr3tuning)
library(data.table)
library(mlr3pipelines)
library(mlr3learners)
library(mlr3viz)
# library(magrittr)

learner <- lrn("classif.rpart")
learner

task <- tsk("spam")
task

learner$train(task)

learner
learner$model

learner$param_set
learner$param_set$values <- list(maxdepth = 1)
learner$train(task)
print(learner$model)

learner$predict(task)
# learner$predict_newdata()

learner$predict(task)$score(msr("classif.ce"))

msr()

cv5 <- rsmp("cv", folds = 5)
resample(task, learner, cv5) -> rr
as.data.table(rr)
print(rr)

rr$aggregate()
rr$predictions()
rr$score()

# benchmarking
future::plan("multicore")
learners <- list(lrn("classif.rpart"), lrn("classif.kknn"))
learner$encapsulate <- c(train = "callr", predict = "callr")
tasks <- list(tsk("iris"), tsk("sonar"), tsk("spam"))
design <- benchmark_grid(tasks, learners, cv5)
design
bmr <- benchmark(design)
bmr
bmr$aggregate()
autoplot(bmr)

library(bbotk)
library(mlr3tuning)
library(paradox)

inst = TuningInstanceSingleCrit$new(
        tsk("iris"),
        lrn('classif.kknn'), 
	cv5, 
	msr('classif.ce'),
	trm("none")
)
inst

gsearch = tnr('grid_search', resolution = 4)
gsearch$optimize(inst)



# -------

train
