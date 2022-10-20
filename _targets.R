# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "tibble",
    "data.table",
    "dtplyr",
    "arrow",
    "dplyr",
    "timetk",
    "mlr3",
    "mlr3data",
    "mlr3filters",
    "mlr3learners",
    "mlr3extralearners",
    "mlr3pipelines",
    "plotly",
    "lattice",
    "catboost"
  ),
  format = "qs"
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source("./src/feat_data.R")

source("_targets_01_data-preprocessor.R")
source("_targets_02_commons.R")
source("_targets_03_xgb.R")
source("_targets_04_rf.R")
source("_targets_05_catboost.R")

list(
  # Inputs
  tar_target(
    HOLDOUT_SET_PERCENTAGE,
    0.3
  ),
  # Tar Scripts
  data_preprocessor,
  commmons,
  xgb,
  rf,
  catboost
)
