library(arrow)
library(lattice)
library(dplyr)
library(plotly)
library(tidyr)
library(tidymodels)
library(recipes)
library(xgboost)
library(lightgbm)

ds <- open_dataset("data/arrow")

train <- ds %>%
    filter(label == "train") %>%
    filter(anomaly == "A0") %>%
    select(-contains("gte")) %>%
    select(
        -timestamp,
        -rowname,
        -contains("gte"),
        -train_end,
        -label,
        -anomaly,
        -weekday_hour:-building_meter
    ) %>%
    collect() %>%
    slice_sample(n = 10000) %>%
    tidyr::drop_na()
train

tsplit <- initial_split(train)
training_tbl <- training(tsplit)
test_tbl <- testing(tsplit)

rec_1 <- recipe(
    meter_reading ~ .,
    data = training_tbl
) %>%
    # step_naomit(all_predictors()) %>%
    # step_dummy(all_nominal_predictors()) %>%
    step_pca(contains("temperature"))

rec_1

rec_1 %>%
    prep() %>%
    juice() %>%
    glimpse()


rf_model <- rand_forest(trees = 300) %>%
    set_engine("ranger") %>%
    set_mode("regression")
rf_model

rf_wflow <- workflow() %>%
    add_recipe(rec_1) %>%
    add_model(rf_model)

rf_wflow

rf_fit <- rf_wflow %>%
    fit(data = training_tbl)
rf_fit

rf_fit %>%
    predict(test_tbl) %>%
    mutate(test_tbl %>% select(y = meter_reading)) %>%
    ggplot(aes(y, .pred)) +
    geom_point(alpha = .15) +
    geom_abline(color = "red") +
    coord_obs_pred() +
    ylab("Predicted")