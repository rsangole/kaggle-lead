library(arrow)
library(dplyr)
library(httpgd)
library(languageserver)

train <- read_csv_arrow("data/data-raw/train_features.csv")

# Drop NA
train <- train %>% filter(!is.na(meter_reading))

# # Add features
train <- train %>% tibble::rownames_to_column()
train$primary_use <- as.factor(train$primary_use)
levels(train$primary_use) <- janitor::make_clean_names(levels(train$primary_use))
train$primary_use <- as.character(train$primary_use)
train <- train %>%
        mutate(
                building_id = as.factor(sprintf("Bld-%d", building_id)),
                site_id = as.factor(sprintf("Site-%d", site_id)),
                anomaly = as.factor(sprintf("A%d", anomaly))
        )
train

train %>%
        group_by(building_id) %>%
        summarise(across(where(is.numeric), var)) %>%
        View()

# Train + Test Split
timestamps <- train %>%
        group_by(building_id) %>%
        summarise(mx = max(timestamp), mn = min(timestamp)) %>%
        mutate(
                ddays = mx - mn,
                train_days = 0.7 * ddays,
                train_end = mn + train_days
        )
timestamps

train <- train %>%
        left_join(timestamps %>% select(building_id, train_end)) %>%
        mutate(
                label = case_when(
                        timestamp <= train_end ~ "train",
                        TRUE ~ "test"
                )
        )
train %>%
        count(label) %>%
        janitor::adorn_percentages("col", , n) %>%
        janitor::adorn_pct_formatting()

train %>%
        group_by(label, primary_use, site_id, building_id) %>%
        write_dataset(path = here::here("data/arrow/"), format = "parquet")