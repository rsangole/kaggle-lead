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
train

# train %>%
#         slice(1:10) %>%
#         mutate(
#                 year = lubridate::year(timestamp),
#                 month = lubridate::month(timestamp),
#                 week = lubridate::week(timestamp),
#                 quarter = lubridate::quarter(timestamp)
#         ) %>%
#         glimpse()


train %>%
        group_by(primary_use, site_id, building_id) %>%
        write_dataset(path = here::here("data/train/"), format = "parquet")