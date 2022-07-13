box::use(data.table[...])
box::use(. / src / feat_data)
box::use(
        arrow[...],
        dplyr[...],
        timetk[...]
)

# Train & Holdout Sets

HOLDOUT_SET_PERCENTAGE <- 0.3
set.seed(42)
dat <- read_csv_arrow("data/data-raw/train_features.csv") |>
        as.data.table()

# Features

# * Clean up primary use col
dat <- feat_data$feat_cleanup_primary_use(dat)

# * Clean up other categoricals
dat <- feat_data$feat_cleanup_categoricals(dat)

setkey(dat, timestamp)

# * Label Train & Hold Out Obs
dat[, row_id := 1:.N]
row_id_dat <- dat[, row_id, .(anomaly, building_id)]
holdout_indices <- row_id_dat[
        , .(row_id = sample(row_id, size = HOLDOUT_SET_PERCENTAGE * nrow(.SD))),
        .(anomaly, building_id)
]
holdout_indices[, label := "holdout"]
holdout_indices[, c("anomaly", "building_id") := NULL]

dat <- merge.data.table(
        x = dat,
        y = holdout_indices,
        by = "row_id",
        all.x = TRUE
)
dat[is.na(label), label := "train"]

dat <- feat_data$feat_replaceNA_add_lags(dat)

dat[]

# Save to disk
dat %>%
        group_by(label, primary_use, site_id, building_id) %>%
        write_dataset(path = here::here("data/arrow-stratifiedsampling/"), format = "parquet")

rm(dat)
gc(1, 1, 1)

# Kaggle Test Data Set

dat <- read_csv_arrow("data/data-raw/test_features.csv") |>
        as.data.table()

# * Clean up primary use col
dat <- feat_data$feat_cleanup_primary_use(dat)

# * Clean up other categoricals
dat <- feat_data$feat_cleanup_categoricals(dat)

setkey(dat, timestamp)

dat <- feat_data$feat_replaceNA_add_lags(dat)

dat[, label := "test"]

dat %>%
        group_by(label, primary_use, site_id, building_id) %>%
        write_dataset(path = here::here("data/arrow-stratifiedsampling/"), format = "parquet")


# ---
# Investigate Test & Train Sets Further

# * read in the data
train_dat <- read_csv_arrow("data/data-raw/train.csv") |>
        dplyr::mutate(cat = "train")
test_dat <- read_csv_arrow("data/data-raw/test.csv") |>
        dplyr::mutate(cat = "test")

# * combine into one
dat <- rbindlist(
        list(
                train_dat,
                test_dat
        ),
        use.names = TRUE,
        fill = TRUE
)
dat[]

# Investigate
# * For each Building Id
# * Does it exist in both sets
x <- dat[, .N, .(building_id, cat)] |>
        dcast(building_id ~ cat, value.var = "N")
x[, inboth := !is.na(test) & !is.na(train)]
x[, .N, inboth]

# * For each Building Id + Site Id combination
# * Does it exist in both sets
x <- dat[, .N, .(building_id, site_id, cat)] |>
        dcast(building_id + site_id ~ cat, value.var = "N")
x[, inboth := !is.na(test) & !is.na(train)]
x[inboth == F]
x
x <- dat[, .N, .(site_id, cat)] |>
        dcast(site_id ~ cat, value.var = "N")
x[, inboth := !is.na(test) & !is.na(train)]
x[inboth == F]

# ** Takeaways
# ** Sites are common between train & test, except 2
# ** None of the buildings are common between train & test
