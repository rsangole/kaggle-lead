box::use(data.table[...])
box::use(. / src / feat_data)
box::use(
        arrow[...],
        dplyr[...],
        timetk[...]
)

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

dat <- feat_data$feat_removeNA_add_lags(dat)

dat[]

# Save to disk
dat %>%
        group_by(label, primary_use, site_id, building_id) %>%
        write_dataset(path = here::here("data/arrow-stratifiedsampling/"), format = "parquet")

rm(dat)
gc(1, 1, 1)
