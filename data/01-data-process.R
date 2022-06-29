box::use(data.table[...])
box::use(. / src / feat_data)
box::use(
        arrow[...],
        dplyr[...],
        timetk[...]
)

dat <- read_csv_arrow("data/data-raw/train_features.csv") |>
        as.data.table()

# Features

# * Clean up primary use col
dat <- feat_data$feat_cleanup_primary_use(dat)

# * Clean up other categoricals
dat <- feat_data$feat_cleanup_categoricals(dat)

setkey(dat, timestamp)

# Calculate Train + Holdout Splits by Time
timestamps <- dat |>
        group_by(building_id) |>
        summarise(mx = max(timestamp), mn = min(timestamp)) |>
        mutate(
                ddays = mx - mn,
                train_days = 0.7 * ddays,
                train_end = mn + train_days
        )
timestamps

# * Label Train & Hold Out Obs
dat <- dat |>
        left_join(timestamps |> select(building_id, train_end)) |>
        mutate(
                label = case_when(
                        timestamp <= train_end ~ "train",
                        TRUE ~ "holdout"
                )
        ) |>
        dplyr::select(-train_end)

dat <- feat_data$feat_removeNA_add_lags(dat)

# Save to disk
dat %>%
        group_by(label, primary_use, site_id, building_id) %>%
        write_dataset(path = here::here("data/arrow/"), format = "parquet")

rm(dat)
gc(1, 1, 1)