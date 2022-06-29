box::use(. / src / feat_data)
box::use(
        arrow[...],
        dplyr[...],
        timetk[...]
)

dat <- read_csv_arrow("data/data-raw/train_features.csv")

# Features

# * Clean up primary use col
dat$primary_use <- as.factor(dat$primary_use)
levels(dat$primary_use) <- janitor::make_clean_names(levels(dat$primary_use))
dat$primary_use <- as.character(dat$primary_use)

# * Clean up other categoricals
dat <- dat |>
        mutate(
                building_id = as.factor(sprintf("Bld-%d", building_id)),
                site_id = as.factor(sprintf("Site-%d", site_id)),
                anomaly = as.factor(sprintf("A%d", anomaly))
        ) |>
        arrange(timestamp)

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

dat <- dat |>
        feat_data$feat_removeNA_add_lags()

# Save to disk
dat %>%
        group_by(label, primary_use, site_id, building_id) %>%
        write_dataset(path = here::here("data/arrow/"), format = "parquet")

rm(dat)
gc(1, 1, 1)



# dat2 |>
#         filter(site_id == "Site-1", building_id == "Bld-107") |>
#         plot_ly(
#                 x = ~timestamp
#         ) |>
#         add_lines(y = ~meter_reading, name = "meter_reading") |>
#         add_lines(y = ~meter_reading_roll_24, name = "meter_reading_roll_24") |>
#         add_lines(y = ~meter_reading_lag1_diff1, name = "meter_reading_lag1_diff1") |>
#         add_lines(y = ~meter_reading_lag1_diff2, name = "meter_reading_lag1_diff2")

# dat2 |>
#         select(timestamp, contains("meter_reading")) |>
#         filter(site_id == "Site-0", building_id == "Bld-1")

# dat |>
#         filter(site_id == "Site-1", building_id == "Bld-108") |>
#         mutate(meter_clean = ts_impute_vec(meter_reading)) |>
#         plot_ly(x = ~timestamp) |>
#         add_markers(y = ~meter_reading, name = "y") |>
#         add_lines(y = ~meter_clean, name = "clean")

# dat |>
#         filter(site_id == "Site-0", building_id == "Bld-1") |>
#         filter(
#                 timestamp > "2016-05-25",
#                 timestamp < "2016-07-01"
#         ) |>
#         mutate(meter_clean = ts_impute_vec(meter_reading)) |>
#         ggplot(aes(x = timestamp, y = meter_reading)) +
#         geom_point(aes(y = meter_clean)) +
#         geom_miss_point()

# # Views
# dat |>
#         group_by(building_id) |>
#         summarise(across(where(is.numeric), var)) |>
#         View()
# dat |>
#         count(label) |>
#         janitor::adorn_percentages("col", , n) |>
#         janitor::adorn_pct_formatting()
