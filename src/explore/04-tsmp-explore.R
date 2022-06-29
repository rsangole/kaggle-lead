library(tsmp)
library(dplyr)
library(arrow)
library(ggplot2)
library(lattice)
library(plotly)
library(data.table)

ds <- open_dataset(here::here("data/arrow"))

ds

puse <- "Education"
sid <- "Site-0"
bid <- "Bld-91"

plot_ts(puse, sid, bid)

ds |>
    dplyr::filter(
        primary_use == puse,
        site_id == sid,
        building_id == bid
    ) |>
    dplyr::select(
        timestamp,
        meter_reading,
        anomaly,
        building_id
    ) |>
    dplyr::collect() |>
    dplyr::arrange(timestamp) |>
    dplyr::mutate(rowid = 1:n()) |>
    dplyr::filter(timestamp < "2016-10-01") -> dat

dat

mp <- tsmp(
    dat$meter_reading,
    window_size = 24,
    n_workers = 5
)
plot(mp)
disc <- mp |>
    find_discord(
        n_discords = 8
    )
disc |>
    plot()


discord_vec <- c(
    purrr::pluck(disc, "discord", "discord_idx") |> unlist(),
    purrr::pluck(disc, "discord", "discord_neighbor") |> unlist()
)
discord_vec
dat |>
    mutate(
        discord = rowid %in% discord_vec,
        anomaly = as.numeric(anomaly) - 1
    ) |>
    plot_ly(
        x = ~timestamp,
        y = ~meter_reading
    ) |>
    add_lines() |>
    add_markers(
        y = ~ anomaly * meter_reading,
        marker = list(color = "red")
    ) |>
    add_markers(
        y = ~ discord * meter_reading,
        marker = list(color = "green")
    )

# discords(mp, k = 2L, neighbor_count = 15) |> plot()

data.table::data.table(
    .mp = as.numeric(mp$mp),
    .pi = as.numeric(mp$pi),
    .rmp = as.numeric(mp$rmp),
    .rpi = as.numeric(mp$rpi)
) -> dt
dt[, rowid := 1:.N]

discord_vec
dt[rowid %in% discord_vec, discord := .mp]

dt |>
    plot_ly(x = ~rowid, y = ~.mp, name = "mp") |>
    add_lines() |>
    add_markers(y = ~discord, marker = list(color = "red")) |>
    add_lines(
        data = dat,
        x = ~rowid,
        y = ~meter_reading,
        inherit = FALSE,
        name = "y"
    ) |>
    add_lines(
        x = 1:nrow(dat),
        y = rep(9, nrow(dat)),
        fill = "red",
        inherit = FALSE
    )

plot_ts(puse, sid, bid) |>
    add_lines(data = dt, x = ~rowid, y = ~.mp, name = "mp")

# ddd <- data.table(ds = 1:nrow(dat))
# ddd[1:length(mp$mp), dist := as.numeric(mp$mp)]
# ddd[is.na(dist), dist := 0]
# ddd[, y := as.numeric(scale(dat$meter_reading))]
# ddd |>
#     plot_ly(
#         x = ~ds,
#         y = ~y
#     ) |>
#     add_lines(name = "y") |>
#     add_lines(y = ~dist, name = "dist")

# result <- analyze(dat$meter_reading, 180)
# result |> plot()

# tibble(
#     mp = as.numeric(result$mp),
#     pi = as.numeric(result$pi)
# ) -> ddd
# ddd |>
#     plot_ly(
#         x = 1:nrow(ddd),
#         y = ~mp,

#     ) |>
#     add_lines()
