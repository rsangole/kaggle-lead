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
sid <- "Site-2"
bid <- "Bld-159"

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
    dplyr::arrange(timestamp) -> dat

dat

mp <- tsmp(
    dat$meter_reading,
    window_size = 48,
    n_workers = 5
)
disc <- mp |>
    find_discord(
        n_discords = 2,
        n_neighbours = 4
    )
disc |>
    plot()


ddd <- data.table(ds = 1:nrow(dat))
ddd[1:length(mp$mp), dist := as.numeric(mp$mp)]
ddd[is.na(dist), dist := 0]
ddd[, y := as.numeric(scale(dat$meter_reading))]
ddd |>
    plot_ly(
        x = ~ds,
        y = ~y
    ) |>
    add_lines(name = "y") |>
    add_lines(y = ~dist, name = "dist")

result <- analyze(dat$meter_reading, 180)
result |> plot()

tibble(
    mp = as.numeric(result$mp),
    pi = as.numeric(result$pi)
) -> ddd
ddd |>
    plot_ly(
        x = 1:nrow(ddd),
        y = ~mp
    ) |>
    add_lines()