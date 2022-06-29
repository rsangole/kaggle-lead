library(arrow)
library(data.table)
library(dplyr)
library(janitor)
library(ggplot2)
library(plotly)
library(lattice)

ds <- open_dataset(here::here("data/arrow"))

ds

plot_ts <- function(puse, sid, bid = NULL) {
        dat <- ds |>
                dplyr::filter(
                        primary_use == puse,
                        site_id == sid
                )
        if (!is.null(bid)) {
                dat <- dat |>
                        dplyr::filter(building_id %in% bid)
        }
        dat |>
                dplyr::select(
                        timestamp,
                        meter_reading,
                        anomaly,
                        building_id
                ) |>
                dplyr::collect() |>
                group_by(building_id) -> dat
        dat |>
                do(
                        p = . |>
                                plot_ly(
                                        x = ~timestamp,
                                        y = ~meter_reading
                                ) |>
                                add_lines(
                                        name = ~ unique(building_id)
                                ) |>
                                add_markers(
                                        data = . |>
                                                dplyr::filter(anomaly == "A1"),
                                        x = ~timestamp,
                                        y = ~meter_reading,
                                        name = "anomaly",
                                        inherit = FALSE,
                                        marker = list(color = "red"),
                                        legendgroup = "anomaly",
                                        showlegend = FALSE
                                )
                ) |>
                plotly::subplot(nrows = length(unique(dat$building_id))) |>
                plotly::layout(title = sprintf("%s : %s", puse, sid))
}

plot_ts("Education", "Site-0")
plot_ts("Education", "Site-1")

