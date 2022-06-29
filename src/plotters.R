
#' @export
plot_ts_from_arrow <- function(ds, puse, sid, bid = NULL) {
        box::use(plotly[...])
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
                dplyr::group_by(building_id) -> dat
        dat |>
                dplyr::do(
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


#' @export
plot_ts <- function(dat) {
        box::use(plotly[...])
        dat |>
                dplyr::group_by(building_id) -> dat
        dat |>
                dplyr::do(
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
                plotly::subplot(nrows = length(unique(dat$building_id)))
}
