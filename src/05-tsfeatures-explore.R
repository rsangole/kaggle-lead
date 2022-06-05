library(tsfeatures)
library(arrow)
library(lattice)
library(dplyr)
library(plotly)
library(tidyr)
library(slider)

ds <- open_dataset("data/train")

main_tbl <- ds %>%
    select(building_id, site_id, anomaly, primary_use, meter_reading) %>%
    collect() %>%
    mutate(
        building_id = sprintf("Bld-%d", building_id),
        site_id = sprintf("Site-%d", site_id),
        anomaly = sprintf("A%d", anomaly)
    )
main_tbl


ts_feat_each_building <- function(dat) {
    tsfeatures(dat$meter_reading)
}

feat_tbl <- main_tbl %>%
    group_by(site_id, anomaly) %>%
    group_modify(~ ts_feat_each_building(.x))
feat_tbl

dotplot(trend ~ anomaly,
    auto.key = TRUE,
    feat_tbl
)

feat_tbl %>%
    pivot_longer(-site_id:-anomaly) %>%
    pivot_wider(names_from = anomaly) %>%
    ggplot(aes(
        x = site_id
    )) +
    geom_point(aes(y = A0), color = "#3d5dc7") +
    geom_point(aes(y = A1), color = "#df8181") +
    geom_segment(aes(
        x = site_id,
        xend = site_id, y = A0, yend = A1
    ), color = "#4e68b5ae") +
    facet_grid(~name, scales = "free") +
    coord_flip()


# Rolling Window TS Feature Based Approach
## For each univariate time-series
## Convert `meter_reading` to a set of ts-features, rolling
## last point in the window is marked as anomaly/not using truth
## [X] matrix will be [tsfeatures], {y} will be {anomaly}
## use logistic reg / xgboost to run classification

# Qs?
## How many models need to be run?
## One for each site? Each use case?


# POC -----

## Select one series

subs_tbl <- ds %>%
    filter(building_id == 1) %>%
    select(building_id, site_id, anomaly, primary_use, meter_reading, timestamp) %>%
    collect() %>%
    mutate(
        building_id = sprintf("Bld-%d", building_id),
        site_id = sprintf("Site-%d", site_id),
        anomaly = sprintf("A%d", anomaly)
    )
subs_tbl

plot_ly(
    data = subs_tbl,
    x = ~timestamp,
    y = ~meter_reading,
    color = ~anomaly,
    symbol = ~anomaly
) %>%
    add_markers()

window_size <- 24 * 7
subs_tbl$meter_reading[1:1000] %>%
    slide_dfr(
        .f = ~ tsfeatures(.x, features = c(
            "acf_features",
            "crossing_points",
            "entropy",
            "stl_features",
            "lumpiness",
            # "max_kl_shift",
            # "max_level_shift",
            # "max_var_shift",
            "flat_spots",
            "pacf_features",
            "std1st_der",
            "outlierinclude_mdrmd",
            "heterogeneity",
            "hw_parameters",
            "nonlinearity"
        )),
        .before = window_size,
        .after = 0L,
        .step = 1L,
        parallel = TRUE,
        .complete = TRUE
    ) -> out
out

subs_tbl %>%
    head(1000) %>%
    tail(nrow(out)) %>%
    bind_cols(out) -> result

result %>%
    select(anomaly, timestamp, x_acf1:nonlinearity) %>%
    pivot_longer(-timestamp:-anomaly) %>%
    xyplot(value ~ timestamp | name, groups = anomaly, data = ., type = "p", scales = "free")

plot_out <- function(p, name) {
    png(sprintf("img/%s.png", name), 800, 800)
    print(p)
    dev.off()
}

result %>%
    select(anomaly, timestamp, meter_reading, trend:diff2_acf10) %>%
    pivot_longer(-timestamp:-anomaly) %>%
    filter(name %in% c("diff1_acf1")) %>%
    xyplot(value ~ timestamp | name, groups = anomaly, data = ., type = "p", scales = "free")