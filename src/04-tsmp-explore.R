library(tsmp)
library(dplyr)
library(arrow)
library(ggplot2)
library(lattice)
library(plotly)

ds <- open_dataset("data/train")

main_tbl <- ds %>%
    select(-year) %>%
    select(-contains("gte")) %>%
    filter(primary_use == "services") %>%
    collect() %>%
    mutate(
        building_id = sprintf("Bld-%d", building_id),
        site_id = sprintf("Site-%d", site_id),
        anomaly = as.factor(sprintf("A%d", anomaly))
    ) %>%
    arrange(timestamp)
main_tbl

xyplot(
    meter_reading ~ timestamp | building_id,
    groups = anomaly,
    main_tbl,
    auto.key = TRUE,
    pch = "."
)