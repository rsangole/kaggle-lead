library(arrow)
library(dplyr)
library(janitor)
library(ggplot2)
library(plotly)
library(lattice)
# library(DataExplorer)

ds <- open_dataset("data/arrow")
ds

# introduce(train)
# glimpse(train)
# skimr::skim(train)

# Anomaly %
ds |>
        count(anomaly) |>
        collect() |>
        adorn_totals() |>
        adorn_percentages("col") |>
        adorn_pct_formatting()

# Anomaly by Use
ds |>
        select(anomaly, primary_use) |>
        collect() |>
        tabyl(anomaly, primary_use) |>
        adorn_totals()
ds |>
        select(anomaly, primary_use) |>
        collect() |>
        tabyl(anomaly, primary_use) |>
        adorn_totals() |>
        adorn_percentages("col") |>
        adorn_pct_formatting()


train <- train %>%
        mutate(
                anomaly     = as.logical(anomaly),
                building_id = as.numeric(as.factor(building_id))
        )

train %>%
        filter(
                building_id == 5,
                !is.na(meter_reading)
        ) %>%
        ggplot(aes(timestamp, meter_reading)) +
        geom_point(aes(color = anomaly))

# 	-> p
# ggplotly(p)

train %>%
        group_by(primary_use) %>%
        summarize(n = length(unique(building_id))) %>%
        arrange(-n)

sample_tbl <- train %>%
        select(building_id, primary_use) %>%
        distinct() %>%
        group_by(primary_use) %>%
        sample_n(5, weight = building_id, replace = TRUE) %>%
        distinct()
sample_tbl



ds %>%
        filter(
                building_id %in% sample_tbl$building_id,
                !is.na(meter_reading)
        ) %>%
        select(timestamp, primary_use, building_id, meter_reading, anomaly) %>%
        collect() %>%
        ggplot(aes(timestamp, meter_reading)) +
        geom_point(aes(color = as.factor(anomaly))) +
        facet_wrap(~primary_use)


plotdat <- ds %>%
        filter(
                building_id %in% sample_tbl$building_id,
                !is.na(meter_reading)
        ) %>%
        select(timestamp, primary_use, building_id, meter_reading, anomaly) %>%
        collect()

plotdat

xyplot(meter_reading ~ timestamp | building_id, groups = primary_use, plotdat %>% filter(building_id == 1247))

xyplot(meter_reading ~ timestamp | primary_use, groups = anomaly, plotdat, auto.key = TRUE, pch = 16, scales = "free")

xyplot(meter_reading ~ timestamp | building_id,
        groups = anomaly,
        plotdat %>% filter(primary_use == "Office") %>% filter(timestamp < "2016-03-01"),
        auto.key = TRUE,
        pch = 16,
        type = "o",
        scales = "free"
)

xyplot(meter_reading ~ timestamp | building_id,
        groups = anomaly,
        plotdat %>% filter(primary_use == "Office") %>% filter(timestamp > "2016-03-01", timestamp < "2016-04-01"),
        auto.key = TRUE,
        pch = 16,
        type = "o",
        scales = "free"
)


xyplot(meter_reading ~ timestamp | building_id,
        groups = anomaly,
        plotdat %>% filter(primary_use == "Education") %>% filter(timestamp > "2016-03-01", timestamp < "2016-04-01"),
        auto.key = TRUE,
        pch = 16,
        type = "o",
        scales = "free"
)

xyplot(meter_reading ~ timestamp | facet,
        groups = anomaly,
        train %>%
                filter(primary_use == "Entertainment/public assembly") %>%
                filter(timestamp > "2016-03-01", timestamp < "2016-04-01") %>%
                mutate(facet = paste0("B", building_id, " S", site_id)),
        auto.key = TRUE,
        pch = 16,
        type = "o",
        scales = "free"
)

train %>%
        filter(primary_use == "Entertainment/public assembly") %>%
        filter(timestamp > "2016-03-01", timestamp < "2016-04-01") %>%
        mutate(week = lubridate::week(timestamp)) %>%
        densityplot(~ meter_reading | building_id,
                data = ., scales = "free", groups = week,
                plot.points = FALSE
        )


train %>%
        filter(primary_use == "Entertainment/public assembly") %>%
        filter(timestamp > "2016-03-01", timestamp < "2016-04-01") %>%
        mutate(week = lubridate::week(timestamp)) %>%
        densityplot(~ meter_reading | building_id,
                data = ., scales = "free", groups = week,
                plot.points = FALSE
        )

train %>%
        filter(primary_use == "Entertainment/public assembly") %>%
        # filter(timestamp > "2016-03-01", timestamp < "2016-04-01") %>%
        mutate(month = lubridate::month(timestamp)) %>%
        densityplot(~ meter_reading | building_id,
                data = ., scales = "free", groups = month,
                plot.points = FALSE
        )
