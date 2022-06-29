library(data.table)
library(arrow)
library(lattice)
library(dplyr)
library(plotly)
ds <- open_dataset("data/train")

pca_tbl <- ds %>%
        # %>%
        select(-year) %>%
        select(-contains("gte")) %>%
        # select(building_id, site_id, anomaly, primary_use) %>%
        collect() %>%
        mutate(
                building_id = sprintf("Bld-%d", building_id),
                site_id = sprintf("Site-%d", site_id),
                anomaly = sprintf("A%d", anomaly)
        )
pca_tbl

scaled_tbl <- ds %>%
        select(-building_id, -anomaly, -site_id) %>%
        select(-year) %>%
        select(-contains("gte")) %>%
        collect() %>%
        select_if(is.numeric) %>%
        purrr::map_df(~ scale(.x))
names(scaled_tbl)

round(colMeans(scaled_tbl), 2)
sapply(scaled_tbl, sd)

data.table::setDT(scaled_tbl)

scaled_tbl[, lapply(.SD, function(x) {
        sum(is.na(x))
})]
scaled_tbl[, lapply(.SD, function(x) {
        sum(is.infinite(x))
})]

prout <- prcomp(scaled_tbl,
        scale. = F,
        center = F
)
prout

# Var Explained
pr.var <- data.table(
        comp = 1:length(prout$sdev),
        var_expl = prout$sdev^2
) %>%
        .[, var_prop := var_expl / sum(var_expl)] %>%
        .[, csum_var_prop := cumsum(var_prop)]

xyplot(var_prop + csum_var_prop ~ comp, pr.var,
        type = "b",
        auto.key = TRUE,
        panel = function(x, y, ...) {
                panel.xyplot(x, y, ...)
                panel.abline(h = 0.8, col = "red", lty = 2)
        }
)


E <- prout$rotation
E_dat <- data.table(E) %>%
        .[, var := stringr::str_replace(rownames(prout$rotation), ".V1", "")]
E_dat[]

G <- prout$x
G_dat <- data.table(G)
G_dat <- cbind(G_dat, pca_tbl)
G_dat[]



barchart(var ~ PC1 + PC2, E_dat,
        origin = 0,
        xlab = "Components",
        auto.key = list(space = "right"),
        par.settings = list(superpose.polygon = list(col = c("#3588c4", "#a82727c6")))
)
barchart(var ~ PC2 + PC3, E_dat,
        origin = 0,
        xlab = "Components",
        auto.key = list(space = "right"),
        par.settings = list(superpose.polygon = list(col = c("#3588c4", "#a82727c6")))
)


plot_varplot <- function(G_dat, E_dat, .x, .y, sample_fr, .color_by, .primary_use) {
        plot_dat <- G_dat %>%
                dplyr::filter(primary_use == .primary_use) %>%
                sample_frac(sample_fr)

        plot_ly() %>%
                add_segments(
                        x = 0,
                        y = 0,
                        xend = ~ get(.x) * 100 / 2,
                        yend = ~ get(.y) * 100 / 2,
                        data = E_dat,
                        inherit = FALSE,
                        hoverinfo = "text",
                        opacity = 0.20,
                        hovertemplate = ~ glue::glue(
                                "{var}"
                        )
                ) %>%
                add_markers(
                        data = plot_dat,
                        x = ~ get(.x),
                        y = ~ get(.y),
                        color = ~ get(.color_by),
                        hoverinfo = "text",
                        hovertemplate = ~ glue::glue(
                                "building_id : {building_id}
                              site_id:{
                                      site_id
                              }
                              anomaly:{
                                      anomaly
                              }
                              primary_use:{
                                      primary_use
                              }
                              wind_speed:{
                                      wind_speed
                              }
                              cloud_coverage:{
                                      cloud_coverage
                              }
                              floor_count:{
                                      floor_count
                              }
                              sea_level_pressure:{
                                      sea_level_pressure
                              }
                              air_temperature_min_lag7:{air_temperature_min_lag7}
                              "
                        )
                ) %>%
                add_text(
                        x = ~ get(.x) * 100 / 2,
                        y = ~ get(.y) * 100 / 2,
                        text = ~var,
                        data = E_dat,
                        inherit = FALSE
                ) %>%
                layout(title = list(text = .primary_use))
}

# plot_varplot(G_dat, E_dat, "PC1", "PC2", 0.005, "anomaly", "education")
plot_varplot(G_dat, E_dat, "PC1", "PC2", 0.005, "site_id", "education")
plot_varplot(G_dat, E_dat, "PC1", "PC2", 0.005, "site_id", "healthcare")
# plot_varplot(G_dat, E_dat, "PC1", "PC2", 0.005, "anomaly", "healthcare")
# plot_varplot(G_dat, E_dat, "PC1", "PC2", 0.01, "anomaly", "parking")
# xyplot(
#         PC1 ~ PC2,
#         G_dat,
#         pch = "."
# )



plot_varplot <- function(G_dat, E_dat, .x, .y, sample_N, .color_by) {
        plot_dat <- G_dat %>%
                group_by(site_id) %>%
                sample_n(sample_N)

        plot_ly() %>%
                add_segments(
                        x = 0,
                        y = 0,
                        xend = ~ get(.x) * 100 / 2,
                        yend = ~ get(.y) * 100 / 2,
                        data = E_dat,
                        inherit = FALSE,
                        hoverinfo = "text",
                        opacity = 0.20,
                        hovertemplate = ~ glue::glue(
                                "{var}"
                        )
                ) %>%
                add_markers(
                        data = plot_dat,
                        x = ~ get(.x),
                        y = ~ get(.y),
                        color = ~ get(.color_by),
                        hoverinfo = "text",
                        hovertemplate = ~ glue::glue(
                                "building_id : {building_id}
                              site_id:{
                                      site_id
                              }
                              anomaly:{
                                      anomaly
                              }
                              primary_use:{
                                      primary_use
                              }
                              wind_speed:{
                                      wind_speed
                              }
                              cloud_coverage:{
                                      cloud_coverage
                              }
                              floor_count:{
                                      floor_count
                              }
                              sea_level_pressure:{
                                      sea_level_pressure
                              }
                              air_temperature_min_lag7:{air_temperature_min_lag7}
                              "
                        )
                ) %>%
                add_text(
                        x = ~ get(.x) * 100 / 2,
                        y = ~ get(.y) * 100 / 2,
                        text = ~var,
                        data = E_dat,
                        inherit = FALSE
                )
        # layout(title = list(text = .primary_use))
}
plot_varplot(G_dat, E_dat, "PC1", "PC2", 55, "site_id")


plot_ly(
        x = ~wind_speed,
        y = ~dew_temperature,
        dat = G_dat %>% group_by(site_id) %>% sample_n(100),
        color = ~primary_use
) %>%
        add_markers()