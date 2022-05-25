library(data.table)
library(arrow)
library(lattice)
library(dplyr)
library(plotly)
ds <- open_dataset("data/train")

pca_tbl <- ds %>%
        select(building_id, site_id, anomaly)

scaled_tbl <- ds %>%
        select(-building_id, -anomaly, -site_id) %>%
        select(-year) %>% 
        select(-contains('gte')) %>% 
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
        panel = function(x,y,...) {
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


plot_varplot <- function(G_dat, E_dat, .x, .y, .color) {
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
                        data = G_dat,
                        x = ~ get(.x),
                        y = ~ get(.y),
                        color = ~ get(.color),
                        hoverinfo = "text",
#                         hovertemplate = ~ glue::glue(
#                                 "genre_name : {genre_name}
#       country_of_origin : {country_of_origin}
#       group_name_adjusted : {group_name_adjusted}
#       content_provider_name : {content_provider_name}
#       inapp_billings : {scales::label_number_si(accuracy = 0.1)(inapp_billings)}
#       app_billings : {scales::label_number_si(accuracy = 0.1)(app_billings)}
#       subs_billings : {scales::label_number_si(accuracy = 0.1)(subs_billings)}
#       dld_uiredld_update_users : {scales::label_number_si(accuracy = 0.1)(dld_uiredld_update_users)}
#       downloads : {scales::label_number_si(accuracy = 0.1)(downloads)}
#       redownloads : {scales::label_number_si(accuracy = 0.1)(redownloads)}"
#                         )
                ) %>%
                add_text(
                        x = ~ get(.x) * 100 / 2,
                        y = ~ get(.y) * 100 / 2,
                        text = ~var,
                        data = E_dat,
                        inherit = FALSE
                )
}

plot_varplot(G_dat, E_dat, "PC1", "PC2", "is_holiday")

xyplot(
	PC1 ~ PC2,
	G_dat,
	pch = '.'
)
