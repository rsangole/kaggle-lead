library(data.table)
library(arrow)
library(lattice)
library(dplyr)
library(plotly)
library(cluster)
library(factoextra)
ds <- open_dataset("data/train")

clust_tbl <- ds %>%
    select(-year) %>%
    select(-contains("gte")) %>%
    select(-anomaly, -meter_reading, -timestamp, -building_meter) %>%
    select(-contains("hour"), -contains("month"), -contains("weekday")) %>%
    collect() %>%
    tidyr::drop_na() %>%
    mutate(
        building_id = sprintf("Bld-%d", building_id),
        site_id = sprintf("Site-%d", site_id)
    )
clust_tbl
clust_tbl %>% skimr::skim()

clust_scaled_tbl <- clust_tbl %>%
    select(-rowname) %>%
    group_by(site_id) %>%
    sample_n(200) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), scale))
clust_scaled_tbl
clust_scaled_tbl %>% skimr::skim()

# Correlation Plot
cor_mat <- clust_scaled_tbl %>%
    select(where(is.numeric)) %>%
    cor()
png("img/corr_plot.png", 800, 800)
corrplot::corrplot(cor_mat)
dev.off()

png("img/yr_sqft_plot.png", 800, 800)
xyplot(year_built ~ square_feet, clust_tbl)
dev.off()

# Row Distances
clust_scaled_tbl %>%
    select(
        square_feet, floor_count, cloud_coverage, air_temperature,
        precip_depth_1_hr,
        wind_speed,
        wind_direction,
        is_holiday,
        dew_temperature
    ) %>%
    get_dist() -> distance
distance %>% str()

png("img/dist_plot.png", 800, 800)
fviz_dist(distance,
    show_labels = FALSE,
    gradient = list(
        low = "#00AFBB",
        mid = "white",
        high = "#FC4E07"
    )
)
dev.off()


# K Means

plot_k <- function(dat, K, nstart = 50) {
    png(sprintf("img/K%d.png", K), 800, 800)
    dat %>%
        kmeans(centers = K, nstart = nstart) %>%
        fviz_cluster(dat) %>%
        print()
    dev.off()
}
clust_scaled_tbl %>%
    select(where(is.numeric)) %>%
    plot_k(3, 100)