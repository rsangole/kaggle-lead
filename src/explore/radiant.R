library(radiant)
library(data.table, help, pos = 2, lib.loc = NULL)
dat = arrow::open_dataset(here::here("data/arrow-stratifiedsampling/label=train/")) |> 
dplyr::collect() |> 
data.table::as.data.table()


test=arrow::open_dataset(here::here("data/arrow-stratifiedsampling/label=holdout/")) |> 
dplyr::collect() |> 
data.table::as.data.table()
radiant::radiant()
