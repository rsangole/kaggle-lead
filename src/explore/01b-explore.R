library(data.table, help, pos = 2, lib.loc = NULL)
# ---
# Investigate Test & Train Sets Further

# * combine into one
dat <- open_dataset("data/arrow-stratifiedsampling/") |> 
    dplyr::collect() |> 
    data.table::as.data.table()

dat[]

# Investigate
# * For each Building Id
# * Does it exist in both sets
x <- dat[, .N, .(building_id, label)] |>
        dcast(building_id ~ label, value.var = "N")
x[, inboth := !is.na(test) & !is.na(train)]
x[, .N, inboth]

# * For each Building Id + Site Id combination
# * Does it exist in both sets
x <- dat[, .N, .(building_id, site_id, label)] |>
        dcast(building_id + site_id ~ label, value.var = "N")
x[, inboth := !is.na(test) & !is.na(train)]
x[inboth == F]
x

x <- dat[, .N, .(site_id, label)] |>
        dcast(site_id ~ label, value.var = "N")
x[, inboth := !is.na(test) & !is.na(train)]
x[inboth == F]

# ** Takeaways
# ** Sites are common between train & test, except 2
# ** None of the buildings are common between train & test
