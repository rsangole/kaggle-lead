library(dplyr)
library(tidyr)
library(ggplot2)

beta_encoder_fit <- function(dat, grp, target) {
        out <- dat %>%
                group_by(.group = get(grp)) %>%
                summarise(sum = sum(get(target)), count = n()) %>%
                rename(!!grp := .group)

        list(
                prior_mean = mean(dat[[target]]),
                stats = out
        )
}

beta_encoder_transform <- function(dat, be, stat_type, N_min = 1) {
        out <- dat %>%
                left_join(be$stats) %>%
                mutate(
                        sum = case_when(
                                is.na(sum) ~ be$prior_mean,
                                TRUE ~ sum
                        ),
                        count = case_when(
                                is.na(count) ~ 1L,
                                TRUE ~ count
                        )
                )

        N_prior <- pmax(N_min - out$count, 0)

        alpha_prior <- be$prior_mean * N_prior
        beta_prior <- (1 - be$prior_mean) * N_prior

        alpha <- alpha_prior + out$sum
        beta <- beta_prior + out$count - out$sum

        if (stat_type == "mean") {
                num <- alpha
                dem <- alpha + beta
        }

        value <- num / dem
        value
}

mtc_tbl <- as_tibble(mtcars) %>%
        mutate(
                cyl = paste0("c", cyl),
                am = paste0("am", am),
                gear = paste0("gear", gear)
        )
mtc_tbl

NMIN <- 4

be <- beta_encoder_fit(mtc_tbl, "gear", "mpg")
mtc_tbl[["gear_mean"]] <- beta_encoder_transform(mtc_tbl, be, "mean", NMIN)
be <- beta_encoder_fit(mtc_tbl, "cyl", "mpg")
mtc_tbl[["cyl_mean"]] <- beta_encoder_transform(mtc_tbl, be, "mean", NMIN)
be <- beta_encoder_fit(mtc_tbl, "am", "mpg")
mtc_tbl[["am_mean"]] <- beta_encoder_transform(mtc_tbl, be, "mean", NMIN)
mtc_tbl

mtc_tbl %>%
        ggplot() +
        geom_point(aes(gear, mpg), alpha = 0.6) +
        geom_point(aes(gear, gear_mean), color = "red", size = 5) +
        theme_classic()

mtc_tbl %>%
        ggplot() +
        geom_point(aes(cyl, mpg), alpha = 0.6) +
        geom_point(aes(cyl, cyl_mean), color = "red", size = 5) +
        theme_classic()

mtc_tbl %>%
        ggplot() +
        geom_point(aes(am, mpg), alpha = 0.6) +
        geom_point(aes(am, am_mean), color = "red", size = 5) +
        theme_classic()
