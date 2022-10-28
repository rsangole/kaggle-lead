analysis <- list(
    tar_target(
        high_corr_columns,
        recipes::recipe(meter_reading ~ ., 
                        data = train_features) |>
            recipes::step_corr(
                recipes::all_numeric_predictors(), 
                threshold = 0.90) |> 
            recipes::prep() |> 
            recipes::tidy(1)
    ),
    tar_target(
        cors,
        cor(train_features[,.SD,.SDcols = is.numeric])
    )
)


# corrplot::corrplot(tar_read(cors),
#                    method = "square",
#                    type = "lower",
#                    hclust.method = "ward",
#                    tl.col = 'gray20')
