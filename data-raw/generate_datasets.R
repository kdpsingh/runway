## code to prepare `single_model_dataset` dataset goes here

set.seed(1)

single_model_dataset =
  data.frame(
    outcomes = c(sample(c(0, 0, 1), 500, replace = TRUE),
                sample(c(1, 1, 0), 500, replace = TRUE)),
    predictions = c(sample(0:60/100, 500, replace = TRUE),
                    sample(40:100/100, 500, replace = TRUE)),
    stringsAsFactors = FALSE
  )

usethis::use_data(single_model_dataset, overwrite = TRUE)

set.seed(2)

multi_model_dataset =
  data.frame(
    outcomes = c(sample(c(0, 0, 1), 500, replace = TRUE),
                sample(c(1, 1, 0), 500, replace = TRUE)),
    predictions = c(sample(0:60/100, 500, replace = TRUE),
                    sample(40:100/100, 500, replace = TRUE)),
    model_name = c(sample(c('Model 1', 'Model 1', 'Model 2'), 500, replace = TRUE),
                   sample(c('Model 2', 'Model 2', 'Model 1'), 500, replace = TRUE)),
    stringsAsFactors = FALSE
  )

usethis::use_data(multi_model_dataset, overwrite = TRUE)
