test_that("threshperf() works", {
  expect_equal(
    single_model_dataset %>%
      threshperf(
        outcome = 'outcomes',
        prediction = 'predictions',
        thresholds = 0.5,
        statistics = c("sens", "spec", "ppv", "npv", "test_pos_rate", "test_neg_rate",
                       "tp_rate", "tn_rate", "fn_rate", "fp_rate", "prevalence")
      ) %>%
      dplyr::select(.metric, .estimate) %>%
      tibble::deframe(),
    c(sens = mean(single_model_dataset$predictions[single_model_dataset$outcomes == 1] >= 0.5),
      spec = mean(single_model_dataset$predictions[single_model_dataset$outcomes == 0] < 0.5),
      prevalence = mean(single_model_dataset$outcomes),
      tp_rate = mean(single_model_dataset$predictions >= 0.5 & single_model_dataset$outcomes == 1),
      fp_rate = mean(single_model_dataset$predictions >= 0.5 & single_model_dataset$outcomes == 0),
      tn_rate = mean(single_model_dataset$predictions < 0.5 & single_model_dataset$outcomes == 0),
      fn_rate = mean(single_model_dataset$predictions < 0.5 & single_model_dataset$outcomes == 1),
      test_pos_rate = mean(single_model_dataset$predictions >= 0.5),
      test_neg_rate = mean(single_model_dataset$predictions < 0.5),
      ppv = mean(single_model_dataset$outcomes[single_model_dataset$predictions >= 0.5]),
      npv = mean(!single_model_dataset$outcomes[single_model_dataset$predictions < 0.5])
    )
  )
})
