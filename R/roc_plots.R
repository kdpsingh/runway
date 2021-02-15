#' Generate an ROC curve plot with error bars showing 95 percent
#' confidence intervals
#'
#' This code builds off of code written by Vincent Guillemot found here:
#' \url{https://rpubs.com/vguillem/465086}.
#'
#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column
#'   containing the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column
#'   containing the predictions.
#' @param ci Show confidence interval ribbon.
#'   Defaults to FALSE.
#' @param plot_title A character string containing the title for the resulting
#'   plot.
#' @return A ggplot containing the calibration plot
#' @examples
#' data(single_model_dataset)
#' roc_plot(single_model_dataset, outcome = 'outcomes', prediction = 'predictions', ci = TRUE)
#' @export
roc_plot <- function(df, outcome, prediction, ci = FALSE, plot_title = '') {
  obj <- pROC::roc_(df, response = outcome, predictor = prediction, ci = ci, plot=FALSE)
  ciobj <- pROC::ci.se(obj, specificities = seq(0, 1, l = 25))
  dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
                       lower = ciobj[, 1],
                       upper = ciobj[, 3])

  g1 = pROC::ggroc(obj) +
    ggplot2::theme_minimal() +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 1,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) +
    ggplot2::coord_equal()

  if(ci){
    g2 = g1 + ggplot2::geom_ribbon(
      data = dat.ci,
      ggplot2::aes(x = x, ymin = lower, ymax = upper),
      fill = "steelblue",
      alpha = 0.2
    ) + ggplot2::ggtitle(capture.output(obj$ci))
  } else g2 = g1 + ggplot2::ggtitle(plot_title)
  g2
}

ci_obj <- function(roc_obj){
  ciobj <- pROC::ci.se(obj, specificities = seq(0, 1, l = 25))
}
dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
                     lower = ciobj[, 1],
                     upper = ciobj[, 3],
                     model_name = model)



roc_plot_multi <- function(df, outcome, prediction, model, ci = FALSE, plot_title = '') {


  how_many_models = df[[model]] %>% unique() %>% length()

  roc_data_list = list()
  for (model_name in unique(df[[model]])) {
    tp_data_list[[model_name]] <-
      threshperf(df[df[[model]] == model_name,],
                 response = outcome,
                 predictor = prediction,
                 ci = ci)
    tp_data_list[[model_name]][[model]] <- model_name
  }

  roc_obj_list = list()
  for (model_name in unique(df[[model]])) {
    tp_data_list[[model_name]] <-
      threshperf(df[df[[model]] == model_name,],
                 ciobj <- pROC::ci.se(obj, specificities = seq(0, 1, l = 25))
    roc_obj_list[[model_name]][[model]] <- model_name
  }

  ci_obj_list = list()
  for (model_name in unique(df[[model]])) {
    tp_data_list[[model_name]] <-
      threshperf(df[df[[model]] == model_name,],
                 response = outcome,
                 predictor = prediction,
                 ci = ci)
    ci_obj_list[[model_name]][[model]] <- model_name
  }

  obj <- pROC::roc_(df, response = outcome, predictor = prediction, ci = ci, plot=FALSE)
  ciobj <- pROC::ci.se(obj, specificities = seq(0, 1, l = 25))
  dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
                       lower = ciobj[, 1],
                       upper = ciobj[, 3])

  g1 = pROC::ggroc(obj) +
    ggplot2::theme_minimal() +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 1,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) +
    ggplot2::coord_equal()

  if(ci){
    g2 = g1 + ggplot2::geom_ribbon(
      data = dat.ci,
      ggplot2::aes(x = x, ymin = lower, ymax = upper),
      fill = "steelblue",
      alpha = 0.2
    ) + ggplot2::ggtitle(capture.output(obj$ci))
  } else g2 = g1 + ggplot2::ggtitle(plot_title)
  g2
}

