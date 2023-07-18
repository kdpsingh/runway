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
    ggplot2::coord_equal() +
    ggplot2::ggtitle(plot_title) + 
    ggplot2::xlab("Specificity") + 
    ggplot2::ylab("Sensitivity")

  if(ci){
    g2 = g1 + ggplot2::geom_ribbon(
      data = dat.ci,
      ggplot2::aes(x = x, ymin = lower, ymax = upper),
      # fill = "steelblue",
      alpha = 0.2
    )
  } else g2 = g1
  g2
}

#' Generate an ROC curve plot with error bars showing 95 percent
#' confidence intervals
#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column
#'   containing the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column
#'   containing the predictions.
#' @param model A character string containing the name of the column
#'   containing the model label.
#' @param ci Show confidence interval ribbon.
#'   Defaults to FALSE.
#' @param plot_title A character string containing the title for the resulting
#'   plot.
#' @return A ggplot containing the ROC plot
#' @examples
#' data(multi_model_dataset)
#' roc_plot_multi(multi_model_dataset, outcome = 'outcomes', prediction = 'predictions', model = 'model_name', ci = TRUE)
#' @export
roc_plot_multi <- function(df, outcome, prediction, model, ci = FALSE, plot_title = '') {

  how_many_models = df[[model]] %>% unique() %>% length()

  ci_data = df %>%
    dplyr::group_by(!!rlang::parse_expr(model)) %>%
    dplyr::group_nest() %>%
    dplyr::mutate(roc = purrr::map(data, ~ pROC::roc_(data = ., response = outcome, predictor = prediction, ci = ci, plot=FALSE)),
                  roc = roc %>% setNames(!!rlang::parse_expr(model)),
                  ci_spec = purrr::map(roc, ~ pROC::ci.se(., specificities = seq(0, 1, l = 25))),
                  ci_ribbon = purrr::map(ci_spec, ~ build_ci_data(.)))

  # build ROC curves
  g1 = pROC::ggroc(ci_data$roc) +
    ggplot2::theme_minimal() +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 1,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) +
    ggplot2::coord_equal() +
    ggplot2::scale_color_brewer(name = 'Models', palette = 'Set1') +
    ggplot2::scale_fill_brewer(name = 'Models', palette = 'Set1') +
    ggplot2::ggtitle(plot_title) + 
    ggplot2::xlab("Specificity") + 
    ggplot2::ylab("Sensitivity")

  # build CI intervals
  if(ci){
    ribbon = ci_data %>%
      dplyr::select(!!rlang::parse_expr(model), ci_ribbon) %>%
      dplyr::rename(name = !!rlang::parse_expr(model)) %>%
      tidyr::unnest_wider(ci_ribbon) %>%
      tidyr::unnest(cols = c(x, lower, upper))

    # ci_values = ci_data %>%
    #   dplyr::select(model_name, roc) %>%
    #   dplyr::mutate(ci = purrr::map(roc, ~ .$ci)) %>%
    #   dplyr::select(-roc)

    ci_values = ci_data %>%
      dplyr::select(!!rlang::parse_expr(model), roc) %>%
      dplyr::mutate(ci = purrr::map(roc, ~ pROC::ci(.)))

    g2 = g1 + ggplot2::geom_ribbon(data = ribbon,
                                   ggplot2::aes(fill = name, x = x, ymin = lower, ymax = upper),
                                   alpha = 1/how_many_models,
                                   inherit.aes = FALSE)
  } else g2 = g1
  g2
}

build_ci_data = function(obj){
  data.frame(x = as.numeric(rownames(obj)),
             lower = obj[, 1],
             upper = obj[, 3])
}
