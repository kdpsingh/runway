#' Calculates threshold-performance data
#'
#' This function is used by the \code{\link{threshperf_plot}} function. This is based on
#' the `threshold_perf()` function from the `probably` package (a part of `tidymodels`)
#' but expands the capability to allow for the range of metrics to include positive and
#' negative predictive value.

#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column containing
#' the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column containing
#' the predictions.
#' @return A data.frame containing the columns \code{.threshold}, \code{.metric},
#' \code{.estimator}, and \code{.estimate}
#' @examples
#' threshperf(single_model_dataset, outcome = 'outcomes', prediction = 'predictions')
#' @export
threshperf <- function(df, outcome, prediction) {

  thresholds = unique(c(0,sort(unique(df$prediction)), 1))

  df <- dplyr::select(df, dplyr::all_of(c(outcome, prediction)))
  df[[outcome]] <- factor(df[[outcome]], levels = c(0,1))
  df <- na.omit(df)

  df <-
    df %>%
    expand_preds(threshold = thresholds,
                 inc = c(outcome, prediction)) %>%
    mutate(alt_pred = recode_data(df[[outcome]], df[[prediction]], .threshold))


  df <- df %>% group_by(.threshold)

  df_metrics <- df %>%
    two_class(truth = get(outcome), estimate = alt_pred)

  sens_vec <- df_metrics %>%
    dplyr::filter(.metric == "sens") %>%
    dplyr::pull(.estimate)

  dist <- df_metrics %>%
    dplyr::filter(.metric == "spec") %>%
    dplyr::mutate(.metric = "distance",
                  .estimate = (1 - sens_vec)^2 + (1 - .estimate)^2)
  df_metrics <- dplyr::bind_rows(df_metrics, dist)
  as.data.frame(df_metrics)
}

#' Generate a threshold-performance plot for a single model
#'
#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column containing
#' the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column containing
#' the predictions.
#' @param plot_title A character string containing the title for the resulting plot.
#' @return A ggplot containing the threshold-performance plot
#' @examples
#' threshperf_plot(single_model_dataset, outcome = 'outcomes', prediction = 'predictions')
#' @export
threshperf_plot <- function(df, outcome, prediction, plot_title = '') {
  tp_data = threshperf(df, outcome, prediction)
  tp_plot =
    tp_data %>%
    dplyr::filter(!.metric %in% c('distance', 'j_index', 'bal_accuracy', 'accuracy')) %>%
    dplyr::mutate(.metric = case_when(.metric == 'accuracy' ~ 'Accuracy',
                               .metric == 'npv' ~ 'NPV',
                               .metric == 'ppv' ~ 'PPV',
                               .metric == 'spec' ~ 'Specificity',
                               .metric == 'sens' ~ 'Sensitivity')) %>%
    dplyr::mutate(.metric = factor(.metric, levels = c('Sensitivity', 'Specificity', 'PPV', 'NPV'))) %>%
    dplyr::mutate(.estimate = .estimate * 100) %>%
    ggplot2::ggplot(ggplot2::aes(x = .threshold, y = .estimate)) +
    ggplot2::geom_line(size=1) +
    ggplot2::facet_grid(.metric~.) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Threshold', y = 'Performance (%)') +
    ggplot2::ggtitle(plot_title)

  threshold_dist_plot =
    df %>%
    ggplot2::ggplot(ggplot2::aes(x=get(prediction))) +
    ggplot2::geom_histogram(fill = 'black', bins = 100) +
    ggplot2::coord_cartesian(xlim=c(0,1)) +
    ggplot2::theme_void()

  patchwork::plot_spacer() +
    (tp_plot / threshold_dist_plot + patchwork::plot_layout(heights = c(10,1))) +
    patchwork::plot_spacer() +
   patchwork::plot_layout(widths = c(1,2,1))
}
