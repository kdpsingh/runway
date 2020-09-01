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

  thresholds = unique(c(0,sort(unique(df[[prediction]])), 1))

  df <- dplyr::select(df, dplyr::all_of(c(outcome, prediction)))

  df <- na.omit(df)

  df_orig <- df

  # IMPORTANT because order of levels matters to yardstick
  if (getOption('yardstick.event_first', default = TRUE)) {
    df[[outcome]] <- factor(df[[outcome]], levels = c(1,0))
  } else {
    df[[outcome]] <- factor(df[[outcome]], levels = c(0,1))
  }


  df <-
    df %>%
    expand_preds(threshold = thresholds,
                 inc = c(outcome, prediction)) %>%
    dplyr::mutate(alt_pred = recode_data(df[[outcome]], df[[prediction]], .threshold))


  df <- df %>% dplyr::group_by(.threshold)

  df_metrics <- df %>%
    two_class(truth = get(outcome), estimate = alt_pred)

  df_metrics <-
    df_metrics %>%
    dplyr::group_by(.threshold) %>%
    dplyr::mutate(denom =
             case_when(
               .metric == 'sens' ~ sum(df_orig[[outcome]] == 1),
               .metric == 'spec' ~ sum(df_orig[[outcome]] == 0),
               .metric == 'ppv' ~ sum(df_orig[[prediction]] >= .threshold),
               .metric == 'npv' ~ sum(df_orig[[prediction]] < .threshold),
             )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(numer = round(.estimate * denom)) %>%
    na.omit()

  df_ci = Hmisc::binconf(x = df_metrics$numer, n = df_metrics$denom,
                         alpha = 0.05, method = 'wilson') %>%
    dplyr::as_tibble() %>%
    dplyr::rename(ll = Lower, ul = Upper) %>%
    dplyr::mutate_at(vars(ul, ll), . %>% scales::oob_squish(range = c(0,1)))

    df_metrics = bind_cols(df_metrics, df_ci)

  data.frame(df_metrics, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Generate a threshold-performance plot for a single model
#'
#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column containing
#' the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column containing
#' the predictions.
#' @param plot_title A character string containing the title for the resulting plot.
#' @return A ggplot containing the threshold-performance plot. The 95 percent
#' confidence intervals are estimated using Wilson's interval from the \code{Hmisc}
#' \code{\link[Hmisc]{binconf}} function.
#' @examples
#' threshperf_plot(single_model_dataset, outcome = 'outcomes', prediction = 'predictions')
#' @export
threshperf_plot <- function(df, outcome, prediction, plot_title = '') {
  tp_data = threshperf(df, outcome, prediction)
  tp_plot =
    tp_data %>%
    dplyr::mutate(.metric = case_when(
                               .metric == 'npv' ~ 'NPV',
                               .metric == 'ppv' ~ 'PPV',
                               .metric == 'spec' ~ 'Specificity',
                               .metric == 'sens' ~ 'Sensitivity')) %>%
    dplyr::mutate(.metric = factor(.metric, levels = c('Sensitivity', 'Specificity', 'PPV', 'NPV'))) %>%
    dplyr::mutate_at(vars(.estimate, ll, ul), . %>% {. * 100}) %>%
    ggplot2::ggplot(ggplot2::aes(x = .threshold,
                                 y = .estimate,
                                 ymin = ll,
                                 ymax = ul)) +
    ggplot2::geom_ribbon(fill = 'lightgrey') +
    ggplot2::geom_line(size = 1) +
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
