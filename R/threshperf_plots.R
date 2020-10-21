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
#' data(single_model_dataset)
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
                 inc = c(outcome, prediction))

  df <-
    df %>%
    dplyr::mutate(alt_pred = recode_data(df[[outcome]], df[[prediction]], .threshold))

  df <- df %>% dplyr::group_by(.threshold)

  df_metrics <- df %>%
    two_class(truth = get(outcome), estimate = alt_pred)

  suppressWarnings({df_metrics <-
    df_metrics %>%
    dplyr::group_by(.threshold) %>%
    dplyr::mutate(denom =
             dplyr::case_when(
               .metric == 'sens' ~ sum(df_orig[[outcome]] == 1),
               .metric == 'spec' ~ sum(df_orig[[outcome]] == 0),
               .metric == 'ppv' ~ sum(df_orig[[prediction]] >= .threshold),
               .metric == 'npv' ~ sum(df_orig[[prediction]] < .threshold),
             )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(numer = round(.estimate * denom)) %>%
    na.omit()})

  df_ci = Hmisc::binconf(x = df_metrics$numer, n = df_metrics$denom,
                         alpha = 0.05, method = 'wilson') %>%
    dplyr::as_tibble() %>%
    dplyr::rename(ll = Lower, ul = Upper) %>%
    dplyr::mutate_at(dplyr::vars(ul, ll), . %>% scales::oob_squish(range = c(0,1)))

    df_metrics = dplyr::bind_cols(df_metrics, df_ci)

  data.frame(df_metrics, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Generate a threshold-performance plot for a single model
#'
#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column
#'   containing the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column
#'   containing the predictions.
#' @param show_denom Show the denominator (as a fraction of the maximum
#'   positives/negatives) for the positive predictive value (PPV) and negative
#'   predictive value (NPV).
#' @param plot_title A character string containing the title for the resulting
#'   plot.
#' @param xmin The minimum possible prediction. Defaults to 0.
#' @param xmax The maximum possible prediction. Defaults to 1.
#' @param pre_tp_geoms A ggplot geom_* (or a list of geoms) that should be drawn
#'   on the threshold-performance plot prior to rendering it.
#' @param pre_dist_geoms A ggplot geom_* (or a list of geoms) that should be
#'   drawn on the distribution plot prior to rendering it.
#' @param post_tp_geoms A ggplot geom_* (or a list of geoms) that should be
#'   drawn on the threshold-performance plot after rendering it.
#' @param post_dist_geoms A ggplot geom_* (or a list of geoms) that should be
#'   drawn on the distribution plot after rendering it.
#' @param heights A numeric vector of length 2 with ratio of heights of plots.
#'   Defaults to c(10, 1).
#' @param widths A numeric vector of length 3 with ratio of widths of plots.
#'   The first and third elements refer to padding. Defaults to c(1, 2, 1).
#' @return A ggplot containing the threshold-performance plot. The 95 percent
#'   confidence intervals are estimated using Wilson's interval from the
#'   \code{Hmisc} \code{\link[Hmisc]{binconf}} function.
#' @examples
#' data(single_model_dataset)
#' threshperf_plot(single_model_dataset, outcome = 'outcomes', prediction = 'predictions')
#' @export
threshperf_plot <- function(df, outcome, prediction, show_denom = TRUE, plot_title = '',
                            xmin = 0,
                            xmax = 1,
                            pre_tp_geoms = NULL,
                            pre_dist_geoms = NULL,
                            post_tp_geoms = NULL,
                            post_dist_geoms = NULL,
                            heights = c(10,1),
                            widths = c(1,2,1)) {
  tp_data = threshperf(df, outcome, prediction)

    tp_data =
    tp_data %>%
    dplyr::group_by(.metric) %>%
    dplyr::mutate(denom_frac =
                    dplyr::if_else(.metric %in% c('ppv', 'npv'),
                            denom/max(denom), NA_real_, NA_real_)) %>%
    dplyr::ungroup()

  tp_plot =
    tp_data %>%
    dplyr::mutate(.metric = dplyr::case_when(
                               .metric == 'npv' ~ 'NPV',
                               .metric == 'ppv' ~ 'PPV',
                               .metric == 'spec' ~ 'Specificity',
                               .metric == 'sens' ~ 'Sensitivity')) %>%
    dplyr::mutate(.metric = factor(.metric, levels = c('Sensitivity', 'Specificity', 'PPV', 'NPV'))) %>%
    dplyr::mutate_at(dplyr::vars(.estimate, ll, ul), . %>% {. * 100}) %>%
    ggplot2::ggplot(ggplot2::aes(x = .threshold,
                                 y = .estimate,
                                 ymin = ll,
                                 ymax = ul))

  if (!is.null(pre_tp_geoms)) {
    tp_plot =
      tp_plot +
      pre_tp_geoms
  }

  if (show_denom) {
    tp_plot =
      tp_plot +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = denom_frac*100), fill = 'grey', alpha = 1/3)
  }

  tp_plot = tp_plot +
    ggplot2::geom_ribbon(fill = 'grey') +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_grid(.metric~.) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim=c(xmin,xmax)) +
    ggplot2::labs(x = 'Threshold', y = 'Performance (%)') +
    ggplot2::ggtitle(plot_title)

  if (!is.null(post_tp_geoms)) {
    tp_plot =
      tp_plot +
      post_tp_geoms
  }

  threshold_dist_plot =
    df %>%
    ggplot2::ggplot(ggplot2::aes(x=get(prediction)))

  if (!is.null(pre_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      pre_dist_geoms
  }

  threshold_dist_plot =
    threshold_dist_plot +
    ggplot2::geom_histogram(fill = 'black', bins = 100) +
    ggplot2::coord_cartesian(xlim=c(xmin,xmax)) +
    ggplot2::theme_void()

  if (!is.null(post_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      post_dist_geoms
  }

  patchwork::plot_spacer() +
    (tp_plot / threshold_dist_plot + patchwork::plot_layout(heights = heights)) +
    patchwork::plot_spacer() +
   patchwork::plot_layout(widths = widths)
}

#' Generate a threshold-performance plot for multiple models with colored/shaded
#' 95 percent confidence intervals
#'
#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column
#'   containing the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column
#'   containing the predictions.
#' @param model A character string containing the name of the column containing
#'   the model names
#' @param plot_title A character string containing the title for the resulting
#'   plot.
#' @param xmin The minimum possible prediction. Defaults to 0.
#' @param xmax The maximum possible prediction. Defaults to 1.
#' @param pre_tp_geoms A ggplot geom_* (or a list of geoms) that should be drawn
#'   on the threshold-performance plot prior to rendering it.
#' @param pre_dist_geoms A ggplot geom_* (or a list of geoms) that should be
#'   drawn on the distribution plot prior to rendering it.
#' @param post_tp_geoms A ggplot geom_* (or a list of geoms) that should be
#'   drawn on the threshold-performance plot after rendering it.
#' @param post_dist_geoms A ggplot geom_* (or a list of geoms) that should be
#'   drawn on the distribution plot after rendering it.
#' @param heights A numeric vector of length 2 with ratio of heights of plots.
#'   Defaults to c(10, 1).
#' @param widths A numeric vector of length 3 with ratio of widths of plots.
#'   The first and third elements refer to padding. Defaults to c(1, 2, 1).
#' @return A ggplot containing the threshold-performance plot. The 95 percent
#'   confidence intervals are estimated using Wilson's interval from the
#'   \code{Hmisc} \code{\link[Hmisc]{binconf}} function.
#' @examples
#' data(multi_model_dataset)
#' threshperf_plot_multi(multi_model_dataset, outcome = 'outcomes', prediction = 'predictions', model = 'model_name')
#' @export
threshperf_plot_multi <- function(df, outcome, prediction, model, plot_title = '',
                                  xmin = 0,
                                  xmax = 1,
                                  pre_tp_geoms = NULL,
                                  pre_dist_geoms = NULL,
                                  post_tp_geoms = NULL,
                                  post_dist_geoms = NULL,
                                  heights = c(10,1),
                                  widths = c(1,2,1)) {

  how_many_models = df[[model]] %>% unique() %>% length()

  tp_data_list = list()
  for (model_name in unique(df[[model]])) {
    tp_data_list[[model_name]] <-
      threshperf(df[df[[model]] == model_name,],
                 outcome,
                 prediction)
    tp_data_list[[model_name]][[model]] <- model_name
  }

  tp_data = dplyr::bind_rows(tp_data_list)

  tp_plot =
    tp_data %>%
    dplyr::mutate(.metric = dplyr::case_when(
      .metric == 'npv' ~ 'NPV',
      .metric == 'ppv' ~ 'PPV',
      .metric == 'spec' ~ 'Specificity',
      .metric == 'sens' ~ 'Sensitivity')) %>%
    dplyr::mutate(.metric = factor(.metric, levels = c('Sensitivity', 'Specificity', 'PPV', 'NPV'))) %>%
    dplyr::mutate_at(dplyr::vars(.estimate, ll, ul), . %>% {. * 100}) %>%
    ggplot2::ggplot(ggplot2::aes(x = .threshold,
                                 y = .estimate,
                                 ymin = ll,
                                 ymax = ul,
                                 color = get(model),
                                 fill = get(model)))


  if (!is.null(pre_tp_geoms)) {
    tp_plot =
      tp_plot +
      pre_tp_geoms
  }

  tp_plot = tp_plot +
    ggplot2::geom_ribbon(alpha = 1/how_many_models) +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_grid(.metric~.) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim=c(xmin,xmax)) +
    ggplot2::labs(x = 'Threshold', y = 'Performance (%)') +
    ggplot2::scale_color_brewer(name = 'Models', palette = 'Set1') +
    ggplot2::scale_fill_brewer(name = 'Models', palette = 'Set1') +
    ggplot2::ggtitle(plot_title)

  if (!is.null(post_tp_geoms)) {
    tp_plot =
      tp_plot +
      post_tp_geoms
  }

  threshold_dist_plot <- ggplot2::ggplot(df, ggplot2::aes(x = get(prediction)))

  if (!is.null(pre_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      pre_dist_geoms
  }

  threshold_dist_plot =
    threshold_dist_plot +
    ggplot2::geom_density(alpha = 1/how_many_models, ggplot2::aes(fill = get(model), color = get(model))) +
    ggplot2::scale_x_continuous(limits = c(xmin, xmax), breaks = seq(xmin, xmax, by = (xmax-xmin)/10)) +
    # scale_color_viridis(discrete = TRUE, option = 'cividis', begin = 0.5) +
    # scale_fill_viridis(discrete = TRUE, option = 'cividis', begin = 0.5) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_color_brewer(palette = 'Set1') +
    ggplot2::scale_fill_brewer(palette = 'Set1') +
    ggplot2::theme_minimal() +
    ggeasy::easy_remove_y_axis() +
    #  easy_remove_x_axis(what = c('ticks','line')) +
    ggeasy::easy_remove_legend(fill, color) +
    ggplot2::theme_void()

  if (!is.null(post_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      post_dist_geoms
  }


  patchwork::plot_spacer() +
    (tp_plot / threshold_dist_plot + patchwork::plot_layout(heights = heights)) +
    patchwork::plot_spacer() +
    patchwork::plot_layout(widths = widths)
}
