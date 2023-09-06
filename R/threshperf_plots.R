#' Calculates threshold-performance data
#'
#' This function is used by the \code{\link{threshperf_plot}} function. This is based on
#' the `threshold_perf()` function from the `probably` package (a part of `tidymodels`)
#' but expands the capability to allow for the range of metrics to include positive and
#' negative predictive value.

#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column containing
#' the outcomes (expressed as 0/1s).
#' @param positive A character string containing the value of outcome that is the positive class.
#' @param prediction A character string containing the name of the column containing
#' the predictions.
#' @param statistics Character vector of statistics to include.
#' Select among `c("sens", "spec", "ppv", "npv", "test_pos_rate", "test_neg_rate", "tp_rate", "tn_rate", "fn_rate", "fp_rate", "prevalence")`.
#' Default is `c("sens", "spec", "ppv", "npv")`.
#' @param prevalence Specify the prevalence of outcome for case-control studies.
#' Default is `NULL` and the prevalence will be estimated from `data`.
#' @param thresholds Numeric vector of thresholds at which the performance
#' statistics are calculated. Default is `NULL`, which returns performance
#' statistics at each observed prediction level.
#' @return A data.frame containing the columns \code{.threshold}, \code{.metric},
#' \code{.estimator}, and \code{.estimate}
#' @examples
#' data(single_model_dataset)
#' threshperf(single_model_dataset, outcome = 'outcomes', positive = '1', prediction = 'predictions')
#' @export
threshperf <- function(df, outcome, positive, prediction, 
                       thresholds = NULL,
                       statistics = c("sens", "spec", "ppv", "npv"),
                       prevalence = NULL) {
  statistics <-
    match.arg(statistics,
              choices = c("sens", "spec", "ppv", "npv", "test_pos_rate", "test_neg_rate",
                          "tp_rate", "tn_rate", "fn_rate", "fp_rate", "prevalence"),
              several.ok = TRUE)

  if(is.null(thresholds)){
    thresholds = unique(c(0,sort(unique(df[[prediction]])), 1))
  }

  df <- dplyr::select(df, dplyr::all_of(c(outcome, prediction)))

  df <- stats::na.omit(df)

  # IMPORTANT because order of levels matters to yardstick

  df[[outcome]] = ifelse(positive == df[[outcome]], 1, 0)  
  df[[outcome]] = factor(df[[outcome]], levels = c(1, 0))

  df <-
    df %>%
    expand_preds(threshold = thresholds,
                 inc = c(outcome, prediction))

  df <-
    df %>%
    dplyr::mutate(alt_pred = recode_data(!!rlang::parse_expr(outcome),
                                         !!rlang::parse_expr(prediction), .threshold))

  df_orig <- df


  df <- df %>% dplyr::group_by(.threshold)

  df_metrics <-
    df %>%
    two_class(truth = !!rlang::parse_expr(outcome), estimate = alt_pred) %>%
    # adding other stats using the prevalence
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(".threshold", ".estimator")),
      names_from = dplyr::all_of(".metric"),
      values_from = dplyr::all_of(".estimate")
    ) %>%
    dplyr::mutate(
      # TODO: Check prev calculation against the yardstick ordering!
      prevalence =
        .env$prevalence %||% unname((table(df[[outcome]]) / nrow(df))[1]),
      # all calculations use sens, spec, and prev to be able to handle
      # case-control data where true prev is not in data frame
      tp_rate = .data$sens * .data$prevalence,
      fp_rate = (1 - .data$spec) * (1 - .data$prevalence),
      tn_rate = .data$spec * (1 - .data$prevalence),
      fn_rate = (1 - .data$sens) * .data$prevalence,
      test_pos_rate = .data$tp_rate + .data$fp_rate,
      test_neg_rate = 1 - .data$test_pos_rate,
      ppv = .data$tp_rate / .data$test_pos_rate,
      npv = .data$tn_rate / .data$test_neg_rate
    ) %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(c(".threshold", ".estimator")),
      names_to = ".metric",
      values_to = ".estimate"
    ) %>%
    dplyr::filter(.data$.metric %in% .env$statistics)

  suppressWarnings({df_metrics <-
    df_metrics %>%
    dplyr::group_by(.threshold) %>%
    dplyr::mutate(
      denom =
        dplyr::case_when(
          .data$.metric == 'sens' ~ sum(df_orig[[outcome]] == 1),
          .data$.metric == 'spec' ~ sum(df_orig[[outcome]] == 0),
          .data$.metric == 'ppv' ~ sum(df_orig$alt_pred == 1),
          .data$.metric == 'npv' ~ sum(df_orig$alt_pred == 0),
          .data$.metric %in% c("tp_rate", "fp_rate", "tn_rate", "fn_rate",
                               "test_pos_rate", "test_neg_rate", "prevalence") ~ nrow(df_orig),

        )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(numer = round(.data$.estimate * .data$denom)) %>%
    stats::na.omit()})

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
#' @param positive A character string containing the value of outcome that is the positive class.
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
#' @param thresholds
#' @return A ggplot containing the threshold-performance plot. The 95 percent
#'   confidence intervals are estimated using Wilson's interval from the
#'   \code{Hmisc} \code{\link[Hmisc]{binconf}} function.
#' @examples
#' data(single_model_dataset)
#' threshperf_plot(single_model_dataset, outcome = 'outcomes', positive = '1', prediction = 'predictions')
#' @export
threshperf_plot <- function(df, outcome, positive, prediction, show_denom = TRUE, plot_title = '',
                            xmin = 0,
                            xmax = 1,
                            pre_tp_geoms = NULL,
                            pre_dist_geoms = NULL,
                            post_tp_geoms = NULL,
                            post_dist_geoms = NULL,
                            heights = c(10,1),
                            widths = c(1,2,1),
                            thresholds = NULL) {
  tp_data = threshperf(df, outcome, positive, prediction, thresholds)
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
    ggplot2::ggplot(ggplot2::aes(x=!!rlang::parse_expr(prediction)))

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
#' @param positive A character string containing the value of outcome that is the positive class.
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
#' @param thresholds
#' @return A ggplot containing the threshold-performance plot. The 95 percent
#'   confidence intervals are estimated using Wilson's interval from the
#'   \code{Hmisc} \code{\link[Hmisc]{binconf}} function.
#' @examples
#' data(multi_model_dataset)
#' threshperf_plot_multi(multi_model_dataset, outcome = 'outcomes', positive = '1', prediction = 'predictions', model = 'model_name')
#' @export
threshperf_plot_multi <- function(df, outcome, positive, prediction, model, plot_title = '',
                                  xmin = 0,
                                  xmax = 1,
                                  pre_tp_geoms = NULL,
                                  pre_dist_geoms = NULL,
                                  post_tp_geoms = NULL,
                                  post_dist_geoms = NULL,
                                  heights = c(10,1),
                                  widths = c(1,2,1), thresholds = NULL) {

  how_many_models = df[[model]] %>% unique() %>% length()

  tp_data_list = list()
  for (model_name in unique(df[[model]])) {
    tp_data_list[[model_name]] <-
      threshperf(df[df[[model]] == model_name,],
                 outcome,
                 positive,
                 prediction, thresholds)
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
                                 color = !!rlang::parse_expr(model),
                                 fill = !!rlang::parse_expr(model)))


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

  threshold_dist_plot <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::parse_expr(prediction)))

  if (!is.null(pre_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      pre_dist_geoms
  }

  threshold_dist_plot =
    threshold_dist_plot +
    ggplot2::geom_density(alpha = 1/how_many_models, ggplot2::aes(fill = !!rlang::parse_expr(model), color = !!rlang::parse_expr(model))) +
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
