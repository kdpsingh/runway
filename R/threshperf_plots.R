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
#' @param statistics Character vector of statistics to include.
#' Select among `c("sens", "spec", "ppv", "npv", "test_pos_rate", "test_neg_rate", "tp_rate", "tn_rate", "fn_rate", "fp_rate", "prevalence")`.
#' Default is `c("sens", "spec", "ppv", "npv")`.
#' @param prevalence Specify the prevalence of outcome for case-control studies.
#' Default is `NULL` and the prevalence will be estimated from `data`.
#' @param positive
#' @param thresholds Numeric vector of thresholds at which the performance
#' statistics are calculated. Default is `NULL`, which returns performance
#' statistics at each observed prediction level.
#' @return A data.frame containing the columns \code{.threshold}, \code{.metric},
#' \code{.estimator}, and \code{.estimate}
#' @examples
#' data(single_model_dataset)
#' threshperf(single_model_dataset, outcome = 'outcomes', prediction = 'predictions')
#' @export
threshperf <- function(df, outcome, prediction, positive = 'has_sepsis',
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

  df <- select(df, all_of(c(outcome, prediction)))

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
    mutate(alt_pred = recode_data(!!parse_expr(outcome),
                                         !!parse_expr(prediction), .threshold))

  df <- df %>% group_by(.threshold)

  df_metrics <-
    df %>%
    two_class(truth = !!parse_expr(outcome), estimate = alt_pred) %>%
    # adding other stats using the prevalence
    pivot_wider(
      id_cols = all_of(c(".threshold", ".estimator")),
      names_from = all_of(".metric"),
      values_from = all_of(".estimate")
    ) %>%
    mutate(
      # TODO: Check prev calculation against the yardstick ordering!
      prevalence =
        .env$prevalence %||% unname((table(df$outcomes) / nrow(df))[2]),
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
    pivot_longer(
      cols = -all_of(c(".threshold", ".estimator")),
      names_to = ".metric",
      values_to = ".estimate"
    ) %>%
    filter(.data$.metric %in% .env$statistics)

  suppressWarnings({df_metrics <-
    df_metrics %>%
    group_by(.threshold) %>%
    mutate(
      denom =
        case_when(
          .data$.metric == 'sens' ~ sum(df_orig[[outcome]] == 1),
          .data$.metric == 'spec' ~ sum(df_orig[[outcome]] == 0),
          .data$.metric == 'ppv' ~ sum(df_orig[[prediction]] >= .threshold),
          .data$.metric == 'npv' ~ sum(df_orig[[prediction]] < .threshold),
          .data$.metric %in% c("tp_rate", "fp_rate", "tn_rate", "fn_rate",
                               "test_pos_rate", "test_neg_rate", "prevalence") ~ nrow(df_orig),

        )
    ) %>%
    ungroup() %>%
    mutate(numer = round(.data$.estimate * .data$denom)) %>%
    na.omit()})

  df_ci = binconf(x = df_metrics$numer, n = df_metrics$denom,
                         alpha = 0.05, method = 'wilson') %>%
    as_tibble() %>%
    rename(ll = Lower, ul = Upper) %>%
    mutate_at(vars(ul, ll), . %>% oob_squish(range = c(0,1)))

  df_metrics = bind_cols(df_metrics, df_ci)

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
#' @param positive
#' @param thresholds
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
                            widths = c(1,2,1),positive = 'has_sepsis', thresholds = NULL) {
  tp_data = threshperf(df, outcome, prediction, thresholds)
  tp_data =
    tp_data %>%
    group_by(.metric) %>%
    mutate(denom_frac =
                    if_else(.metric %in% c('ppv', 'npv'),
                                   denom/max(denom), NA_real_, NA_real_)) %>%
    ungroup()

  tp_plot =
    tp_data %>%
    mutate(.metric = case_when(
      .metric == 'npv' ~ 'NPV',
      .metric == 'ppv' ~ 'PPV',
      .metric == 'spec' ~ 'Specificity',
      .metric == 'sens' ~ 'Sensitivity')) %>%
    mutate(.metric = factor(.metric, levels = c('Sensitivity', 'Specificity', 'PPV', 'NPV'))) %>%
    mutate_at(vars(.estimate, ll, ul), . %>% {. * 100}) %>%
    ggplot(aes(x = .threshold,
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
      geom_ribbon(aes(ymin = 0, ymax = denom_frac*100), fill = 'grey', alpha = 1/3)
  }

  tp_plot = tp_plot +
    geom_ribbon(fill = 'grey') +
    geom_line(size = 1) +
    facet_grid(.metric~.) +
    theme_bw() +
    coord_cartesian(xlim=c(xmin,xmax)) +
    labs(x = 'Threshold', y = 'Performance (%)') +
    ggtitle(plot_title)

  if (!is.null(post_tp_geoms)) {
    tp_plot =
      tp_plot +
      post_tp_geoms
  }

  threshold_dist_plot =
    df %>%
    ggplot(aes(x=!!parse_expr(prediction)))

  if (!is.null(pre_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      pre_dist_geoms
  }

  threshold_dist_plot =
    threshold_dist_plot +
    geom_histogram(fill = 'black', bins = 100) +
    coord_cartesian(xlim=c(xmin,xmax)) +
    theme_void()

  if (!is.null(post_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      post_dist_geoms
  }

  plot_spacer() +
    (tp_plot / threshold_dist_plot + plot_layout(heights = heights)) +
    plot_spacer() +
    plot_layout(widths = widths)
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
#' @param thresholds
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
                                  widths = c(1,2,1), thresholds = NULL) {

  how_many_models = df[[model]] %>% unique() %>% length()

  tp_data_list = list()
  for (model_name in unique(df[[model]])) {
    tp_data_list[[model_name]] <-
      threshperf(df[df[[model]] == model_name,],
                 outcome,
                 prediction, thresholds)
    tp_data_list[[model_name]][[model]] <- model_name
  }

  tp_data = bind_rows(tp_data_list)

  tp_plot =
    tp_data %>%
    mutate(.metric = case_when(
      .metric == 'npv' ~ 'NPV',
      .metric == 'ppv' ~ 'PPV',
      .metric == 'spec' ~ 'Specificity',
      .metric == 'sens' ~ 'Sensitivity')) %>%
    mutate(.metric = factor(.metric, levels = c('Sensitivity', 'Specificity', 'PPV', 'NPV'))) %>%
    mutate_at(vars(.estimate, ll, ul), . %>% {. * 100}) %>%
    ggplot(aes(x = .threshold,
                                 y = .estimate,
                                 ymin = ll,
                                 ymax = ul,
                                 color = !!parse_expr(model),
                                 fill = !!parse_expr(model)))


  if (!is.null(pre_tp_geoms)) {
    tp_plot =
      tp_plot +
      pre_tp_geoms
  }

  tp_plot = tp_plot +
    geom_ribbon(alpha = 1/how_many_models) +
    geom_line(size = 1) +
    facet_grid(.metric~.) +
    theme_bw() +
    coord_cartesian(xlim=c(xmin,xmax)) +
    labs(x = 'Threshold', y = 'Performance (%)') +
    scale_color_brewer(name = 'Models', palette = 'Set1') +
    scale_fill_brewer(name = 'Models', palette = 'Set1') +
    ggtitle(plot_title)

  if (!is.null(post_tp_geoms)) {
    tp_plot =
      tp_plot +
      post_tp_geoms
  }

  threshold_dist_plot <- ggplot(df, aes(x = !!parse_expr(prediction)))

  if (!is.null(pre_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      pre_dist_geoms
  }

  threshold_dist_plot =
    threshold_dist_plot +
    geom_density(alpha = 1/how_many_models, aes(fill = !!parse_expr(model), color = !!parse_expr(model))) +
    scale_x_continuous(limits = c(xmin, xmax), breaks = seq(xmin, xmax, by = (xmax-xmin)/10)) +
    # scale_color_viridis(discrete = TRUE, option = 'cividis', begin = 0.5) +
    # scale_fill_viridis(discrete = TRUE, option = 'cividis', begin = 0.5) +
    xlab("") +
    ylab("") +
    scale_color_brewer(palette = 'Set1') +
    scale_fill_brewer(palette = 'Set1') +
    theme_minimal() +
    easy_remove_y_axis() +
    #  easy_remove_x_axis(what = c('ticks','line')) +
    easy_remove_legend(fill, color) +
    theme_void()

  if (!is.null(post_dist_geoms)) {
    threshold_dist_plot =
      threshold_dist_plot +
      post_dist_geoms
  }


  plot_spacer() +
    (tp_plot / threshold_dist_plot + plot_layout(heights = heights)) +
    plot_spacer() +
    plot_layout(widths = widths)
}

