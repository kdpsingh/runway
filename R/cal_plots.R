#' Generate a single calibration plot with error bars showing 95 percent
#' confidence intervals
#'
#' This code builds off of code written by Darren Dahly, PhD in this blog post:
#' \url{https://darrendahly.github.io/post/homr/}.
#'
#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column
#'   containing the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column
#'   containing the predictions.
#' @param n_bins Number of bins. Defaults to 10. Set to 0 to hide binned
#'   calibration.
#' @param show_loess Whether to show loess smoothed calibration estimates.
#'   Defaults to FALSE. For \code{cal_plot()}, you can display both a binned
#'   calibration plot and a loess curve. In contrast, for
#'   \code{cal_plot_multi()}, these options are mutually exclusive.
#' @param plot_title A character string containing the title for the resulting
#'   plot.
#' @return A ggplot containing the calibration plot
#' @examples
#' data(single_model_dataset)
#' cal_plot(single_model_dataset, outcome = 'outcomes', prediction = 'predictions', n_bins = 5)
#' @export
cal_plot <- function(df, outcome, prediction, n_bins = 10, show_loess = FALSE, plot_title = '', ...){

  # The calibration plot
  g1 <- mutate(df, bin = ntile(!!parse_expr(prediction), n_bins)) %>%
    # Bin prediction into n_bins
    group_by(bin) %>%
    mutate(n = n(), # Get ests and CIs
           bin_pred = mean(!!parse_expr(prediction), na.rm = TRUE),
           bin_prob = mean(as.numeric(as.character(!!parse_expr(outcome))), na.rm = TRUE),
           se = sqrt((bin_prob * (1 - bin_prob)) / n),
           ul = bin_prob + 1.96 * se,
           ll = bin_prob - 1.96 * se) %>%
    mutate_at(vars(ul, ll), . %>% oob_squish(range = c(0,1))) %>%
    ungroup() %>%
    ggplot() +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_abline(linetype = 'dashed') # 45 degree line indicating perfect calibration

  if (n_bins > 0) {
    g1 = g1  +
      geom_point(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul),
                        size = 2, color = 'black') +
      geom_errorbar(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul),
                           size = 0.5, color = "black", width = 0.02)
  }
    # geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
    #             color = "black", formula = y~-1 + x) +
    # straight line fit through estimates

  if (show_loess) {
    g1 = g1 +
    geom_smooth(aes(x = !!parse_expr(prediction), y = as.numeric(!!parse_expr(outcome))),
               color = "black", se = TRUE, method = "loess")
    # loess fit through estimates
  }

  g1 = g1 +
    xlab("Predicted Probability") +
    ylab("Observed Risk") +
    theme_minimal() +
    theme(aspect.ratio = 1) +
    ggtitle(plot_title)

  # The distribution plot
  g2 <- ggplot(df, aes(x = !!parse_expr(prediction))) +
    geom_histogram(fill = "black", bins = 100) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    easy_remove_y_axis() +
    easy_remove_x_axis() +
    theme_void() +
    theme(aspect.ratio = 0.1)

  # g2 / g1 + plot_layout(heights = c(1,10))
  layout = c(area(t = 1, b = 10, l = 1, r = 10),
             area(t = 11, b = 12, l = 1, r = 10))

  g1 / g2

}


#' Generate multiple calibration plots with colored/shaded 95 percent confidence intervals
#'
#' This code builds off of code written by Darren Dahly, PhD in this blog post:
#' \url{https://darrendahly.github.io/post/homr/}.
#'
#' @param df The df as a data.frame.
#' @param outcome A character string containing the name of the column containing
#' the outcomes (expressed as 0/1s).
#' @param prediction A character string containing the name of the column containing
#' the predictions.
#' @param model A character string containing the name of the column containing
#' the model names
#' @param n_bins Number of bins. Defaults to 10. Set to 0 to hide binned calibration.
#' @param show_loess Whether to show loess smoothed calibration estimates. Defaults to FALSE.
#' For \code{cal_plot()}, you can display both a binned calibration plot and a loess curve. In
#' contrast, for \code{cal_plot_multi()}, these options are mutually exclusive. To display a loess
#' line, you must set \code{n_bins} to 0.
#' @param plot_title A character string containing the title for the resulting plot.
#' @return A ggplot containing the calibration plot
#' @examples
#' data(multi_model_dataset)
#' cal_plot_multi(multi_model_dataset, outcome = 'outcomes',model = 'model_name', prediction = 'predictions', n_bins = 5)
#' @export
cal_plot_multi <- function(df, outcome, prediction, model, n_bins = 10, show_loess = FALSE, plot_title = '', ...){

  if((n_bins > 0 && show_loess == TRUE) || (n_bins == 0 && show_loess == FALSE)) {
    stop('You must either set n_bins > 0 and show_loess to FALSE or set n_bins to 0 and show_loess to TRUE. Both cannot be displayed for cal_plot_multi()')
  }

  how_many_models = df[[model]] %>% unique() %>% length()

  # The calibration plot
  g1 <- df %>%
    group_by(!!parse_expr(model)) %>%
    mutate(bin = ntile(!!parse_expr(prediction), n_bins)) %>%
    # Bin prediction
    group_by(!!parse_expr(model), bin) %>%
    mutate(n = n(), # Get ests and CIs
           bin_pred = mean(!!parse_expr(prediction), na.rm = TRUE),
           bin_prob = mean(as.numeric(as.character(!!parse_expr(outcome))), na.rm = TRUE),
           se = sqrt((bin_prob * (1 - bin_prob)) / n),
           ul = bin_prob + 1.96 * se,
           ll = bin_prob - 1.96 * se) %>%
    mutate_at(vars(ul, ll), . %>% oob_squish(range = c(0,1))) %>%
    ungroup() %>%
    ggplot() +
    # geom_errorbar(size = 0.5, width = 0.02) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_abline(linetype = 'dashed') # 45 degree line indicating perfect calibration
    # geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
    #            color = "black", formula = y~-1 + x) +
    # straight line fit through estimates
    # geom_smooth(aes(x = get(prediction), y = as.numeric(outcome)),
    #            color = "red", se = FALSE, method = "loess") +
    # loess fit through estimates
    # scale_color_viridis(discrete = TRUE, option = 'cividis', begin = 0.5) +
    # scale_fill_viridis(discrete = TRUE, option = 'cividis', begin = 0.5) +


    if (show_loess == FALSE && n_bins > 0) {
      g1 = g1 + aes(x = bin_pred,
                   y = bin_prob,
                   color = !!parse_expr(model),
                   fill = !!parse_expr(model)) +
        geom_ribbon(aes(ymin = ll,
                                          ymax = ul,),
                             alpha = 1/how_many_models) +
        geom_point(size = 2) +
        geom_line(size = 1, alpha = 1/how_many_models)
    }
    else if (show_loess == TRUE && n_bins == 0) {
      g1 = g1 +
        stat_smooth(aes(x = !!parse_expr(prediction), y = as.numeric(!!parse_expr(outcome)),
                                          color = !!parse_expr(model), fill = !!parse_expr(model)),
                            #              alpha = 1/how_many_models), # currently ignored by geom_smooth
                             se = TRUE, method = "loess")
      # loess fit through estimates
    }

  g1 = g1 +
    xlab("Predicted Probability") +
    ylab("Observed Risk") +
    scale_color_brewer(name = 'Models', palette = 'Set1') +
    scale_fill_brewer(name = 'Models', palette = 'Set1') +
    theme_minimal() +
    theme(aspect.ratio = 1) +
    ggtitle(plot_title)


  # The distribution plot
  g2 <- ggplot(df, aes(x = !!parse_expr(prediction))) +
    geom_density(alpha = 1/how_many_models, aes(fill = !!parse_expr(model), color = !!parse_expr(model))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    coord_fixed() +
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
    theme_void() +
    theme(aspect.ratio = 0.1)

  layout = c(area(t = 1, b = 10, l = 1, r = 10),
             area(t = 11, b = 12, l = 1, r = 10))

  g1 / g2

}

