#' A single calibration plot with error bars showing 95% confidence intervals
#'
#' @param df The df as a data.frame.
#' @param outcome_var A character string containing the name of the column containing
#' the predictions.
#' @param pred_var A character string containing the name of the column containing
#' the predictions.
#' @param n_bins Number of bins. Defaults to 10.
#' @param plot_title A character string containing the title for the resulting plot.
#'
#' @return A ggplot containing the calibration plot
cal_plot <- function(df, outcome_var, pred_var, n_bins = 10, plot_title = '', ...){

  # The calibration plot
  g1 <- dplyr::mutate(df, bin = dplyr::ntile(get(pred_var), n_bins)) %>%
    # Bin prediction into n_bins
    dplyr::group_by(bin) %>%
    dplyr::mutate(n = dplyr::n(), # Get ests and CIs
           bin_pred = mean(get(pred_var), na.rm = TRUE),
           bin_prob = mean(as.numeric(as.character(get(outcome_var))), na.rm = TRUE),
           se = sqrt((bin_prob * (1 - bin_prob)) / n),
           ul = bin_prob + 1.96 * se,
           ll = bin_prob - 1.96 * se) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
    ggplot2::geom_errorbar(size = 0.5, color = "black", width = 0.02) +
    ggplot2::geom_point(size = 2, color = 'black') +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::geom_abline() + # 45 degree line indicating perfect calibration
    # geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
    #             color = "black", formula = y~-1 + x) +
    # straight line fit through estimates
    # geom_smooth(ggplot2::aes(x = get(pred_var), y = as.numeric(outcome)),
    #            color = "red", se = FALSE, method = "loess") +
    # loess fit through estimates
    ggplot2::xlab("Predicted Probability") +
    ggplot2::ylab("Observed Risk") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(plot_title)

  # The distribution plot
  g2 <- ggplot2::ggplot(df, ggplot2::aes(x = get(pred_var))) +
    ggplot2::geom_histogram(fill = "black", bins = 200) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggeasy::easy_remove_y_axis() +
    ggeasy::easy_remove_x_axis() +
    ggplot2::theme_void()

  # g2 / g1 + patchwork::plot_layout(heights = c(1,10))
  layout = c(patchwork::area(t = 1, b = 8, l = 1, r = 10),
             patchwork::area(t = 9, b = 10, l = 1, r = 10))

  g1 / g2 + patchwork::plot_layout(design = layout)

}



#' Multiple calibration plots with colored/shaded 95% confidence intervals
#'
#' @param df The df as a data.frame.
#' @param outcome_var A character string containing the name of the column containing
#' the predictions.
#' @param pred_var A character string containing the name of the column containing
#' the predictions.
#' @param model_column A character string containing the name of the column containing
#' the model names
#' @param n_bins Number of bins. Defaults to 10.
#' @param plot_title A character string containing the title for the resulting plot.
#'
#' @return A ggplot containing the calibration plot
cal_plot_multi <- function(df, outcome_var, pred_var, model_column, n_bins = 10, plot_title = '', ...){

  how_many_models = df[[model_column]] %>% unique() %>% length()

  # The calibration plot
  g1 <- df %>%
    dplyr::group_by(get(model_column)) %>%
    dplyr::mutate(bin = dplyr::ntile(get(pred_var), n_bins)) %>%
    # Bin prediction
    dplyr::group_by(get(model_column), bin) %>%
    dplyr::mutate(n = dplyr::n(), # Get ests and CIs
           bin_pred = mean(get(pred_var), na.rm = TRUE),
           bin_prob = mean(as.numeric(as.character(get(outcome_var))), na.rm = TRUE),
           se = sqrt((bin_prob * (1 - bin_prob)) / n),
           ul = bin_prob + 1.96 * se,
           ll = bin_prob - 1.96 * se) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = bin_pred,
                        y = bin_prob,
                        ymin = ll,
                        ymax = ul,
                        color = get(model_column),
                        fill = get(model_column))) +
    # geom_errorbar(size = 0.5, width = 0.02) +
    ggplot2::geom_ribbon(alpha = 1/how_many_models) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(size = 1, alpha = 1/how_many_models) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::geom_abline(linetype = 'dashed') + # 45 degree line indicating perfect calibration
    # geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
    #            color = "black", formula = y~-1 + x) +
    # straight line fit through estimates
    # geom_smooth(ggplot2::aes(x = get(pred_var), y = as.numeric(outcome)),
    #            color = "red", se = FALSE, method = "loess") +
    # loess fit through estimates
    # scale_color_viridis(discrete = TRUE, option = 'cividis', begin = 0.5) +
    # scale_fill_viridis(discrete = TRUE, option = 'cividis', begin = 0.5) +
    ggplot2::xlab("Predicted Probability") +
    ggplot2::ylab("Observed Risk") +
    ggeasy::easy_remove_legend(color) +
    ggeasy::easy_add_legend_title(fill = 'Model') +
    ggplot2::scale_color_brewer(palette = 'Set1') +
    ggplot2::scale_fill_brewer(palette = 'Set1') +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(plot_title)


  # The distribution plot
  g2 <- ggplot2::ggplot(df, ggplot2::aes(x = get(pred_var))) +
    ggplot2::geom_density(alpha = 1/how_many_models, ggplot2::aes(fill = get(model_column), color = get(model_column))) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
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

  layout = c(patchwork::area(t = 1, b = 8, l = 1, r = 10),
             patchwork::area(t = 9, b = 10, l = 1, r = 10))

  g1 / g2 + patchwork::plot_layout(design = layout)

}

