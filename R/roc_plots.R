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
  obj <- roc_(df, response = outcome, predictor = prediction, ci = ci, plot=FALSE)
  ciobj <- ci.se(obj, specificities = seq(0, 1, l = 25))
  dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
                       lower = ciobj[, 1],
                       upper = ciobj[, 3])

  g1 = ggroc(obj) +
    theme_minimal() +
    geom_abline(
      slope = 1,
      intercept = 1,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) +
    coord_equal() +
    ggtitle(plot_title) +
    xlab("1-Specificity") +
    ylab("Sensitivity")

  if(ci){
    g2 = g1 + geom_ribbon(
      data = dat.ci,
      aes(x = x, ymin = lower, ymax = upper),
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
    group_by(!!parse_expr(model)) %>%
    group_nest() %>%
    mutate(roc = map(data, ~ roc_(data = ., response = outcome, predictor = prediction, ci = ci, plot=FALSE)),
                  roc = roc %>% setNames(!!parse_expr(model)),
                  ci_spec = map(roc, ~ ci.se(., specificities = seq(0, 1, l = 25))),
                  ci_ribbon = map(ci_spec, ~ build_ci_data(.)))

  # build ROC curves
  g1 = ggroc(ci_data$roc) +
    theme_minimal() +
    geom_abline(
      slope = 1,
      intercept = 1,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) +
    coord_equal() +
    scale_color_brewer(name = 'Models', palette = 'Set1') +
    scale_fill_brewer(name = 'Models', palette = 'Set1') +
    ggtitle(plot_title) +
    xlab("1-Specificity") +
    ylab("Sensitivity")

  # build CI intervals
  if(ci){
    ribbon = ci_data %>%
      select(!!parse_expr(model), ci_ribbon) %>%
      rename(name = !!parse_expr(model)) %>%
      unnest_wider(ci_ribbon) %>%
      unnest(cols = c(x, lower, upper))

    # ci_values = ci_data %>%
    #   select(model_name, roc) %>%
    #   mutate(ci = map(roc, ~ .$ci)) %>%
    #   select(-roc)

    ci_values = ci_data %>%
      select(!!parse_expr(model), roc) %>%
      mutate(ci = map(roc, ~ ci(.)))

    g2 = g1 + geom_ribbon(data = ribbon,
                                   aes(fill = name, x = x, ymin = lower, ymax = upper),
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
