expand_preds <- function (.data, threshold, inc = NULL) {
  threshold <- unique(threshold)
  nth <- length(threshold)
  n_data <- nrow(.data)
  if (!is.null(inc))
    .data <- dplyr::select(.data, all_of(inc))
  .data <- .data[rep(1:nrow(.data), times = nth), ]
  .data$.threshold <- rep(threshold, each = n_data)
  .data
}

recode_data <- function (obs, prob, threshold) {  
  lvl <- levels(obs)
  binary_pred <- ifelse(prob >= threshold, lvl[1], lvl[2])
  factor(binary_pred, levels = lvl)
}

two_class = function (...) {
  mets <- yardstick::metric_set(yardstick::sens,
                                yardstick::spec)
  mets(...)
}
