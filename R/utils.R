expand_preds <- function (.data, threshold, inc = NULL) {
  threshold <- unique(threshold)
  nth <- length(threshold)
  n_data <- nrow(.data)
  if (!is.null(inc))
    .data <- select(.data, all_of(inc))
  .data <- .data[rep(1:nrow(.data), times = nth), ]
  .data$.threshold <- rep(threshold, each = n_data)
  .data
}

recode_data <- function (obs, prob, threshold) {
  lvl <- levels(obs)
  if (getOption("yardstick.event_first", default = TRUE)) {
    pred <- ifelse(prob >= threshold, lvl[1], lvl[2])
  }
  else {
    pred <- ifelse(prob >= threshold, lvl[2], lvl[1])
  }
  factor(pred, levels = lvl)
}

two_class = function (...) {
  mets <- metric_set(sens,
                                spec)
  mets(...)
}
