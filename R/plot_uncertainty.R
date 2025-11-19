#' Plot prediction intervals
#'
#' @description
#' Given the tidy output from [predict_interval()], creates a faceted ribbon plot
#' displaying point predictions and corresponding uncertainty bands. Optionally
#' overlays observed outcomes for quick calibration diagnostics.
#'
#' @param intervals A tibble returned by [predict_interval()].
#' @param actual Optional numeric vector of observed outcomes aligned with the
#'   `row` column in `intervals`.
#' @param facet Logical. If `TRUE` (default) a facet is drawn per method;
#'   otherwise methods are distinguished via colour.
#' @param title Optional plot title.
#' @param show_points Logical controlling whether point predictions are drawn.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' ufit <- fit_uncertainty(mod, method = "bootstrap", B = 100)
#' ints <- predict_interval(ufit, mtcars[1:10, ])
#' plot_uncertainty(ints, actual = mtcars$mpg[1:10])
plot_uncertainty <- function(
    intervals,
    actual = NULL,
    facet = TRUE,
    title = NULL,
    show_points = TRUE) {

  required_cols <- c("row", "method", "lower", "upper", "estimate")
  missing_cols <- setdiff(required_cols, names(intervals))
  if (length(missing_cols) > 0) {
    stop("Missing columns in intervals tibble: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  p <- ggplot2::ggplot(intervals, ggplot2::aes(x = row, y = estimate, colour = method))
  p <- p +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper, fill = method),
      alpha = 0.2,
      colour = NA
    )

  if (show_points) {
    p <- p + ggplot2::geom_point()
  } else {
    p <- p + ggplot2::geom_line()
  }

  if (!is.null(actual)) {
    actual_df <- tibble::tibble(
      row = seq_along(actual),
      actual = actual
    )
    p <- p +
      ggplot2::geom_point(
        data = actual_df,
        ggplot2::aes(x = row, y = actual),
        inherit.aes = FALSE,
        colour = "black",
        size = 2,
        alpha = 0.5
      )
  }

  if (facet) {
    p <- p + ggplot2::facet_wrap(~method, scales = "free_y")
  }

  p <- p +
    ggplot2::labs(
      x = "Observation index",
      y = "Prediction",
      colour = "Method",
      fill = "Method",
      title = title
    ) +
    ggplot2::theme_minimal()

  p
}

