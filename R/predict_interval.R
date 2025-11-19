#' Compute prediction intervals from an uncertainty fit
#'
#' @description
#' Given an object created by `fit_uncertainty()`, `predict_interval()` produces
#' uncertainty-aware predictions for new data using one or more resampling
#' strategies. Output is returned in a tidy tibble with one row per observation
#' per method.
#'
#' @param object An object returned by [fit_uncertainty()].
#' @param newdata A data frame containing predictors at which predictions should
#'   be evaluated. Must include the same columns that were used to fit the
#'   original model.
#' @param level Nominal coverage level. Defaults to the level stored in the
#'   fit object.
#' @param method Subset of methods to evaluate. Defaults to all methods that
#'   were enabled when `fit_uncertainty()` was called.
#' @param B Optional integer specifying the number of resamples to draw. If
#'   omitted, the value stored inside the fit object is used.
#' @param keep_draws If `TRUE`, the function also returns the raw resampled
#'   draws (useful for custom plotting). Defaults to `FALSE`.
#' @param seed Optional integer for reproducibility.
#' @param ... Reserved for future extensions.
#'
#' @return A tibble with columns `row`, `method`, `estimate`, `lower`, `upper`,
#'   `std_error`, and optional metadata. When `keep_draws = TRUE`, an additional
#'   list-column `draws` is included.
#' @export
#'
#' @examples
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' ufit <- fit_uncertainty(mod, method = c("bootstrap", "jackknife"), B = 100)
#' predict_interval(ufit, newdata = mtcars[1:5, ])
predict_interval <- function(
    object,
    newdata,
    level = object$level,
    method = object$method,
    B = object$B,
    keep_draws = FALSE,
    seed = NULL,
    ...) {

  if (!inherits(object, "uncertainty_fit")) {
    stop("`object` must be produced by fit_uncertainty().", call. = FALSE)
  }

  newdata <- as.data.frame(newdata)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  method <- intersect(method, object$method)
  if (length(method) == 0) {
    stop("No overlap between requested method and fitted methods.", call. = FALSE)
  }

  train_data <- object$train_data
  response_name <- object$response

  base_pred <- stats::predict(object$model, newdata = newdata)
  alpha <- (1 - level) / 2
  out_rows <- vector("list", length(method))
  draws <- vector("list", length(method))

  for (i in seq_along(method)) {
    m <- method[i]
    if (m == "bootstrap") {
      boot_mat <- bootstrap_predictions(object$model, train_data, newdata, B = B, seed = seed)
      lower <- apply(boot_mat, 1, rcpp_quantile, probs = c(alpha))
      upper <- apply(boot_mat, 1, rcpp_quantile, probs = c(1 - alpha))
      se <- apply(boot_mat, 1, stats::sd)
      out_rows[[i]] <- stack_interval_df(
        method = rep("bootstrap", length(lower)),
        lower = lower,
        estimate = base_pred,
        upper = upper,
        std_error = se
      )
      out_rows[[i]]$row <- seq_len(nrow(newdata))
      if (keep_draws) {
        draws[[i]] <- lapply(seq_len(nrow(newdata)), function(j) boot_mat[j, ])
      }
    } else if (m == "jackknife") {
      jk <- jackknife_predictions(object$model, train_data, newdata)
      se <- sqrt(jk$variance)
      crit <- stats::qt(1 - alpha, df = nrow(train_data) - 1)
      lower <- base_pred - crit * se
      upper <- base_pred + crit * se
      out_rows[[i]] <- stack_interval_df(
        method = rep("jackknife", length(lower)),
        lower = lower,
        estimate = base_pred,
        upper = upper,
        std_error = se
      )
      out_rows[[i]]$row <- seq_len(nrow(newdata))
      if (keep_draws) {
        draws[[i]] <- lapply(seq_len(nrow(newdata)), function(j) jk$predictions[j, ])
      }
    } else if (m == "bayes") {
      bayes_draws <- bayesian_linpred(object$model, newdata, B = B, seed = seed)
      lower <- apply(bayes_draws, 1, rcpp_quantile, probs = c(alpha))
      upper <- apply(bayes_draws, 1, rcpp_quantile, probs = c(1 - alpha))
      se <- apply(bayes_draws, 1, stats::sd)
      out_rows[[i]] <- stack_interval_df(
        method = rep("bayes", length(lower)),
        lower = lower,
        estimate = base_pred,
        upper = upper,
        std_error = se
      )
      out_rows[[i]]$row <- seq_len(nrow(newdata))
      if (keep_draws) {
        draws[[i]] <- lapply(seq_len(nrow(newdata)), function(j) bayes_draws[j, ])
      }
    } else if (m == "conformal") {
      conf <- conformal_interval(object$model, train_data, newdata, response_name, level = level)
      out_rows[[i]] <- stack_interval_df(
        method = rep("conformal", length(conf$center)),
        lower = conf$lower,
        estimate = conf$center,
        upper = conf$upper,
        std_error = rep(NA_real_, length(conf$center)),
        detail = rep(conf$radius, length(conf$center))
      )
      out_rows[[i]]$row <- seq_len(nrow(newdata))
      if (keep_draws) {
        draws[[i]] <- replicate(length(conf$center), NA_real_, simplify = FALSE)
      }
    }
  }

  result <- dplyr::bind_rows(out_rows)
  result <- dplyr::relocate(result, row, method, estimate, lower, upper, std_error, detail)

  if (keep_draws) {
    result$draws <- unlist(draws, recursive = FALSE)
  }
  tibble::as_tibble(result)
}

