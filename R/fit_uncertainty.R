#' Fit an uncertainty wrapper around a predictive model
#'
#' @description
#' `fit_uncertainty()` stores the training data, residual diagnostics, and
#' resampling configuration needed to compute uncertainty estimates for any
#' model that supports `predict()` and `update()`. The function does **not**
#' refit the model; instead, it records the information required so that
#' intervals can be generated later with `predict_interval()`.
#'
#' @param model A fitted model object with valid `predict()` and `update()`
#'   methods (e.g., objects returned by `lm()`, `glm()`, `nnet::nnet()`,
#'   `caret::train()`, etc.).
#' @param X Optional data frame or matrix of predictors used to fit `model`.
#'   Required when the original training data cannot be reconstructed from the
#'   fitted object (e.g., when the model stores a pre-processed design matrix
#'   only). Must have the same number of rows as `y`.
#' @param y Optional numeric outcome vector aligned with `X`. If omitted,
#'   `fit_uncertainty()` attempts to retrieve the response from `model`.
#' @param method Character vector of uncertainty methods to enable. Supported
#'   choices are `"jackknife"`, `"bootstrap"`, `"bayes"`, and `"conformal"`.
#'   Provide more than one value to enable multiple methods.
#' @param B Number of resamples or posterior draws used by methods that require
#'   Monte Carlo sampling (bootstrap and Bayesian). Defaults to 200.
#' @param level Nominal coverage level used by conformal intervals. Individual
#'   methods may compute quantiles directly in `predict_interval()`.
#' @param seed Optional integer seed to make resampling reproducible.
#' @param ... Additional arguments reserved for future extensions.
#'
#' @return An object of class `"uncertainty_fit"` that records the fitted model,
#'   training data, resampling configuration, and stored diagnostics.
#' @export
#'
#' @examples
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' ufit <- fit_uncertainty(mod, method = c("bootstrap", "conformal"), B = 100)
#' ufit
fit_uncertainty <- function(
    model,
    X = NULL,
    y = NULL,
    method = c("jackknife", "bootstrap", "bayes", "conformal"),
    B = 200,
    level = 0.95,
    seed = NULL,
    ...) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  supported <- c("jackknife", "bootstrap", "bayes", "conformal")
  method <- unique(method)
  bad_methods <- setdiff(method, supported)
  if (length(bad_methods) > 0) {
    stop("Unsupported method(s): ", paste(bad_methods, collapse = ", "), call. = FALSE)
  }

  data_info <- ensure_data(model, X = X, y = y)
  response_name <- data_info$response
  train_data <- data_info$data

  if (!is.numeric(train_data[[response_name]])) {
    stop("Currently `fit_uncertainty()` only supports numeric responses.", call. = FALSE)
  }

  fitted_vals <- stats::predict(model, newdata = train_data)
  residuals <- train_data[[response_name]] - fitted_vals

  structure(
    list(
      model = model,
      train_data = train_data,
      response = response_name,
      method = method,
      B = B,
      level = level,
      residuals = residuals,
      call = match.call(),
      dots = list(...)
    ),
    class = "uncertainty_fit"
  )
}

#' @export
print.uncertainty_fit <- function(x, ...) {
  cat("<uncertainty_fit>\n")
  cat("Model class : ", paste(class(x$model), collapse = "/"), "\n", sep = "")
  cat("Methods     : ", paste(x$method, collapse = ", "), "\n", sep = "")
  cat("Resamples   : ", x$B, "\n", sep = "")
  cat("Coverage    : ", format(x$level, digits = 3), "\n", sep = "")
  invisible(x)
}

