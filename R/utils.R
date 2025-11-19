#' @keywords internal
ensure_data <- function(model, X = NULL, y = NULL) {
  if (!is.null(X) && !is.null(y)) {
    if (!is.data.frame(X)) {
      X <- as.data.frame(X)
    }
    if (nrow(X) != length(y)) {
      stop("X and y must have the same number of observations.", call. = FALSE)
    }
    response_name <- tryCatch(
      {
        f <- stats::formula(model)
        if (length(f) >= 2) {
          as.character(f)[2]
        } else {
          NULL
        }
      },
      error = function(e) NULL
    )
    if (is.null(response_name) || response_name == "") {
      response_name <- if (!is.null(colnames(y))) {
        colnames(y)[1]
      } else {
        "response"
      }
    }
    X[[response_name]] <- y
    return(list(
      data = X,
      response = response_name
    ))
  }

  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(mf)) {
    stop(
      "Could not reconstruct training data. ",
      "Please supply both X and y explicitly.",
      call. = FALSE
    )
  }

  response_name <- names(mf)[1]
  list(
    data = mf,
    response = response_name
  )
}

#' @keywords internal
safe_update <- function(model, data) {
  updated <- tryCatch(
    stats::update(model, data = data),
    error = function(e) NULL
  )
  if (is.null(updated)) {
    stop(
      "Model of class '", class(model)[1], "' does not support update(). ",
      "Please provide a model that can be refit via stats::update.",
      call. = FALSE
    )
  }
  updated
}

#' @keywords internal
jackknife_predictions <- function(model, train_data, newdata) {
  n <- nrow(train_data)
  jk_preds <- matrix(NA_real_, nrow = nrow(newdata), ncol = n)
  for (i in seq_len(n)) {
    refit <- safe_update(model, data = train_data[-i, , drop = FALSE])
    jk_preds[, i] <- stats::predict(refit, newdata = newdata)
  }

  pseudo_mean <- rowMeans(jk_preds)
  center <- rowMeans(jk_preds)
  jack_var <- ((n - 1) / n) * rowSums((jk_preds - pseudo_mean)^2)
  list(
    predictions = jk_preds,
    variance = jack_var
  )
}

#' @keywords internal
bootstrap_predictions <- function(model, train_data, newdata, B = 200, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  n <- nrow(train_data)
  boot_preds <- matrix(NA_real_, nrow = nrow(newdata), ncol = B)
  for (b in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    boot_refit <- safe_update(model, data = train_data[idx, , drop = FALSE])
    boot_preds[, b] <- stats::predict(boot_refit, newdata = newdata)
  }
  boot_preds
}

#' @keywords internal
bayesian_linpred <- function(model, newdata, B = 200, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed + 1)
  }
  coefs <- stats::coef(model)
  vc <- tryCatch(stats::vcov(model), error = function(e) NULL)
  if (is.null(vc)) {
    stop("Model must support vcov() for Bayesian sampling.", call. = FALSE)
  }
  terms_obj <- stats::terms(model)
  X <- stats::model.matrix(stats::delete.response(terms_obj), newdata)
  draws <- MASS::mvrnorm(n = B, mu = coefs, Sigma = vc)
  pred_draws <- X %*% t(draws)
  as.matrix(pred_draws)
}

#' @keywords internal
conformal_interval <- function(model, train_data, newdata, response_name, level = 0.95) {
  preds <- stats::predict(model, newdata = train_data)
  actual <- train_data[[response_name]]
  residuals <- abs(actual - preds)
  alpha <- 1 - level
  n <- length(residuals)
  k <- ceiling((n + 1) * (1 - alpha))
  k <- min(max(k, 1), n)
  radius <- rcpp_quantile(residuals, probs = (k / (n + 1)))
  center <- stats::predict(model, newdata = newdata)
  lower <- center - radius
  upper <- center + radius
  list(
    center = center,
    lower = lower,
    upper = upper,
    radius = radius
  )
}

#' @keywords internal
stack_interval_df <- function(method, lower, estimate, upper, std_error = NA_real_, detail = NA_real_) {
  tibble::tibble(
    method = method,
    lower = lower,
    estimate = estimate,
    upper = upper,
    std_error = std_error,
    detail = detail
  )
}

