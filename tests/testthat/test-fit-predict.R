test_that("fit_uncertainty stores training diagnostics", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  ufit <- fit_uncertainty(model, method = c("bootstrap", "jackknife"), B = 50)

  expect_s3_class(ufit, "uncertainty_fit")
  expect_equal(length(ufit$residuals), nrow(mtcars))
  expect_true(all(c("bootstrap", "jackknife") %in% ufit$method))
})

test_that("predict_interval generates sensible bounds", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  ufit <- fit_uncertainty(model, method = c("bootstrap", "conformal"), B = 20, seed = 123)
  preds <- predict_interval(ufit, newdata = mtcars[1:5, ], level = 0.90, seed = 2024)

  expect_true(all(c("row", "method", "estimate", "lower", "upper") %in% names(preds)))
  expect_equal(nrow(preds), 5 * length(unique(ufit$method)))
  expect_true(all(preds$upper >= preds$lower))
})

test_that("rcpp_quantile matches base quantile implementation", {
  x <- rnorm(100)
  expect_equal(
    rcpp_quantile(x, c(0.1, 0.5, 0.9)),
    as.numeric(stats::quantile(x, c(0.1, 0.5, 0.9), names = FALSE)),
    tolerance = 1e-8
  )
})

