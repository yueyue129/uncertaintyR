# uncertaintyR <img src="https://img.shields.io/badge/status-dev-blue.svg" align="right" />

Quantifying and visualizing predictive uncertainty across statistical and machine learning models.

## Installation

```r
# install.packages("devtools")
devtools::install_github("yourname/uncertaintyR")
```

Development dependencies: `Rcpp`, `ggplot2`, `bench`, `testthat`, `rmarkdown`.

## Motivation

Point predictions alone are seldom sufficient for high-stakes decision making. `uncertaintyR`
provides a unified API to derive prediction intervals using resampling, Bayesian, and conformal
approaches while keeping the user-facing workflow close to base R generics.

## Key Features

- `fit_uncertainty()` stores training diagnostics for any model supporting `predict()` and `update()`.
- `predict_interval()` computes bootstrap, jackknife, Bayesian, or conformal prediction bands.
- `plot_uncertainty()` generates coverage visualisations with minimal code.
- Optional benchmarking helpers in the vignette illustrate correctness via `all.equal()` and
  efficiency via `bench::mark()`.

## Quick Start

```r
library(uncertaintyR)

mod <- lm(mpg ~ wt + hp, data = mtcars)
u_fit <- fit_uncertainty(mod, method = c("bootstrap", "conformal"), B = 200)
intervals <- predict_interval(u_fit, newdata = mtcars[1:10, ])
plot_uncertainty(intervals, actual = mtcars$mpg[1:10])
```

## Documentation

- Function reference: see help pages via `?fit_uncertainty`, `?predict_interval`, `?plot_uncertainty`.
- Vignette: `vignette("uncertaintyR-demo")` demonstrates benchmarking against base `predict.lm()`.

## Testing

Automated tests are provided through `testthat`:

```r
devtools::test()
```

## Contributing

Issues and pull requests are welcome. Please run `devtools::check()` before submitting changes.

## License

MIT © Yueyue129

