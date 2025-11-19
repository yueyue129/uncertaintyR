#include <Rcpp.h>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rcpp_quantile(NumericVector x, NumericVector probs) {
  if (x.size() == 0) {
    Rcpp::stop("`x` must have at least one observation.");
  }
  NumericVector sorted = clone(x);
  std::sort(sorted.begin(), sorted.end());

  int n = sorted.size();
  NumericVector result(probs.size());

  for (int i = 0; i < probs.size(); ++i) {
    double p = probs[i];
    if (R_IsNA(p) || p < 0.0 || p > 1.0) {
      Rcpp::stop("Probabilities must lie in [0, 1].");
    }
    if (n == 1) {
      result[i] = sorted[0];
      continue;
    }
    double idx = p * (n - 1);
    int low = std::floor(idx);
    int high = std::ceil(idx);
    double weight = idx - low;
    double val_low = sorted[low];
    double val_high = sorted[high];
    result[i] = val_low + weight * (val_high - val_low);
  }

  return result;
}

