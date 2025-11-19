#' uncertaintyR: Quantifying predictive uncertainty
#'
#' @keywords internal
#' @useDynLib uncertaintyR, .registration = TRUE
#' @import Rcpp
#' @importFrom stats predict qt sd coef model.frame update vcov model.matrix terms delete.response
"_PACKAGE"

utils::globalVariables(c("estimate", "method", "lower", "upper", "std_error", "detail", "row"))

