#' @title Superseded suggest s_tau given a uniform prior on tau.
#' @export
#' @keywords internal
#' @description For the uniform prior on tau,
#'   suggest a value of the `s_tau` hyperparameter
#'   to achieve a given amount of borrowing in the hierarchical model.
#'   Only use if a diffuse prior on `tau` is not feasible.
#' @details See the hierarchical model section of the
#'   methods vignette for details.
#' @return Numeric of length equal to `length(precision_ratio)` and
#'   `length(sigma)`, suggested values of s_tau for each element of
#'   `precision_ratio` and `sigma`.
#' @param precision_ratio Positive numeric vector of elements between 0 and 1
#'   with target precision ratios.
#' @param sigma Positive numeric vector of residual standard deviations.
#' @param n Number of non-missing patients.
#' @examples
#' hb_s_tau(precision_ratio = 0.5, sigma = 1, n = 100)
hb_s_tau <- function(precision_ratio = 0.5, sigma = 1, n = 100) {
  true(precision_ratio, . > 0, . < 1, is.finite(.))
  true(sigma, . > 0, is.finite(.))
  true(length(precision_ratio) == length(sigma))
  2 * sigma * sqrt((1 / n) * ((1 / precision_ratio) - 1))
}
