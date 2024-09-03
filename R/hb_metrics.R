#' @title Legacy function to calculate borrowing metrics
#' @export
#' @keywords internal
#' @description Legacy function to calculate
#'   historical borrowing metrics using
#'   summary output from a fitted borrowing model and
#'   analogous summaries from the benchmark models.
#'   Please use [hb_ess()] instead of [hb_metrics()].
#' @return A data frame with borrowing metrics.
#' @param pool A data frame returned by [hb_summary()]
#'   for the pooled model.
#' @param independent A data frame returned by [hb_summary()]
#'   for the independent model.
#' @param borrow A data frame returned by [hb_summary()]
#'   for the mixture or hierarchical model.
#' @examples
#' if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
#' data <- hb_sim_independent(n_continuous = 2)$data
#' mcmc_borrow <- hb_mcmc_hierarchical(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 100,
#'   n_iterations = 200
#' )
#' mcmc_pool <- hb_mcmc_pool(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
#' mcmc_independent <- hb_mcmc_independent(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
#' borrow <- hb_summary(mcmc_borrow, data)
#' pool <- hb_summary(mcmc_pool, data)
#' independent <- hb_summary(mcmc_independent, data)
#' hb_metrics(
#'   borrow = borrow,
#'   pool = pool,
#'   independent = independent
#' )
#' }
hb_metrics <- function(borrow, pool, independent) {
  true(is.data.frame(borrow))
  true(is.data.frame(pool))
  true(is.data.frame(independent))
  true(nrow(borrow) == nrow(pool))
  true(nrow(borrow) == nrow(independent))
  true("group" %in% colnames(borrow))
  true(all(borrow$group == pool$group))
  true(all(borrow$group == independent$group))
  for (name in c("response_mean", "response_variance")) {
    true(name %in% colnames(borrow))
    true(name %in% colnames(pool))
    true(name %in% colnames(independent))
  }
  borrow <- dplyr::filter(borrow, group == min(group))
  pool <- dplyr::filter(pool, group == min(group))
  independent <- dplyr::filter(independent, group == min(group))
  mean_shift_ratio <- (borrow$response_mean - independent$response_mean) /
    (pool$response_mean - independent$response_mean)
  variance_shift_ratio <-
    (borrow$response_variance - independent$response_variance) /
    (pool$response_variance - independent$response_variance)
  out <- tibble::tibble(
    mean_shift_ratio = mean_shift_ratio,
    variance_shift_ratio = variance_shift_ratio
  )
  out
}
