#' @title Bayesian model averaging weight for the pooled model
#' @export
#' @family bma-utils
#' @description Calculate the Bayesian model averaging weight
#'   of the pooled model.
#' @details This calcualtion is part of a Bayesian model averaging
#'   placebo borrowing method which averages the posteriors
#'   of the pooled and independnet models.
#'   Uses the method by Best et al. (2019). See the vignettes
#'   for details.
#' @references Best N, Price RG, Pouliquen IJ, Keene ON.
#'   Assessing efficacy in important subgroups in confirmatory trials:
#'   An example using Bayesian dynamic borrowing.
#'   Pharm Stat. 2021 May;20(3):551-562. doi: 10.1002/pst.2093.
#'   Epub 2021 Jan 21. PMID: 33475231; PMCID: PMC8247867.
#'   <https://onlinelibrary.wiley.com/doi/10.1002/pst.2093>.
#' @param ml_pool Positive numeric of length 1,
#'   marginal likelihood of the pooled model.
#'   Compute with [hb_ml_pool()].
#' @param ml_independent Positive numeric of length 1,
#'   marginal likelihood of the pooled model.
#'   Compute with [hb_ml_independent()].
#' @param prior_weight Numeric of length 1 between 0 and 1,
#'   prior weight on the pooled model.
#' @examples
#' if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
#' data <- hb_sim_independent(n_continuous = 2)$data
#' mcmc_pool <- hb_mcmc_pool(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 2000,
#'   n_warmup = 2000,
#'   n_iterations = 8000
#' )
#' mcmc_independent <- hb_mcmc_independent(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 2000,
#'   n_warmup = 2000,
#'   n_iterations = 8000
#' )
#' suppressWarnings({
#'   ml_pool <- exp(hb_ml_pool(mcmc = mcmc_pool, data = data)logml)
#'   ml_independent <- exp(
#'     hb_ml_independent(mcmc = mcmc_independent, data = data)$logml
#'   )
#' })
#' hb_bma_weight(
#'   ml_pool = ml_pool,
#'   ml_independent = ml_independent
#' )
#' }
hb_bma_weight <- function(
  ml_pool,
  ml_independent,
  prior_weight = 0.5
) {
  true(ml_pool, length(.) == 1L, is.numeric(.), !anyNA(.), . > 0)
  true(ml_independent, length(.) == 1L, is.numeric(.), !anyNA(.), . > 0)
  true(prior_weight, length(.) == 1L, is.numeric(.), !anyNA(.), . >= 0, . <= 1)
  pool <- ml_pool * prior_weight
  independent <- ml_independent * (1 - prior_weight)
  pool / (pool + independent)
}
