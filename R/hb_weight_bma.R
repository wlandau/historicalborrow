#' @title Bayesian model averaging weight for the pooled model
#'   using the method by Best et al. (2021).
#' @export
#' @family model-weights
#' @description Calculate the weight on the pooled model
#'   using the Bayesian model averaging technique by
#'   Best et al (2021).
#' @details This calcualtion is part of a Bayesian model averaging
#'   placebo borrowing method which averages the posteriors
#'   of the pooled and independnet models.
#'   Uses the method by Best et al. (2021). See the vignettes
#'   for details.
#' @references Best N, Price RG, Pouliquen IJ, Keene ON.
#'   Assessing efficacy in important subgroups in confirmatory trials:
#'   An example using Bayesian dynamic borrowing.
#'   Pharm Stat. 2021 May;20(3):551-562. doi: 10.1002/pst.2093.
#'   Epub 2021 Jan 21. PMID: 33475231; PMCID: PMC8247867.
#'   <https://onlinelibrary.wiley.com/doi/10.1002/pst.2093>.
#' @param mll_pool Finite numeric of length 1,
#'   marginal log likelihood of the pooled model.
#'   Use `hb_mll_pool()$logml`.
#' @param mll_independent Finite numeric of length 1,
#'   marginal log likelihood of the pooled model.
#'   Use `hb_mll_independent()$logml`.
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
#'   mll_pool <- hb_mll_pool(mcmc = mcmc_pool, data = data)
#'   mll_independent <- hb_mll_independent(mcmc = mcmc_independent, data = data)
#' })
#' hb_weight_bma(
#'   mll_pool = mll_pool$logml,
#'   mll_independent = mll_independent$logml
#' )
#' }
hb_weight_bma <- function(
  mll_pool,
  mll_independent,
  prior_weight = 0.5
) {
  true(mll_pool, length(.) == 1L, is.numeric(.), is.finite(.))
  true(mll_independent, length(.) == 1L, is.numeric(.), is.finite(.))
  true(
    prior_weight,
    length(.) == 1L,
    is.numeric(.),
    is.finite(.),
    . >= 0,
    . <= 1
  )
  log_pool <- mll_pool + log(prior_weight)
  log_independent <- mll_independent + log(1 - prior_weight)
  log_denominator <- log(exp(log_pool) + exp(log_independent))
  log_weight <- log_pool - log_denominator
  exp(log_weight)
}
