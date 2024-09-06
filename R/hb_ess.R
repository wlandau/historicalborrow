#' @title Effective sample size (ESS)
#' @export
#' @family summary
#' @description Quantify borrowing with effective sample size (ESS)
#'   as cited and explained in the methods vignette at
#'   <https://wlandau.github.io/historicalborrow/articles/methods.html>.
#' @return A data frame with one row and the following columns:
#'   * `v0`: posterior predictive variance of the control group mean of a
#'     hypothetical new study given the pooled model.
#'     Calculated as the mean over MCMC samples of `1 / sum(sigma_i ^ 2)`,
#'     where each `sigma_i` is the residual standard deviation of
#'     study `i` estimated from the pooled model.
#'   * `v_tau`: posterior predictive variance of a hypothetical
#'     new control group mean under the hierarchical model.
#'     Calculated by averaging over predictive draws,
#'     where each predictive draw is from
#'     `rnorm(n = 1, mean = mu_, sd = tau_)` and `mu_` and `tau_` are the
#'     `mu` and `tau` components of an MCMC sample.
#'   * `n`: number of non-missing historical control patients.
#'   * `weight`: strength of borrowing as a ratio of variances: `v0 / v_tau`.
#'   * `ess`: strength of borrowing as an effective sample size:
#'      `n v0 / v_tau`, where `n` is the number of non-missing historical
#'      control patients.
#' @inheritParams hb_data
#' @param mcmc_pool A fitted model from [hb_mcmc_pool()].
#' @param mcmc_hierarchical A fitted model from [hb_mcmc_hierarchical()].
#' @examples
#'   data <- hb_sim_independent(n_continuous = 2)$data
#'   data$group <- sprintf("group%s", data$group)
#'   data$study <- sprintf("study%s", data$study)
#'   pool <- hb_mcmc_pool(
#'     data,
#'     n_chains = 1,
#'     n_adapt = 100,
#'     n_warmup = 50,
#'     n_iterations = 50
#'   )
#'   hierarchical <- hb_mcmc_hierarchical(
#'     data,
#'     n_chains = 1,
#'     n_adapt = 100,
#'     n_warmup = 50,
#'     n_iterations = 50
#'   )
#'   hb_ess(
#'     mcmc_pool = pool,
#'     mcmc_hierarchical = hierarchical,
#'     data = data
#'   )
hb_ess <- function(
  mcmc_pool,
  mcmc_hierarchical,
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient"
) {
  true(
    mcmc_hierarchical,
    is.data.frame(.),
    !is.null(colnames(.)),
    all(c("mu", "tau") %in% colnames(.))
  )
  true(mcmc_pool, is.data.frame(.), !is.null(colnames(.)))
  true(data, is.data.frame(.), !is.null(colnames(.)))
  data <- hb_data(
    data = data,
    response = response,
    study = study,
    study_reference = study_reference,
    group = group,
    group_reference = group_reference,
    patient = patient,
    covariates = character(0L)
  )
  v0 <- hb_ess_v0(data, mcmc_pool)
  v_tau <- hb_ess_v_tau(mcmc_hierarchical)
  weight <- v0 / v_tau
  n <- sum(
    data$group == min(data$group) &
      data$study < max(data$study) &
      !is.na(data$response)
  )
  ess <- n * weight
  tibble::tibble(
    ess = ess,
    weight = weight,
    n = n,
    v0 = v0,
    v_tau = v_tau
  )
}

hb_ess_v_tau <- function(mcmc_hierarchical) {
  stats::var(
    stats::rnorm(
      n = nrow(mcmc_hierarchical),
      mean = mcmc_hierarchical$mu,
      sd = mcmc_hierarchical$tau
    )
  )
}

hb_ess_v0 <- function(data, mcmc_pool) {
  sigma <- dplyr::select(mcmc_pool, tidyselect::starts_with("sigma["))
  precision <- as.data.frame(lapply(sigma, function(x) x ^ (-2)))
  mean(1 / rowSums(precision))
}
