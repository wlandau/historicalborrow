#' @title Mixture model MCMC
#' @export
#' @family mcmc
#' @description Run the mixture model with MCMC.
#' @details The study-specific components of the mixture prior are all fixed
#'   in advance. Mixture components are normal distributions
#'   with means in `m_omega` and standard deviations in `s_omega`.
#'   These vectors are ordered with historical studies first
#'   and the current study last.
#'   These mixture components can be computed using
#'   [hb_mcmc_mixture_hyperparameters()] on a full set of data
#'   (all the historical studies and the current study together).
#'   Then the `m_omega` and `s_omega` columns of the output
#'   can be plugged directly into [hb_mcmc_mixture()].
#'   See the examples for a demonstration.
#' @return A tidy data frame of parameter samples from the
#'   posterior distribution. Columns `.chain`, `.iteration`,
#'   and `.draw` have the meanings documented in the
#'   `posterior` package.
#' @inheritParams hb_sim_mixture
#' @inheritParams hb_mcmc_pool
#' @param m_omega Numeric with length equal to the number of
#'   supposed studies (but only the current study is in the data).
#'   `m_omega` is the prior control group mean of each study.
#'   The last element corresponds to the current study,
#'   and the others are for historical studies.
#' @param s_omega Numeric with length equal to the number of
#'   supposed studies (but only the current study is in the data).
#'   `s_omega` is the prior control group standard deviation of each study.
#'   The last element corresponds to the current study,
#'   and the others are for historical studies.
#' @param p_omega Numeric with length equal to the number of
#'   supposed studies (but only the current study is in the data).
#'   `p_omega` is the prior control group mixture proportion of each study.
#'   The last element corresponds to the current study,
#'   and the others are for historical studies.
#' @examples
#' data_all_studies <- hb_sim_independent(n_continuous = 2)$data
#' data_all_studies$study <- paste0("study", data_all_studies$study)
#' hyperparameters <- hb_mcmc_mixture_hyperparameters(
#'   data = data_all_studies,
#'   response = "response",
#'   study = "study",
#'   study_reference = "study5",
#'   group = "group",
#'   group_reference = 1,
#'   patient = "patient",
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
#' print(hyperparameters)
#' data_current_study <- dplyr::filter(data_all_studies, study == max(study))
#' hb_mcmc_mixture(
#'   data = data_current_study,
#'   response = "response",
#'   study = "study",
#'   study_reference = "study5",
#'   group = "group",
#'   group_reference = 1,
#'   patient = "patient",
#'   m_omega = hyperparameters$m_omega, # use hyperparams from historical data
#'   s_omega = hyperparameters$s_omega, # use hyperparams from historical data
#'   p_omega = rep(1 / nrow(hyperparameters), nrow(hyperparameters)),
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
hb_mcmc_mixture <- function(
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient",
  covariates = grep("^covariate", colnames(data), value = TRUE),
  s_delta = 30,
  s_beta = 30,
  s_sigma = 30,
  m_omega = c(0, 0),
  s_omega = c(30, 30),
  p_omega = 1 / length(m_omega),
  n_chains = 4,
  n_adapt = 2e3,
  n_warmup = 4e3,
  n_iterations = 2e4,
  quiet = TRUE
) {
  true(s_delta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_beta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_sigma, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(m_omega, is.numeric(.), is.finite(.), length(.) >= 1)
  true(s_omega, is.numeric(.), is.finite(.), . > 0)
  true(p_omega, is.numeric(.), is.finite(.), . >= 0, . <= 1, sum(.) == 1)
  true(length(m_omega) == length(s_omega))
  true(length(s_omega) == length(p_omega))
  true(n_chains, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_adapt, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_warmup, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_iterations, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(quiet, length(.) == 1, !anyNA(.), is.logical(.))
  data <- hb_data(
    data = data,
    response = response,
    study = study,
    study_reference = study_reference,
    group = group,
    group_reference = group_reference,
    patient = patient,
    covariates = covariates
  )
  true(
    length(unique(data$study)) == 1,
    message = "mixture model data should have only one study."
  )
  x_alpha <- get_x_alpha_pool_or_mixture(data)
  x_delta <- get_x_delta(data)
  x_beta <- get_x_beta(data = data, x_alpha = x_alpha, x_delta = x_delta)
  hb_warn_identifiable(
    response = data$response,
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta
  )
  data_list <- list(
    y = data$response,
    n_data = nrow(data),
    n_study = length(m_omega),
    n_delta = ncol(x_delta),
    n_beta = ncol(x_beta),
    s_delta = s_delta,
    s_beta = s_beta,
    s_sigma = s_sigma,
    m_omega = m_omega,
    s_omega = s_omega,
    p_omega = p_omega,
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta
  )
  file <- "mixture_beta.jags"
  variables <- c("alpha", "delta", "beta", "sigma", "omega", "post_p")
  if (!prod(dim(x_beta))) {
    data_list$n_beta <- NULL
    data_list$s_beta <- NULL
    data_list$x_beta <- NULL
    file <- "mixture_nobeta.jags"
    variables <- c("alpha", "delta", "sigma", "omega", "post_p")
  }
  jags_mcmc(
    file = file,
    variables = variables,
    data_list = data_list,
    n_chains = n_chains,
    n_adapt = n_adapt,
    n_warmup = n_warmup,
    n_iterations = n_iterations,
    quiet = quiet
  )
}
