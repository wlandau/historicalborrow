#' @title Mixture model MCMC hyperparameters
#' @export
#' @family mcmc
#' @description Run a simple model separately on each historical study
#'   control group and compute hyperparameters for [hb_mcmc_mixture()].
#' @details The model is a simple Bayesian model with a normal likelihood,
#'   an unknown mean `mu`, and an unknown standard deviation `sigma`.
#'   For each historical study, the posterior mean of `mu` becomes
#'   the corresponding component of `m_omega` in the output,
#'   and the posterior standard deviation of `mu`
#'   becomes the corresponding component of `s_omega` in the output.
#'   See the examples in this help file for a demonstration.
#'   `m_omega` and `s_omega` define the components of the mixture prior
#'   in [hb_mcmc_mixture()] that act as the contribution of the
#'   historical studies to the model.
#' @return A tidy data frame of hyperparameter values for [hb_mcmc_mixture()].
#'   The first several rows are for historical studies, and the last row
#'   is for the current study. Studies/rows are sorted in the order
#'   [hb_mcmc_mixture()] sorts them, so you can use columns `m_omega`
#'   and `s_omega` for the same dataset and same values of other arguments
#'   directly in [hb_mcmc_mixture()].
#' @inheritParams hb_mcmc_mixture
#' @param m_mu Numeric of length 1, prior mean of the mean `mu` in
#'   the simple model.
#' @param s_mu Numeric of length 1, prior standard deviation of the
#'   mean `mu` in the simple model.
#' @param s_sigma Numeric of length 1, uniform prior upper bound
#'   of the residual standard deviation `sigma` in the simple model.
#' @param m_omega_current Numeric with length 1,
#'   `m_omega` value of the current study.
#'   Inserted as the final component of the `m_omega` column in the output.
#' @param s_omega_current Numeric with length 1,
#'   `s_omega` value of the current study.
#'   Inserted as the final component of the `s_omega` column in the output.
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
hb_mcmc_mixture_hyperparameters <- function(
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient",
  m_mu = 0,
  s_mu = 30,
  s_sigma = 30,
  m_omega_current = 0,
  s_omega_current = 30,
  n_chains = 4,
  n_adapt = 2e3,
  n_warmup = 4e3,
  n_iterations = 2e4,
  quiet = TRUE
) {
  true(m_mu, length(.) == 1, is.finite(.), is.numeric(.))
  true(s_mu, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_sigma, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(m_omega_current, length(.) == 1, is.finite(.), is.numeric(.))
  true(s_omega_current, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_chains, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_adapt, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_warmup, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_iterations, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(quiet, length(.) == 1, !anyNA(.), is.logical(.))
  data_standardized <- hb_data(
    data = data,
    response = response,
    study = study,
    study_reference = study_reference,
    group = group,
    group_reference = group_reference,
    patient = patient,
    covariates = character(0)
  )
  true(
    length(unique(data_standardized$study)) >= 1,
    message = paste(
      "hb_mcmc_mixture_hyperparameters()",
      "data should have more than one study."
    )
  )
  data_standardized <- dplyr::filter(data_standardized, group == min(group))
  out <- dplyr::group_modify(
    dplyr::group_by(data_standardized, study),
    ~hb_mcmc_mixture_hyperparameters_study(
      .x,
      m_mu = m_mu,
      s_mu = s_mu,
      s_sigma = s_sigma,
      n_chains = n_chains,
      n_adapt = n_adapt,
      n_warmup = n_warmup,
      n_iterations = n_iterations,
      quiet = quiet
    )
  )
  out$m_omega[out$study == max(out$study)] <- m_omega_current
  out$s_omega[out$study == max(out$study)] <- s_omega_current
  out$study_index <- out$study
  study_vector <- data[[study]]
  study_index_vector <- as_index_max(data[[study]], max = study_reference)
  index <- tibble::tibble(
    study = study_vector,
    study_index = study_index_vector
  )
  index <- dplyr::distinct(index, study, study_index)
  out$study <- NULL
  out <- dplyr::left_join(x = out, y = index, by = "study_index")
  out[, c("study", "study_index", "m_omega", "s_omega"), drop = FALSE]
}

hb_mcmc_mixture_hyperparameters_study <- function(
  data,
  m_mu,
  s_mu,
  s_sigma,
  n_chains,
  n_adapt,
  n_warmup,
  n_iterations,
  quiet
) {
  samples <- hb_mcmc_mixture_hyperparameters_study_samples(
    data = data,
    m_mu = m_mu,
    s_mu = s_mu,
    s_sigma = s_mu,
    n_chains = n_chains,
    n_adapt = n_adapt,
    n_warmup = n_warmup,
    n_iterations = n_iterations,
    quiet = quiet
  )
  tibble::tibble(m_omega = mean(samples$mu), s_omega = sd(samples$mu))
}

hb_mcmc_mixture_hyperparameters_study_samples <- function(
  data,
  m_mu,
  s_mu,
  s_sigma,
  n_chains,
  n_adapt,
  n_warmup,
  n_iterations,
  quiet
) {
  data_list <- list(
    y = data$response,
    n = nrow(data),
    m_mu = m_mu,
    s_mu = s_mu,
    s_sigma = s_sigma
  )
  jags_mcmc(
    file = "mixture_hyperparameters.jags",
    variables = c("mu", "sigma"),
    data_list = data_list,
    n_chains = n_chains,
    n_adapt = n_adapt,
    n_warmup = n_warmup,
    n_iterations = n_iterations,
    quiet = quiet
  )
}
