#' @title Non-longitudinal hierarchical simulations.
#' @export
#' @family simulate
#' @description Simulate from the non-longitudinal hierarchical model.
#' @return A list with the following elements:
#'   * `data`: tidy long-form dataset with the patient-level data.
#'     one row per patient and indicator columns for the study,
#'     group (e.g. treatment arm), and patient ID. The `response`
#'     columns is the patient response. The other columns are
#'     baseline covariates. The control group is the one with
#'     the `group` column equal to 1, and the current study (non-historical)
#'     is the one with the maximum value of the `study` column.
#'     Only the current study has any non-control-group patients,
#'     the historical studies have only the control group.
#'   * `parameters`: named list of model parameter values.
#'     See the model specification vignette for details.
#'   * `matrices`: A named list of model matrices.
#'     See the model specification vignette for details.
#' @inheritParams hb_sim_pool
#' @param s_mu Numeric of length 1,
#'   prior standard deviation of `mu`.
#' @param s_tau Numeric of length 1,
#'   Upper bound on `tau`.
#' @param mu Numeric of length 1,
#'   mean of the control group means `alpha`.
#' @param tau Numeric of length 1,
#'   standard deviation of the control group means `alpha`.
#' @examples
#' hb_sim_hierarchical()$data
hb_sim_hierarchical <- function(
  n_study = 5,
  n_group = 3,
  n_patient = 100,
  n_continuous = 0,
  n_binary = 0,
  s_delta = 1,
  s_beta = 1,
  s_sigma = 1,
  s_mu = 1,
  s_tau = 1,
  alpha = stats::rnorm(n = n_study, mean = mu, sd = tau),
  delta = stats::rnorm(n = n_group - 1, mean = 0, sd = s_delta),
  beta = stats::rnorm(
    n = n_study * (n_continuous + n_binary),
    mean = 0,
    sd = s_delta
  ),
  sigma = stats::runif(n = n_study, min = 0, max = s_sigma),
  mu = stats::rnorm(n = 1, mean = 0, sd = s_mu),
  tau = stats::runif(n = 1, min = 0, max = s_tau)
) {
  true(n_study, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_group, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_patient, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_continuous, length(.) == 1, is.finite(.), is.numeric(.), . >= 0)
  true(n_binary, length(.) == 1, is.finite(.), is.numeric(.), . >= 0)
  true(n_study, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_delta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_beta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_sigma, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_mu, length(.) == 1, is.finite(.), is.numeric(.))
  true(s_tau, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(alpha, is.finite(.), length(.) == n_study)
  true(delta, is.finite(.), is.numeric(.), length(.) == n_group - 1)
  true(beta, (all(is.finite(.)) || !length(beta)), is.numeric(.))
  true(length(beta) == n_study * (n_continuous + n_binary))
  true(sigma, all(is.finite(.)), is.numeric(.), length(.) == n_study)
  true(mu, is.numeric(.), is.finite(.), length(.) == 1)
  true(tau, is.numeric(.), is.finite(.), length(.) == 1, . > 0)
  data <- hb_sim_grid(n_study, n_group, n_patient)
  x_alpha <- get_x_alpha(data)
  x_delta <- get_x_delta(data)
  covariates <- hb_sim_x_beta(
    data = data,
    x_alpha = x_alpha,
    x_delta = x_delta,
    n_continuous = n_continuous,
    n_binary = n_binary
  )
  data <- dplyr::bind_cols(data, tibble::as_tibble(covariates))
  x_beta <- get_x_beta(data = data, x_alpha = x_alpha, x_delta = x_delta)
  data$response <- hb_sim_response(
    data = data,
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta,
    alpha = alpha,
    delta = delta,
    beta = beta,
    sigma = sigma
  )
  hb_warn_identifiable(
    response = data$response,
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta
  )
  parameters <- list(
    alpha = alpha,
    delta = delta,
    beta = beta,
    sigma = sigma,
    mu = mu,
    tau = tau
  )
  matrices <- list(
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta
  )
  list(
    data = data,
    parameters = parameters,
    matrices = matrices
  )
}
