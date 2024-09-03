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
#' @param prior_tau Character string, name of the prior of `tau`.
#'   If `prior_tau` equals `"uniform"`, then the prior on `tau` is
#'   a uniform prior with lower bound 0 and upper bound `s_tau`.
#'   If `prior_tau` equals `"half-t"`, then the prior on `tau` is a
#'   half Student-t prior with center 0, lower bound 0, scale parameter
#'   `s_tau`, and degrees of freedom `d_tau`. The scale parameter `s_tau`
#'   is analogous to the `sigma` parameter of
#'   the Student-t parameterization given at
#'   <https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html>. # nolint
#' @param s_mu Numeric of length 1,
#'   prior standard deviation of `mu`.
#' @param d_tau Positive numeric of length 1. Degrees of freedom of the
#'   Student t prior of `tau` if `prior_tau` is `"half-t"`.
#' @param s_tau Non-negative numeric of length 1.
#'   If `prior_tau` is `"half-t"`, then `s_tau` is the scale parameter of
#'   the Student t prior of `tau` and analogous to the `sigma` parameter of
#'   the Student-t parameterization given at
#'   <https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html>. # nolint
#'   If `prior_tau` is `"uniform"`, then `s_tau` is the upper bound of `tau`.
#'   Upper bound on `tau` if `prior_tau` is `"uniform"`.
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
  prior_tau = "half-t",
  s_delta = 1,
  s_beta = 1,
  s_sigma = 1,
  s_mu = 1,
  s_tau = 1,
  d_tau = 4,
  alpha = NULL,
  delta = stats::rnorm(n = n_group - 1, mean = 0, sd = s_delta),
  beta = stats::rnorm(
    n = n_study * (n_continuous + n_binary),
    mean = 0,
    sd = s_delta
  ),
  sigma = stats::runif(n = n_study, min = 0, max = s_sigma),
  mu = stats::rnorm(n = 1, mean = 0, sd = s_mu),
  tau = NULL
) {
  true(n_study, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_group, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_patient, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_continuous, length(.) == 1, is.finite(.), is.numeric(.), . >= 0)
  true(n_binary, length(.) == 1, is.finite(.), is.numeric(.), . >= 0)
  true(
    prior_tau,
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    as.character(.) %in% c("half-t", "uniform")
  )
  true(n_study, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_delta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_beta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_sigma, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_mu, length(.) == 1, is.finite(.), is.numeric(.))
  true(s_tau, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(d_tau, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(delta, is.finite(.), is.numeric(.), length(.) == n_group - 1)
  true(beta, (all(is.finite(.)) || !length(beta)), is.numeric(.))
  true(length(beta) == n_study * (n_continuous + n_binary))
  true(sigma, all(is.finite(.)), is.numeric(.), length(.) == n_study)
  true(mu, is.numeric(.), is.finite(.), length(.) == 1)
  if (is.null(tau)) {
    if (identical(as.character(prior_tau), "half-t")) {
      tau <- abs(stats::rt(n = 1, df = d_tau)) * s_tau
    } else if (identical(as.character(prior_tau), "uniform")) {
      tau <- stats::runif(n = 1, min = 0, max = s_tau)
    }
  }
  true(tau, is.numeric(.), is.finite(.), length(.) == 1, . > 0)
  alpha <- if (is.null(alpha)) {
    alpha <- stats::rnorm(n = n_study, mean = mu, sd = tau)
  }
  true(alpha, is.finite(.), length(.) == n_study)
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
