#' @title Non-longitudinal mixture simulations.
#' @export
#' @family simulate
#' @description Simulate from the non-longitudinal mixture model.
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
#' @param beta Numeric vector of `n_continuous + n_binary`
#'   fixed effect parameters.
#'   The first `n_continuous` betas
#'   are for the continuous covariates, and the rest are for
#'   the binary covariates.
#'   `betas` enters the model by multiplying with
#'   `$matrices$x_alpha` (see the return value).
#' @param pi Integer of length 1,
#'   index of the mixture component randomly
#'   chosen for `alpha`.
#' @param omega Numeric of length `n_study`,
#'   Candidate placebo mean parameters
#'   drawn from each of the mixture components.
#' @param m_omega Numeric of length 1 or `n_study`,
#'   prior control group mean of each study.
#'   If length `n_study`,
#'   then the last element corresponds to the current study,
#'   and the others are for historical studies.
#' @param s_omega Numeric of length 1 or `n_study`,
#'   prior control group standard deviation of each study.
#'   If length `n_study`,
#'   the the last element corresponds to the current study,
#'   and the others are for historical studies.
#' @param p_omega Numeric of length `n_study`,
#'   prior mixture proportion of each study.
#'   If length `n_study`,
#'   then the last element corresponds to the current study,
#'   and the others are for historical studies.
#' @examples
#' hb_sim_mixture()$data
hb_sim_mixture <- function(
  n_study = 5,
  n_group = 3,
  n_patient = 100,
  n_continuous = 0,
  n_binary = 0,
  s_delta = 1,
  s_beta = 1,
  s_sigma = 1,
  m_omega = 0,
  s_omega = 1,
  p_omega = 1 / n_study,
  alpha = omega[pi],
  delta = stats::rnorm(n = n_group - 1, mean = 0, sd = s_delta),
  beta = stats::rnorm(n = n_continuous + n_binary, mean = 0, sd = s_delta),
  sigma = stats::runif(n = 1, min = 0, max = s_sigma),
  pi = sample.int(n = n_study, size = 1, prob = p_omega),
  omega = stats::rnorm(n = n_study, mean = m_omega, sd = s_omega)
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
  true(m_omega, is.numeric(.), is.finite(.))
  true(s_omega, is.numeric(.), is.finite(.), . > 0)
  true(p_omega, is.numeric(.), is.finite(.), . >= 0, . <= 1)
  m_omega <- if_any(length(m_omega) == 1, rep(m_omega, n_study), m_omega)
  s_omega <- if_any(length(s_omega) == 1, rep(s_omega, n_study), s_omega)
  p_omega <- if_any(length(p_omega) == 1, rep(p_omega, n_study), p_omega)
  true(length(m_omega) == n_study)
  true(length(s_omega) == n_study)
  true(p_omega, length(.) == n_study, sum(.) == 1)
  true(alpha, length(.) == 1, is.finite(.), is.numeric(.))
  true(delta, is.finite(.), is.numeric(.))
  true(length(delta) == n_group - 1)
  true(beta, (all(is.finite(.)) || !length(.)), is.numeric(.))
  true(length(beta) == n_continuous + n_binary)
  true(sigma, length(.) == 1, is.finite(.), is.numeric(.))
  data <- hb_sim_grid(n_study = 1, n_group, n_patient)
  x_alpha <- get_x_alpha_pool_or_mixture(data)
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
    pi = pi,
    omega = omega
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
