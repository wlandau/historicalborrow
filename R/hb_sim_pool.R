#' @title Non-longitudinal pooled simulations.
#' @export
#' @family simulate
#' @description Simulate from the non-longitudinal pooled model.
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
#' @param n_study Number of studies to simulate.
#' @param n_group Number of groups (e.g. study arms) to simulate per study.
#' @param n_patient Number of patients to simulate per study per group.
#' @param n_continuous Number of continuous covariates to simulate
#'   (all from independent standard normal distributions).
#' @param n_binary Number of binary covariates to simulate
#'   (all from independent Bernoulli distributions with p = 0.5).
#' @param s_alpha Numeric of length 1, prior standard deviation
#'   of the study-specific control group mean parameters `alpha`.
#' @param s_delta Numeric of length 1, prior standard deviation
#'   of the study-by-group effect parameters `delta`.
#' @param s_beta Numeric of length 1, prior standard deviation
#'   of the fixed effects `beta`.
#' @param s_sigma Numeric of length 1, prior upper bound
#'   of the residual standard deviations.
#' @param alpha Numeric vector of length 1 for the pooled
#'   and mixture models and length `n_study` for the
#'   independent and hierarchical models.
#'   `alpha` is the vector of control group mean parameters.
#'   `alpha` enters the model by multiplying with
#'   `$matrices$x_alpha` (see the return value).
#'   The control group in the data is the one with the
#'   `group` column equal to 1.
#' @param delta Numeric vector of length `n_group - 1`
#'   of treatment effect parameters.
#'   `delta` enters the model by multiplying with
#'   `$matrices$x_delta` (see the return value).
#'   The control (non-treatment) group in the data is the one with the
#'   `group` column equal to 1.
#' @param beta Numeric vector of `n_study * (n_continuous + n_binary)`
#'   fixed effect parameters. Within each study,
#'   the first `n_continuous` betas
#'   are for the continuous covariates, and the rest are for
#'   the binary covariates. All the `beta`s for one study
#'   appear before all the `beta`s for the next study,
#'   and studies are arranged in increasing order of
#'   the sorted unique values in `$data$study` in the output.
#'   `betas` enters the model by multiplying with
#'   `$matrices$x_alpha` (see the return value).
#' @param sigma Numeric vector of `n_study` study-specific
#'   residual standard deviations.
#' @examples
#' hb_sim_pool(n_continuous = 1)$data
hb_sim_pool <- function(
  n_study = 5,
  n_group = 3,
  n_patient = 100,
  n_continuous = 0,
  n_binary = 0,
  s_alpha = 1,
  s_delta = 1,
  s_beta = 1,
  s_sigma = 1,
  alpha = stats::rnorm(n = 1, mean = 0, sd = s_alpha),
  delta = stats::rnorm(n = n_group - 1, mean = 0, sd = s_delta),
  beta = stats::rnorm(
    n = n_study * (n_continuous + n_binary),
    mean = 0,
    sd = s_delta
  ),
  sigma = stats::runif(n = n_study, min = 0, max = s_sigma)
) {
  true(n_study, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_group, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_patient, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(n_continuous, length(.) == 1, is.finite(.), is.numeric(.), . >= 0)
  true(n_binary, length(.) == 1, is.finite(.), is.numeric(.), . >= 0)
  true(n_study, length(.) == 1, is.finite(.), is.numeric(.), (. > 0))
  true(s_alpha, length(.) == 1, is.finite(.), is.numeric(.), (. > 0))
  true(s_delta, length(.) == 1, is.finite(.), is.numeric(.), (. > 0))
  true(s_beta, length(.) == 1, is.finite(.), is.numeric(.), (. > 0))
  true(s_sigma, length(.) == 1, is.finite(.), is.numeric(.), (. > 0))
  true(alpha, length(.) == 1, is.finite(.))
  true(delta, is.finite(.), is.numeric(.), length(.) == n_group - 1)
  true(beta, all(is.finite(.)) || !length(.), is.numeric(.))
  true(length(beta) == n_study * (n_continuous + n_binary))
  true(sigma, is.finite(.), is.numeric(.), length(.) == n_study)
  data <- hb_sim_grid(n_study, n_group, n_patient)
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
    sigma = sigma
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
