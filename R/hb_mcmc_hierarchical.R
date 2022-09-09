#' @title Hierarchical model MCMC
#' @export
#' @family mcmc
#' @description Run the hierarchical model with MCMC.
#' @return A tidy data frame of parameter samples from the
#'   posterior distribution. Columns `.chain`, `.iteration`,
#'   and `.draw` have the meanings documented in the
#'   `posterior` package.
#' @inheritParams hb_sim_hierarchical
#' @inheritParams hb_mcmc_pool
#' @param s_mu Positive numeric of length 1,
#'   hyperparameter: prior standard deviation of the
#'   mean `mu` of the study-level control means `alpha`.
#' @param s_tau Positive numeric of length 1,
#'   hyperparameter: uniform  prior upper bound of the
#'   standard deviation `tau` of the study-level control means `alpha`.
#' @examples
#' if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
#' data <- hb_sim_hierarchical(n_continuous = 2)$data
#' hb_mcmc_hierarchical(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
#' }
hb_mcmc_hierarchical <- function(
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
  s_mu = 30,
  s_tau = 30,
  n_chains = 4,
  n_adapt = 2e3,
  n_warmup = 4e3,
  n_iterations = 2e4,
  quiet = TRUE
) {
  true(s_delta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_beta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_sigma, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_mu, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_tau, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
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
    length(unique(data$study)) > 1,
    message = "hierarchical model data should have more than one study."
  )
  x_alpha <- get_x_alpha(data)
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
    study = data$study,
    n_data = nrow(data),
    n_study = length(unique(data$study)),
    n_alpha = ncol(x_alpha),
    n_delta = ncol(x_delta),
    n_beta = ncol(x_beta),
    s_delta = s_delta,
    s_beta = s_beta,
    s_sigma = s_sigma,
    s_mu = s_mu,
    s_tau = s_tau,
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta,
    n_study_current = sum(
      (data$study == max(data$study)) & !is.na(data$response)
    )
  )
  file <- "hierarchical_beta.jags"
  variables <- c(
    "alpha",
    "delta",
    "beta",
    "sigma",
    "mu",
    "tau",
    "precision_ratio"
  )
  if (!prod(dim(x_beta))) {
    data_list$n_beta <- NULL
    data_list$s_beta <- NULL
    data_list$x_beta <- NULL
    file <- "hierarchical_nobeta.jags"
    variables <- c(
      "alpha",
      "delta",
      "sigma",
      "mu",
      "tau",
      "precision_ratio"
    )
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
