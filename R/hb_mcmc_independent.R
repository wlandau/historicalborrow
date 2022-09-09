#' @title Independent-study model MCMC
#' @export
#' @family mcmc
#' @description Run the independent-study model with MCMC.
#' @return A tidy data frame of parameter samples from the
#'   posterior distribution. Columns `.chain`, `.iteration`,
#'   and `.draw` have the meanings documented in the
#'   `posterior` package.
#' @inheritParams hb_sim_independent
#' @inheritParams hb_mcmc_pool
#' @examples
#' data <- hb_sim_independent(n_continuous = 2)$data
#' hb_mcmc_independent(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
hb_mcmc_independent <- function(
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient",
  covariates = grep("^covariate", colnames(data), value = TRUE),
  s_alpha = 30,
  s_delta = 30,
  s_beta = 30,
  s_sigma = 30,
  n_chains = 4,
  n_adapt = 2e3,
  n_warmup = 4e3,
  n_iterations = 2e4,
  quiet = TRUE
) {
  true(s_alpha, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_delta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_beta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_sigma, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
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
    s_alpha = s_alpha,
    s_delta = s_delta,
    s_beta = s_beta,
    s_sigma = s_sigma,
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta
  )
  file <- "independent_beta.jags"
  variables <- c("alpha", "delta", "beta", "sigma")
  if (!prod(dim(x_beta))) {
    data_list$n_beta <- NULL
    data_list$s_beta <- NULL
    data_list$x_beta <- NULL
    file <- "independent_nobeta.jags"
    variables <- c("alpha", "delta", "sigma")
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
