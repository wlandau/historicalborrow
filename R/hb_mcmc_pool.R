#' @title Non-longitudinal pooled MCMC
#' @export
#' @family mcmc
#' @description Run the non-longitudinal pooled model with MCMC.
#' @return A tidy data frame of parameter samples from the
#'   posterior distribution. Columns `.chain`, `.iteration`,
#'   and `.draw` have the meanings documented in the
#'   `posterior` package.
#' @inheritParams hb_data
#' @inheritParams hb_sim_pool
#' @param data Tidy data frame with one row per patient,
#'   indicator columns for the response variable,
#'   study, group, and patient,
#'   and covariates. All columns must be atomic vectors
#'   (e.g. not lists). The data for the mixture and simple models
#'   should have just one study,
#'   and the others should have
#'   data from more than one study. The simple model can be used
#'   to get the historical data components of `m_omega` and `s_omega`
#'   for the mixture model.
#' @param n_chains Number of MCMC chains to run.
#' @param n_adapt Number of adaptation iterations to run.
#' @param n_warmup Number of warmup iterations per chain to run.
#' @param n_iterations Number of saved MCMC iterations per chain to run.
#' @param quiet Logical of length 1, `TRUE` to suppress R console output.
#' @examples
#' if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
#' data <- hb_sim_pool(n_continuous = 2)$data
#' hb_mcmc_pool(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
#' }
hb_mcmc_pool <- function(
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
    study = data$study,
    n_data = nrow(data),
    n_alpha = ncol(x_alpha),
    n_delta = ncol(x_delta),
    n_beta = ncol(x_beta),
    n_study = length(unique(data$study)),
    s_alpha = s_alpha,
    s_delta = s_delta,
    s_beta = s_beta,
    s_sigma = s_sigma,
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta
  )
  file <- "pool_beta.jags"
  variables <- c("alpha", "delta", "beta", "sigma")
  if (!prod(dim(x_beta))) {
    data_list$n_beta <- NULL
    data_list$s_beta <- NULL
    data_list$x_beta <- NULL
    file <- "pool_nobeta.jags"
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
