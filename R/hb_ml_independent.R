#' @title Marginal likelihood of the non-longitudinal
#'   independent model
#' @export
#' @family marginal-likelihood
#' @description Estimate the marginal likelihood of the
#'   non-longitudinal independent model using bridge sampling
#'   (`bridgesampling` R package).
#' @details Bridge sampling is described by Gronau et al. (2020).
#' @references Gronau QF, Singmann H, Wagenmakers E (2020). “bridgesampling:
#'   An R Package for Estimating Normalizing Constants.” _Journal of
#'   Statistical Software_, *92*(10), 1-29.
#'   doi:10.18637/jss.v092.i10
#'   <https://doi.org/10.18637/jss.v092.i10>.
#' @return A numeric of length 1 with the marignal likelihood from bridge
#'   sampling, with an object of class `"bridge"`
#'   from the `bridgesampling`
#'   package assigned as an attribute called `"bridge"`.
#' @inheritParams hb_data
#' @inheritParams hb_sim_independent
#' @inheritParams hb_mcmc_independent
#' @param mcmc A wide data frame of posterior samples returned by
#'   [hb_mcmc_independent()].
#' @examples
#' if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
#' data <- hb_sim_independent(n_continuous = 2)$data
#' mcmc <- hb_mcmc_independent(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50 # need a lot more in real life
#' )
#' suppressWarnings(hb_ml_independent(mcmc = mcmc, data = data))
#' }
hb_ml_independent <- function(
    mcmc,
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
    quiet = TRUE
) {
  true(s_alpha, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_delta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_beta, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
  true(s_sigma, length(.) == 1, is.finite(.), is.numeric(.), . > 0)
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
  if (!prod(dim(x_beta))) {
    data_list$n_beta <- NULL
    data_list$s_beta <- NULL
    data_list$x_beta <- NULL
  }
  hb_ml(mcmc = mcmc, data_list = data_list, quiet = quiet)
}
