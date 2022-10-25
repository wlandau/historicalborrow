#' @title Model ensemble summary
#' @export
#' @family summary
#' @description Summarize a weighted average of the pooled and independent
#'   models.
#' @details The `hb_summary_weighted()` function is just like [hb_summary()],
#'   except the summarized posterior is a mixture between the independent
#'   and pooled model posteriors. The method is documented in the vignettes,
#'   and the technique is based on the work of Best et al. (2021)
#'   and Gronau et al. (2020).
#' @references
#'   * Best N, Price RG, Pouliquen IJ, Keene ON.
#'     Assessing efficacy in important subgroups in confirmatory trials:
#'     An example using Bayesian dynamic borrowing.
#'     Pharm Stat. 2021 May;20(3):551-562. doi: 10.1002/pst.2093.
#'     Epub 2021 Jan 21. PMID: 33475231; PMCID: PMC8247867.
#'     <https://onlinelibrary.wiley.com/doi/10.1002/pst.2093>.
#'   * Gronau QF, Singmann H, Wagenmakers E (2020). “bridgesampling:
#'     An R Package for Estimating Normalizing Constants.” _Journal of
#'     Statistical Software_, *92*(10), 1-29.
#'     doi:10.18637/jss.v092.i10
#'     <https://doi.org/10.18637/jss.v092.i10>.
#' @return A tidy data frame with one row per group per weight
#'   and the columns in the following list. Unless otherwise specified,
#'   the quantities are calculated at the group level.
#'   Some are calculated for the current (non-historical) study only,
#'   while others pertain to the combined dataset which includes
#'   all historical studies.
#'   The mixture model is an exception because the `data` argument
#'   only includes the current study, so other quantities that include
#'   historical information will need to borrow from an `hb_summary()`
#'   call on one of the other models.
#'   * `group`: group label.
#'   * `data_mean`: observed mean response specific to the current study.
#'   * `data_sd`: observed standard deviation of the response
#'     specific to the current study.
#'   * `data_lower`: lower bound of a simple frequentist 95% confidence
#'     interval of the observed mean specific to the current study.
#'   * `data_upper`: upper bound of a simple frequentist 95% confidence
#'     interval of the observed mean specific to the current study.
#'   * `data_n`: number of non-missing observations in the combined dataset
#'     with all studies.
#'   * `data_N`: total number of observations (missing and non-missing)
#'     in the combined dataset with all studies.
#'   * `data_n_study_*`: number of non-missing observations separately
#'     for each study.
#'     The suffixes of these column names are integer study indexes.
#'     Call `dplyr::distinct(hb_data(your_data), study, study_label)`
#'     to see which study labels correspond to these integer indexes.
#'     Note: the combined dataset for the mixture model
#'     is just the current study. If all the `data_n_study_*` results
#'     across all studies
#'     are desired, then call `hb_summary()` on a different model (e.g. pooled).
#'   * `data_N_study_*`: same as `data_n_study_*` except both missing and
#'     non-missing observations are counted (total number of observations).
#'   * `response_mean`: Estimated posterior mean of the response
#'     from the model specific to the current study.
#'     Typically, the raw response is change from baseline,
#'     in which case `response_mean` is estimating change from baseline.
#'   * `response_sd`: Estimated posterior standard deviation of the mean
#'     response from the model specific to the current study.
#'   * `response_variance`: Estimated posterior variance of the mean
#'     response from the model specific to the current study.
#'   * `response_lower`: Lower bound of a 95% posterior interval on the mean
#'     response from the model specific to the current study.
#'   * `response_upper`: Upper bound of a 95% posterior interval on the mean
#'     response from the model specific to the current study.
#'   * `response_mean_mcse`: Monte Carlo standard error of `response_mean`.
#'   * `response_sd_mcse`: Monte Carlo standard error of `response_sd`.
#'   * `response_lower_mcse`: Monte Carlo standard error of `response_lower`.
#'   * `response_upper_mcse`: Monte Carlo standard error of `response_upper`.
#'   * `diff_mean`: Estimated treatment effect from the model
#'     specific to the current study.
#'   * `diff_lower`: Lower bound of a 95% posterior interval on the treatment
#'     effect from the model specific to the current study..
#'   * `diff_upper`: Upper bound of a 95% posterior interval on the treatment
#'     effect from the model specific to the current study..
#'   * `diff_mean_mcse`: Monte Carlo standard error of `diff_mean`.
#'   * `diff_lower_mcse`: Monte Carlo standard error of `diff_lower`.
#'   * `diff_upper_mcse`: Monte Carlo standard error of `diff_upper`.
#'   * `P(diff > EOI)`, `P(diff < EOI)`: CSF probabilities on the
#'     treatment effect specified with the `eoi` and `direction`
#'     arguments. Specific to the current study.
#'   * `effect_mean`: Estimated posterior mean of effect size
#'     (treatment difference divided by residual standard deviation).
#'     Specific to the current study.
#'   * `effect_lower`: Lower bound of a 95% posterior interval of effect size
#'     from the model. Specific to the current study.
#'   * `effect_upper`: Upper bound of a 95% posterior interval of effect size
#'     from the model. Specific to the current study.
#'   * `weight`: weight on the pooled model used in the weighted average.
#' @inheritParams hb_summary
#' @param mcmc_pool A wide data frame of posterior samples returned by
#'   [hb_mcmc_pool()].
#'   Must have the same number of rows as `mcmc_independent`.
#' @param mcmc_independent A wide data frame of posterior samples returned by
#'   [hb_mcmc_independent()].
#'   Must have the same number of rows as `mcmc_pool`.
#' @param weights Numeric vector of weights 1 between 0 and 1.
#'   Each is a model averaging mixture weight on the pooled model.
#'   The function returns one row group for each weight in the vector.
#' @examples
#' if (!identical(Sys.getenv("HB_TEST", unset = ""), "")) {
#' data <- hb_sim_independent(n_study = 2)$data
#' data$group <- sprintf("group%s", data$group)
#' mcmc_pool <- hb_mcmc_pool(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
#' mcmc_independent <- hb_mcmc_independent(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
#' hb_summary_weighted(
#'   mcmc_pool = mcmc_pool,
#'   mcmc_independent = mcmc_independent, 
#'   data = data,
#'   weights = c(0.5, 0.75)
#' )
#' }
hb_summary_weighted <- function(
  mcmc_pool,
  mcmc_independent,
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient",
  covariates = grep("^covariate", colnames(data), value = TRUE),
  eoi = 0,
  direction = "<",
  weights = 0.5
) {
  true(mcmc_pool, is.data.frame(.), !is.null(colnames(.)))
  true(mcmc_independent, is.data.frame(.), !is.null(colnames(.)))
  true(nrow(mcmc_pool) == nrow(mcmc_independent))
  true(eoi, is.numeric(.), is.finite(.))
  true(all(direction %in% c(">", "<")))
  true(length(eoi) == length(direction))
  true(length(eoi) > 0)
  true(weights, is.numeric(.), is.finite(.), . >= 0, . <= 1)
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
  x_alpha_pool <- get_x_alpha_pool_or_mixture(data)
  x_alpha_independent <- get_x_alpha(data)
  x_delta <- get_x_delta(data)
  x_beta_pool <- get_x_beta(
    data = data,
    x_alpha = x_alpha_pool,
    x_delta = x_delta
  )
  x_beta_independent <- get_x_beta(
    data = data,
    x_alpha = x_alpha_independent,
    x_delta = x_delta
  )
  samples_response_pool <- get_samples_response(
    mcmc = mcmc_pool,
    data = data,
    x_alpha = x_alpha_pool,
    x_delta = x_delta,
    x_beta = x_beta_pool
  )
  samples_response_pool <- dplyr::rename(
    samples_response_pool,
    value_pool = value
  )
  samples_response_independent <- get_samples_response(
    mcmc = mcmc_independent,
    data = data,
    x_alpha = x_alpha_independent,
    x_delta = x_delta,
    x_beta = x_beta_independent
  )
  samples_response_independent <- dplyr::rename(
    samples_response_independent,
    value_independent = value
  )
  samples_response_proto <- dplyr::left_join(
    x = samples_response_pool,
    y = samples_response_independent,
    by = c("study", "group", "sample")
  )
  samples_sigma_pool <- get_samples_sigma(mcmc_pool)
  samples_sigma_pool <- dplyr::rename(
    samples_sigma_pool,
    value_pool = value
  )
  samples_sigma_independent <- get_samples_sigma(mcmc_independent)
  samples_sigma_independent <- dplyr::rename(
    samples_sigma_independent,
    value_independent = value
  )
  samples_sigma_proto <- dplyr::left_join(
    x = samples_sigma_pool,
    y = samples_sigma_independent,
    by = "sample"
  )
  out_list <- list()
  index <- 1L
  for (weight in weights) {
    samples_response <- samples_response_proto
    samples_response$value <- (weight * samples_response$value_pool) +
      ((1 - weight) * samples_response$value_independent)
    samples_response$value_pool <- NULL
    samples_response$value_independent <- NULL
    samples_diff <- get_samples_diff(samples_response)
    samples_sigma <- samples_sigma_proto
    samples_sigma$value <- (weight * samples_sigma$value_pool) +
      ((1 - weight) * samples_sigma$value_independent)
    samples_sigma$value_pool <- NULL
    samples_sigma$value_independent <- NULL
    samples_effect <- get_samples_effect(samples_diff, samples_sigma)
    table_data <- get_table_data(data)
    table_data_n_study <- get_table_data_n_study(data)
    table_data_N_study <- get_table_data_N_study(data)
    table_data_current <- get_table_data_current(data)
    table_response <- get_table_response(samples_response)
    table_diff <- get_table_diff(samples_diff)
    table_eoi <- get_table_eoi(samples_diff, eoi, direction)
    table_effect <- get_table_effect(samples_effect)
    out <- tibble::tibble(group = sort(unique(samples_response$group)))
    out <- dplyr::left_join(out, y = table_data, by = "group")
    out <- dplyr::left_join(out, y = table_data_n_study, by = "group")
    out <- dplyr::left_join(out, y = table_data_N_study, by = "group")
    out <- dplyr::left_join(out, y = table_data_current, by = "group")
    out <- dplyr::left_join(out, y = table_response, by = "group")
    out <- dplyr::left_join(out, y = table_diff, by = "group")
    out <- dplyr::left_join(out, y = table_eoi, by = "group")
    out <- dplyr::left_join(out, y = table_effect, by = "group")
    groups <- dplyr::distinct(data, group, group_label)
    out <- dplyr::left_join(x = out, y = groups, by = "group")
    out$weight <- weight
    out_list[[index]] <- dplyr::select(
      out,
      weight,
      group,
      group_label,
      tidyselect::everything()
    )
    index <- index + 1L
  }
  dplyr::bind_rows(out_list)
}
