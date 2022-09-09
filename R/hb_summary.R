#' @title Model summary
#' @export
#' @family summary
#' @description Summarize a fitted model in a table.
#' @details The `hb_summary()` function post-processes the results from
#'   the model. It accepts MCMC samples of parameters and returns
#'   interpretable group-level posterior summaries such as change
#'   from baseline response and treatment effect. To arrive at these
#'   summaries, `hb_summary()` computes marginal posteriors of
#'   transformed parameters. The transformations derive patient-level
#'   fitted values from model parameters, then derive group-level
#'   responses as averages of fitted values. We refer to this style
#'   of estimation as "unconditional estimation", as opposed to
#'   "conditional estimation", which takes each group mean to be the
#'   appropriate linear combination of the relevant `alpha` and `delta`
#'   parameters, without using `beta` components or going through fitted
#'   values. If the baseline covariates are balanced across studies,
#'   unconditional and conditional estimation should produce similar
#'   estimates of placebo and treatment effects.
#' @return A tidy data frame with one row per group (e.g. treatment arm)
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
#'   * `precision_ratio`: For the hierarchical model only,
#'     a model-based mean of the precision ratio. Specific to the current study.
#'   * `precision_ratio_lower`: For the hierarchical model only, lower bound
#'     of a model-based 95% posterior interval of the precision ratio.
#'     Specific to the current study.
#'   * `precision_ratio_upper`: For the hierarchical model only, upper bound
#'     of a model-based 95% posterior interval of the precision ratio.
#'     Specific to the current study.
#'   * `mix_prop_*`: For the mixture model only, posterior mixture proportions
#'     of each of the mixture components. The last one is for the current study
#'     and the first ones are for the historical studies. The suffixes of these
#'     column names are the integer study indexes.
#'     Call `dplyr::distinct(hb_data(your_data), study, study_label)`
#'     to see which study labels correspond to these integer indexes.
#' @inheritParams hb_mcmc_pool
#' @param mcmc A wide data frame of posterior samples returned by
#'   [hb_mcmc_hierarchical()] or similar MCMC function.
#' @param eoi Numeric of length at least 1,
#'   vector of effects of interest (EOIs) for critical success factors (CSFs).
#' @param direction Character of length `length(eoi)` indicating how
#'   to compare the treatment effect to each EOI. `">"` means
#'   Prob(treatment effect > EOI), and `"<"` means
#'   Prob(treatment effect < EOI). All elements of `direction`
#'   must be either `">"` or `"<"`.
#' @examples
#' data <- hb_sim_pool(n_continuous = 2)$data
#' data$group <- sprintf("group%s", data$group)
#' mcmc <- hb_mcmc_pool(
#'   data,
#'   n_chains = 1,
#'   n_adapt = 100,
#'   n_warmup = 50,
#'   n_iterations = 50
#' )
#' hb_summary(mcmc, data)
hb_summary <- function(
  mcmc,
  data,
  response = "response",
  study = "study",
  study_reference = max(data[[study]]),
  group = "group",
  group_reference = min(data[[group]]),
  patient = "patient",
  covariates = grep("^covariate", colnames(data), value = TRUE),
  eoi = 0,
  direction = "<"
) {
  true(mcmc, is.data.frame(.), !is.null(colnames(.)))
  true(eoi, is.numeric(.), is.finite(.))
  true(all(direction %in% c(">", "<")))
  true(length(eoi) == length(direction))
  true(length(eoi) > 0)
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
  x_alpha <- if_any(
    sum(grepl("^alpha", colnames(mcmc))) == 1,
    get_x_alpha_pool_or_mixture(data),
    get_x_alpha(data)
  )
  x_delta <- get_x_delta(data)
  x_beta <- get_x_beta(data = data, x_alpha = x_alpha, x_delta = x_delta)
  samples_response <- get_samples_response(
    mcmc = mcmc,
    data = data,
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = x_beta
  )
  samples_diff <- get_samples_diff(samples_response)
  samples_sigma <- get_samples_sigma(mcmc)
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
  if ("precision_ratio" %in% colnames(mcmc)) {
    table_precision_ratio <- get_table_precision_ratio(mcmc)
    out <- dplyr::left_join(out, y = table_precision_ratio, by = "group")
  }
  if (any(grepl("^post_p", colnames(mcmc)))) {
    table_mix_prop <- get_table_mix_prop(mcmc)
    out <- dplyr::left_join(out, y = table_mix_prop, by = "group")
  }
  groups <- dplyr::distinct(data, group, group_label)
  out <- dplyr::left_join(x = out, y = groups, by = "group")
  dplyr::select(out, group, group_label, tidyselect::everything())
}

get_samples_response <- function(mcmc, data, x_alpha, x_delta, x_beta) {
  index_max <- data$study == max(data$study)
  data <- data[index_max,, drop = FALSE] # nolint
  x_alpha <- x_alpha[index_max,, drop = FALSE] # nolint
  x_delta <- x_delta[index_max,, drop = FALSE] # nolint
  x_beta <- x_beta[index_max,, drop = FALSE] # nolint
  alpha <- t(as.matrix(mcmc[, grepl("^alpha", colnames(mcmc)), drop = FALSE]))
  delta <- t(as.matrix(mcmc[, grepl("^delta", colnames(mcmc)), drop = FALSE]))
  beta <- t(as.matrix(mcmc[, grepl("^beta", colnames(mcmc)), drop = FALSE]))
  gc()
  x_alpha <- Matrix::Matrix(x_alpha, sparse = TRUE)
  x_delta <- Matrix::Matrix(x_delta, sparse = TRUE)
  x_beta <- Matrix::Matrix(x_beta, sparse = TRUE)
  gc()
  fitted <- x_alpha %*% alpha
  rm(alpha)
  rm(x_alpha)
  gc()
  fitted <- fitted + x_delta %*% delta
  rm(delta)
  rm(x_delta)
  gc()
  fitted <- fitted + x_beta %*% beta
  rm(beta)
  rm(x_beta)
  gc()
  groups <- tibble::tibble(
    study = data$study,
    group = data$group
  )
  groups$index <- paste(groups$study, groups$group)
  groups$index <- ordered(groups$index, levels = unique(groups$index))
  unique_groups <- groups[!duplicated(groups$index), ]
  out <- apply(fitted, 2, function(sample) tapply(sample, groups$index, mean))
  rm(fitted)
  gc()
  colnames(out) <- paste0("sample", seq_len(ncol(out)))
  out <- tibble::as_tibble(out)
  out$study <- unique_groups$study
  out$group <- unique_groups$group
  tidyr::pivot_longer(
    data = out,
    cols = tidyselect::starts_with("sample"),
    names_to = "sample"
  )
}

get_samples_diff <- function(samples_response) {
  control <- dplyr::filter(samples_response, group == min(group))
  control <- dplyr::rename(control, value_control = value)
  control$group <- NULL
  treatment <- dplyr::filter(samples_response, group > min(group))
  out <- dplyr::left_join(
    x = treatment,
    y = control,
    by = c("study", "sample")
  )
  out$value <- out$value - out$value_control
  out$value_control <- NULL
  out
}

get_samples_sigma <- function(mcmc) {
  out <- dplyr::select(mcmc, tidyselect::starts_with("sigma"))
  out$sample <- paste0("sample", seq_len(nrow(out)))
  out <- tidyr::pivot_longer(out, tidyselect::starts_with("sigma"))
  if (length(unique(out$name)) > 1) {
    out$study <- as.integer(gsub("sigma\\[|\\]", "", out$name))
    out <- dplyr::filter(out, study == max(study))
    out$study <- NULL
  }
  out$name <- NULL
  out
}

get_samples_effect <- function(samples_diff, samples_sigma) {
  samples_diff$value_diff <- samples_diff$value
  samples_diff$value <- NULL
  samples_sigma$value_sigma <- samples_sigma$value
  samples_sigma$value <- NULL
  out <- dplyr::left_join(x = samples_diff, y = samples_sigma, by = "sample")
  out$value <- out$value_diff / out$value_sigma
  out$value_diff <- NULL
  out$value_sigma <- NULL
  out
}

get_table_data <- function(data) {
  dplyr::summarize(
    dplyr::group_by(data, group),
    data_n = sum(!is.na(response)),
    data_N = dplyr::n(),
    .groups = "drop"
  )
}

get_table_data_current <- function(data) {
  out <- dplyr::summarize(
    dplyr::group_by(dplyr::filter(data, study == max(study)), group),
    data_mean = mean(response, na.rm = TRUE),
    data_sd = sd(response, na.rm = TRUE),
    n = sum(!is.na(response)),
    data_lower = data_mean - stats::qnorm(0.975) * data_sd / sqrt(n),
    data_upper = data_mean + stats::qnorm(0.975) * data_sd / sqrt(n),
    .groups = "drop"
  )
  out$n <- NULL
  out
}


get_table_data_n_study <- function(data) {
  out <- dplyr::summarize(
    dplyr::group_by(data, study, group),
    n = sum(!is.na(response)),
    .groups = "drop"
  )
  tidyr::pivot_wider(
    out,
    names_from = "study",
    values_from = "n",
    names_prefix = "data_n_study_",
    values_fill = 0L
  )
}

get_table_data_N_study <- function(data) {
  out <- dplyr::summarize(
    dplyr::group_by(data, study, group),
    n = dplyr::n(),
    .groups = "drop"
  )
  tidyr::pivot_wider(
    out,
    names_from = "study",
    values_from = "n",
    names_prefix = "data_N_study_",
    values_fill = 0L
  )
}

get_table_response <- function(samples_response) {
  dplyr::summarize(
    dplyr::group_by(samples_response, group),
    response_mean = mean(value),
    response_variance = stats::var(value),
    response_sd = stats::sd(value),
    response_lower = quantile(value, 0.025),
    response_upper = quantile(value, 0.975),
    response_mean_mcse = posterior::mcse_mean(value),
    response_sd_mcse = posterior::mcse_sd(value),
    response_lower_mcse = posterior::mcse_quantile(value, 0.025),
    response_upper_mcse = posterior::mcse_quantile(value, 0.975),
    .groups = "drop"
  )
}

get_table_diff <- function(samples_diff) {
  dplyr::summarize(
    dplyr::group_by(samples_diff, group),
    diff_mean = mean(value),
    diff_lower = quantile(value, 0.025),
    diff_upper = quantile(value, 0.975),
    diff_mean_mcse = posterior::mcse_mean(value),
    diff_lower_mcse = posterior::mcse_quantile(value, 0.025),
    diff_upper_mcse = posterior::mcse_quantile(value, 0.975),
    .groups = "drop"
  )
}

get_table_eoi <- function(samples_diff, eoi, direction) {
  out <- list()
  for (index in seq_along(eoi)) {
    section <- dplyr::summarize(
      dplyr::group_by(samples_diff, group),
      value = if_any(
        direction[index] == ">",
        mean(value > eoi[index]),
        mean(value < eoi[index])
      )
    )
    name <- sprintf("P(diff %s %s)", direction[index], eoi[index])
    section[[name]] <- section$value
    section$value <- NULL
    out[[index]] <- section
  }
  Reduce(f = function(x, y) dplyr::left_join(x, y, by = "group"), x = out)
}

get_table_effect <- function(samples_effect) {
  dplyr::summarize(
    dplyr::group_by(samples_effect, group),
    effect_mean = mean(value),
    effect_lower = quantile(value, 0.025),
    effect_upper = quantile(value, 0.975),
    effect_mean_mcse = posterior::mcse_mean(value),
    effect_lower_mcse = posterior::mcse_quantile(value, 0.025),
    effect_upper_mcse = posterior::mcse_quantile(value, 0.975),
    .groups = "drop"
  )
}

get_table_precision_ratio <- function(mcmc) {
  dplyr::summarize(
    mcmc,
    group = 1,
    precision_ratio = mean(mcmc$precision_ratio),
    precision_ratio_lower = quantile(mcmc$precision_ratio, 0.025),
    precision_ratio_upper = quantile(mcmc$precision_ratio, 0.975)
  )
}

get_table_mix_prop <- function(mcmc) {
  out <- dplyr::summarize(
    mcmc,
    dplyr::across(tidyselect::starts_with("post_p"), mean)
  )
  colnames(out) <- paste0("mix_prop_", seq_len(ncol(out)))
  out$group <- 1
  out
}
