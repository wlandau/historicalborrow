test_that("hb_summary() pool", {
  set.seed(0)
  data <- hb_sim_pool(n_continuous = 2)$data
  data$group <- sprintf("group%s", data$group)
  mcmc <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 100,
    n_iterations = 100
  )
  out <- hb_summary(mcmc, data, eoi = c(0, 1), direction = c(">", "<"))
  expect_equal(out$group_label, c("group1", "group2", "group3"))
  expect_equal(out$group, seq_len(3))
  expect_equal(dim(out), c(3, 41))
  cols <- c(
    "group", "group_label", "data_mean", "data_sd", "data_lower", "data_upper",
    "data_n", "data_N",
    "data_n_study_1", "data_n_study_2", "data_n_study_3", "data_n_study_4",
    "data_n_study_5",
    "data_N_study_1", "data_N_study_2", "data_N_study_3", "data_N_study_4",
    "data_N_study_5",
    "response_mean", "response_sd", "response_variance", "response_lower",
    "response_upper", "response_mean_mcse", "response_sd_mcse",
    "response_lower_mcse",  "response_upper_mcse", "diff_mean", "diff_lower",
    "diff_upper",  "diff_mean_mcse", "diff_lower_mcse", "diff_upper_mcse",
    "P(diff > 0)",  "P(diff < 1)", "effect_mean", "effect_lower",
    "effect_upper",  "effect_mean_mcse", "effect_lower_mcse",
    "effect_upper_mcse"
  )
  expect_equal(sort(cols), sort(colnames(out)))
})

test_that("hb_summary() independent", {
  set.seed(0)
  data <- hb_sim_independent(n_continuous = 2)$data
  data$group <- sprintf("group%s", data$group)
  mcmc <- hb_mcmc_independent(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 200,
    n_iterations = 200
  )
  out <- hb_summary(mcmc, data, eoi = c(0, 1), direction = c(">", "<"))
  expect_equal(out$group_label, c("group1", "group2", "group3"))
  expect_equal(out$group, seq_len(3))
  expect_equal(dim(out), c(3, 41))
  cols <- c(
    "group", "group_label", "data_mean", "data_lower", "data_upper",
    "data_n", "data_N", "data_sd",
    "data_n_study_1", "data_n_study_2", "data_n_study_3", "data_n_study_4",
    "data_n_study_5",
    "data_N_study_1", "data_N_study_2", "data_N_study_3", "data_N_study_4",
    "data_N_study_5",
    "response_mean", "response_sd", "response_variance", "response_lower",
    "response_upper", "response_mean_mcse", "response_sd_mcse",
    "response_lower_mcse",  "response_upper_mcse", "diff_mean", "diff_lower",
    "diff_upper",  "diff_mean_mcse", "diff_lower_mcse", "diff_upper_mcse",
    "P(diff > 0)",  "P(diff < 1)", "effect_mean", "effect_lower",
    "effect_upper",  "effect_mean_mcse", "effect_lower_mcse",
    "effect_upper_mcse"
  )
  expect_equal(sort(cols), sort(colnames(out)))
})

test_that("hb_summary() hierarchical", {
  set.seed(0)
  data <- hb_sim_hierarchical(n_continuous = 2)$data
  data$group <- sprintf("group%s", data$group)
  mcmc <- hb_mcmc_hierarchical(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 500,
    n_iterations = 500
  )
  out <- hb_summary(mcmc, data, eoi = c(0, 1), direction = c(">", "<"))
  expect_equal(out$group_label, c("group1", "group2", "group3"))
  expect_equal(out$group, seq_len(3))
  expect_equal(dim(out), c(3, 44))
  cols <- c(
    "group", "group_label", "data_mean", "data_lower", "data_upper",
    "data_n", "data_N", "data_sd",
    "data_n_study_1", "data_n_study_2", "data_n_study_3", "data_n_study_4",
    "data_n_study_5",
    "data_N_study_1", "data_N_study_2", "data_N_study_3", "data_N_study_4",
    "data_N_study_5",
    "response_mean", "response_sd", "response_variance", "response_lower",
    "response_upper", "response_mean_mcse", "response_sd_mcse",
    "response_lower_mcse",  "response_upper_mcse", "diff_mean", "diff_lower",
    "diff_upper",  "diff_mean_mcse", "diff_lower_mcse", "diff_upper_mcse",
    "P(diff > 0)",  "P(diff < 1)", "effect_mean", "effect_lower",
    "effect_upper",  "effect_mean_mcse", "effect_lower_mcse",
    "effect_upper_mcse",
    "precision_ratio", "precision_ratio_lower", "precision_ratio_upper"
  )
  expect_equal(sort(cols), sort(colnames(out)))
})

test_that("hb_summary() mixture", {
  data_all_studies <- hb_sim_independent(n_continuous = 2)$data
  hyperparameters <- hb_mcmc_mixture_hyperparameters(
    data = data_all_studies,
    response = "response",
    study = "study",
    study_reference = 5,
    group = "group",
    group_reference = 1,
    patient = "patient",
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  data <- dplyr::filter(data_all_studies, study == max(study))
  data$group <- sprintf("group%s", data$group)
  mcmc <- hb_mcmc_mixture(
    data = data,
    response = "response",
    study = "study",
    study_reference = 5,
    group = "group",
    group_reference = "group1",
    patient = "patient",
    m_omega = hyperparameters$m_omega,
    s_omega = hyperparameters$s_omega,
    p_omega = rep(1 / nrow(hyperparameters), nrow(hyperparameters)),
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 100,
    n_iterations = 200
  )
  out <- hb_summary(mcmc, data, eoi = c(0, 1), direction = c(">", "<"))
  expect_equal(out$group_label, c("group1", "group2", "group3"))
  expect_equal(out$group, seq_len(3))
  expect_equal(dim(out), c(3, 38))
  cols <- c(
    "group", "group_label", "data_mean", "data_lower", "data_upper",
    "data_n", "data_N", "data_n_study_1", "data_N_study_1", "data_sd",
    "response_mean", "response_sd", "response_variance", "response_lower",
    "response_upper", "response_mean_mcse", "response_sd_mcse",
    "response_lower_mcse",  "response_upper_mcse", "diff_mean", "diff_lower",
    "diff_upper",  "diff_mean_mcse", "diff_lower_mcse", "diff_upper_mcse",
    "P(diff > 0)",  "P(diff < 1)", "effect_mean", "effect_lower",
    "effect_upper",  "effect_mean_mcse", "effect_lower_mcse",
    "effect_upper_mcse",
    sprintf("mix_prop_%s", seq_len(5))
  )
  expect_equal(sort(cols), sort(colnames(out)))
  for (i in seq_len(5)) {
    expect_equal(
      out[[sprintf("mix_prop_%s", i)]][1],
      mean(mcmc[[sprintf("post_p[%s]", i)]])
    )
  }
})

test_that("hb_summary() data counts", {
  set.seed(0)
  data <- hb_sim_pool(
    n_continuous = 1,
    n_binary = 0,
    n_study = 2,
    n_group = 2,
    n_patient = 3
  )$data
  data$response[1] <- NA
  mcmc <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 100,
    n_iterations = 100
  )
  out <- hb_summary(mcmc, data, eoi = c(0, 1), direction = c(">", "<"))
  expect_equal(out$data_n, c(5, 3))
  expect_equal(out$data_N, c(6, 3))
  expect_equal(out$data_n_study_1, c(2, 0))
  expect_equal(out$data_n_study_2, c(3, 3))
  expect_equal(out$data_N_study_1, c(3, 0))
  expect_equal(out$data_N_study_2, c(3, 3))
})

test_that("hb_summary() data summaries", {
  set.seed(0)
  data <- hb_sim_pool(
    n_continuous = 1,
    n_binary = 0,
    n_study = 2,
    n_group = 2,
    n_patient = 3
  )$data
  mcmc <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  out <- hb_summary(mcmc = mcmc, data = data)
  expect_equal(
    out$data_mean,
    c(
      mean(data$response[data$study == 2 & data$group == 1]),
      mean(data$response[data$study == 2 & data$group == 2])
    )
  )
  expect_equal(
    out$data_sd,
    c(
      sd(data$response[data$study == 2 & data$group == 1]),
      sd(data$response[data$study == 2 & data$group == 2])
    )
  )
  expect_equal(
    out$data_lower,
    c(
      mean(data$response[data$study == 2 & data$group == 1]) -
        stats::qnorm(0.975) *
          sd(data$response[data$study == 2 & data$group == 1]) /
          sqrt(length(data$response[data$study == 2 & data$group == 1])),
      mean(data$response[data$study == 2 & data$group == 2]) -
        stats::qnorm(0.975) *
          sd(data$response[data$study == 2 & data$group == 2]) /
          sqrt(length(data$response[data$study == 2 & data$group == 2]))
    )
  )
  expect_equal(
    out$data_upper,
    c(
      mean(data$response[data$study == 2 & data$group == 1]) +
        stats::qnorm(0.975) *
          sd(data$response[data$study == 2 & data$group == 1]) /
          sqrt(length(data$response[data$study == 2 & data$group == 1])),
      mean(data$response[data$study == 2 & data$group == 2]) +
        stats::qnorm(0.975) *
          sd(data$response[data$study == 2 & data$group == 2]) /
          sqrt(length(data$response[data$study == 2 & data$group == 2]))
    )
  )
})

test_that("hb_summary() pool mock mcmc", {
  set.seed(0)
  data <- hb_sim_pool(
    n_continuous = 1,
    n_binary = 0,
    n_study = 2,
    n_group = 2,
    n_patient = 3
  )$data
  mcmc <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  mcmc <- head(mcmc, 6)
  for (col in colnames(mcmc)) {
    mcmc[[col]] <- mcmc[[col]] + seq_len(6) / 10
  }
  out <- hb_summary(mcmc, data, eoi = c(0, 1), direction = c(">", "<"))
  expect_equal(out$group, seq_len(2))
  data_current <- dplyr::filter(data, study == 2)
  x_alpha <- get_x_alpha_pool_or_mixture(data)
  x_delta <- get_x_delta(data)
  x_beta <- get_x_beta(data = data, x_alpha = x_alpha, x_delta = x_delta)
  response_samples <- lapply(
    seq_len(6),
    function(x) {
      out <- x_beta[-seq_len(3), 2, drop = TRUE] *
        mcmc[["beta[2]"]][x] + rep(c(0, mcmc$delta[x]), each = 3) +
        rep(c(mcmc$alpha[x], 0), each = 3)
      as.data.frame(cbind(group = data_current$group, value = out, sample = x))
    }
  )
  response <- dplyr::bind_rows(response_samples)
  response <- dplyr::group_by(response, group, sample)
  response <- dplyr::summarize(response, value = mean(value), .groups = "drop")
  response <- dplyr::arrange(response, group, sample)
  for (i in seq_len(2)) {
    j <- data$group == i
    k <- j & data$study == max(data$study)
    expect_equal(out$data_mean[i], mean(data$response[k]))
    expect_equal(out$data_sd[i], sd(data$response[k]))
    expect_equal(
      out$data_lower[i],
      mean(data$response[k]) -
        stats::qnorm(0.975) *
          sd(data$response[k]) /
          sqrt(length(data$response[k]))
    )
    expect_equal(
      out$data_upper[i],
      mean(data$response[k]) +
        stats::qnorm(0.975) *
          sd(data$response[k]) /
          sqrt(length(data$response[k]))
    )
    expect_equal(
      out$response_mean[i],
      mean(dplyr::filter(response, group == i)$value)
    )
    expect_equal(
      out$response_lower[i],
      quantile(dplyr::filter(response, group == i)$value, 0.025)
    )
    expect_equal(
      out$response_upper[i],
      quantile(dplyr::filter(response, group == i)$value, 0.975)
    )
    expect_equal(
      out$response_sd[i],
      sd(dplyr::filter(response, group == i)$value)
    )
    expect_equal(
      out$response_variance[i],
      var(dplyr::filter(response, group == i)$value)
    )
  }
  v <- response$value
  control <- v[rep(seq_len(6), times = 2)]
  samples_diff <- v - control
  samples_diff <- utils::tail(samples_diff, 6)
  expect_equal(
    out$diff_mean[2],
    mean(samples_diff)
  )
  expect_equal(
    out$diff_lower[2],
    quantile(samples_diff, 0.025)
  )
  expect_equal(
    out$diff_upper[2],
    quantile(samples_diff, 0.975)
  )
  expect_equal(out[["P(diff > 0)"]][2], mean(samples_diff > 0))
  expect_equal(out[["P(diff < 1)"]][2], mean(samples_diff < 1))
  samples_effect <- samples_diff / mcmc[["sigma[2]"]]
  expect_equal(
    out$effect_mean[2],
    mean(samples_effect)
  )
  expect_equal(
    out$effect_lower[2],
    quantile(samples_effect, 0.025)
  )
  expect_equal(
    out$effect_upper[2],
    quantile(samples_effect, 0.975)
  )
})

test_that("hb_summary() precision ratio", {
  set.seed(0)
  data <- hb_sim_hierarchical(
    n_continuous = 1,
    n_binary = 0,
    n_study = 2,
    n_group = 2,
    n_patient = 3
  )$data
  mcmc <- hb_mcmc_hierarchical(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  out <- hb_summary(mcmc, data, eoi = c(0, 1), direction = c(">", "<"))
  sigma <- mcmc[["sigma[2]"]]
  tau <- mcmc$tau
  n <- 6
  samples <- (1 / tau ^ 2) / ((1 / tau ^ 2) + (1 / (sigma ^ 2 / n)))
  expect_equal(
    out$precision_ratio[1],
    mean(samples)
  )
  expect_equal(
    out$precision_ratio_lower[1],
    quantile(samples, 0.025)
  )
  expect_equal(
    out$precision_ratio_upper[1],
    quantile(samples, 0.975)
  )
})
