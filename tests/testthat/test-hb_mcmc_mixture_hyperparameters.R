test_that("hb_mcmc_mixture_hyperparameters()", {
  set.seed(0)
  data <- hb_sim_independent(
    n_study = 5,
    n_continuous = 2
  )$data
  data$study <- sprintf("s%s", data$study)
  out <- hb_mcmc_mixture_hyperparameters(
    data = data,
    response = "response",
    study = "study",
    study_reference = "s4",
    group = "group",
    group_reference = 1,
    patient = "patient",
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  expect_equal(dim(out), c(5, 4))
  expect_equal(
    sort(colnames(out)),
    sort(c("study", "study_index", "m_omega", "s_omega"))
  )
  expect_equal(out$study_index, seq_len(5))
  expect_equal(out$study, c("s1", "s2", "s3", "s5", "s4"))
  expect_equal(abs(out$m_omega) == 0, c(rep(FALSE, 4), TRUE))
  expect_equal(abs(out$s_omega) == 30, c(rep(FALSE, 4), TRUE))
})
