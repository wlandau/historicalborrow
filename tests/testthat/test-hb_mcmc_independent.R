test_that("hb_mcmc_independent() without betas", {
  set.seed(0)
  data <- hb_sim_independent(
    n_study = 5,
    n_group = 3,
    n_patient = 100,
    n_continuous = 0,
    n_binary = 0,
    s_alpha = 1,
    s_delta = 1,
    s_beta = 1,
    s_sigma = 1
  )$data
  out <- hb_mcmc_independent(
    data,
    n_chains = 2,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  lapply(out, function(x) true(is.numeric(x) && all(is.finite(x))))
  exp <- c(
    ".chain",
    ".draw",
    ".iteration",
    sprintf("alpha[%s]", seq_len(5)),
    sprintf("delta[%s]", seq_len(2)),
    sprintf("sigma[%s]", seq_len(5))
  )
  expect_equal(sort(colnames(out)), sort(exp))
})

test_that("hb_mcmc_independent() with betas", {
  set.seed(0)
  data <- hb_sim_independent(
    n_study = 5,
    n_group = 3,
    n_patient = 100,
    n_continuous = 1,
    n_binary = 0,
    s_alpha = 1,
    s_delta = 1,
    s_beta = 1,
    s_sigma = 1
  )$data
  out <- hb_mcmc_independent(
    data,
    n_chains = 2,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  lapply(out, function(x) true(is.numeric(x) && all(is.finite(x))))
  exp <- c(
    ".chain",
    ".draw",
    ".iteration",
    sprintf("alpha[%s]", seq_len(5)),
    sprintf("delta[%s]", seq_len(2)),
    sprintf("beta[%s]", seq_len(5)),
    sprintf("sigma[%s]", seq_len(5))
  )
  expect_equal(sort(colnames(out)), sort(exp))
})
