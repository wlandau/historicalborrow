test_that("hb_mcmc_mixture() without betas", {
  set.seed(0)
  data <- hb_sim_mixture(
    n_study = 3,
    n_group = 3,
    n_patient = 100,
    n_continuous = 0,
    n_binary = 0,
    s_delta = 1,
    s_beta = 1,
    s_sigma = 1
  )$data
  out <- hb_mcmc_mixture(
    data,
    n_chains = 2,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50,
    m_omega = c(0, 0, 0),
    s_omega = c(1, 2, 3),
    p_omega = c(5 / 12, 1 / 3, 1 / 4)
  )
  lapply(out, function(x) true(is.numeric(x) && all(is.finite(x))))
  exp <- c(
    ".chain",
    ".draw",
    ".iteration",
    "alpha",
    sprintf("delta[%s]", seq_len(2)),
    "sigma",
    sprintf("omega[%s]", seq_len(3)),
    sprintf("post_p[%s]", seq_len(3))
  )
  expect_equal(sort(colnames(out)), sort(exp))
})

test_that("hb_mcmc_mixture() with betas", {
  set.seed(0)
  data <- hb_sim_mixture(
    n_study = 5,
    n_group = 3,
    n_patient = 100,
    n_continuous = 2,
    n_binary = 0,
    s_delta = 1,
    s_beta = 1,
    s_sigma = 1
  )$data
  out <- hb_mcmc_mixture(
    data,
    n_chains = 2,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50,
    m_omega = c(0, 0, 0),
    s_omega = c(1, 2, 3),
    p_omega = c(5 / 12, 1 / 3, 1 / 4)
  )
  lapply(out, function(x) true(is.numeric(x) && all(is.finite(x))))
  exp <- c(
    ".chain",
    ".draw",
    ".iteration",
    "alpha",
    sprintf("delta[%s]", seq_len(2)),
    sprintf("beta[%s]", seq_len(2)),
    "sigma",
    sprintf("omega[%s]", seq_len(3)),
    sprintf("post_p[%s]", seq_len(3))
  )
  expect_equal(sort(colnames(out)), sort(exp))
})
