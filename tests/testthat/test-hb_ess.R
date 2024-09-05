test_that("hb_ess()", {
  set.seed(0L)
  data <- hb_sim_independent(n_study = 3L)$data
  data$response[1L] <- NA_real_
  data$group <- sprintf("group%s", data$group)
  data$study <- sprintf("study%s", data$study)
  pool <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  hierarchical <- hb_mcmc_hierarchical(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  out <- hb_ess(
    mcmc_pool = pool,
    mcmc_hierarchical = hierarchical,
    data = data
  )
  v0 <- mean(
    (
      (pool$`sigma[1]` ^ (-2)) +
        (pool$`sigma[2]` ^ (-2)) +
        (pool$`sigma[3]` ^ (-2))
    ) ^ (-1)
  )
  expect_equal(out$v0, v0)
  exp <- mean(hierarchical$tau^2 + var(hierarchical$mu))
  expect_equal(object = out$v_tau, expected = exp, tolerance = 0.1)
  expect_equal(out$n, 199L)
  expect_equal(out$weight, out$v0 / out$v_tau)
  expect_equal(out$ess, out$n * out$weight)
})
