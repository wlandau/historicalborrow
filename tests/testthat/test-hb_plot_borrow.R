test_that("hb_plot_borrow()", {
  set.seed(0)
  data <- hb_sim_independent(n_continuous = 2)$data
  mcmc_borrow <- hb_mcmc_hierarchical(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 200,
    n_iterations = 200
  )
  mcmc_pool <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 200,
    n_iterations = 200
  )
  mcmc_independent <- hb_mcmc_independent(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 200,
    n_iterations = 200
  )
  borrow <- hb_summary(mcmc_borrow, data)
  pool <- hb_summary(mcmc_pool, data)
  independent <- hb_summary(mcmc_independent, data)
  out <- hb_plot_borrow(
    borrow = borrow,
    pool = pool,
    independent = independent
  )
  expect_s3_class(out, "ggplot")
})
