test_that("hb_convergence()", {
  set.seed(0)
  data <- hb_sim_pool(n_continuous = 2)$data
  mcmc <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 200,
    n_iterations = 200
  )
  out <- hb_convergence(mcmc)
  expect_equal(dim(out), c(1, 3))
  expect_equal(
    sort(colnames(out)),
    sort(c("max_rhat", "min_ess_bulk", "min_ess_tail"))
  )
})
