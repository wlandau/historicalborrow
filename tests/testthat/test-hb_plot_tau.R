test_that("hb_plot_tau()", {
  data <- hb_sim_independent(n_continuous = 2)$data
  mcmc <- hb_mcmc_hierarchical(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 100,
    n_iterations = 200
  )
  out <- hb_plot_tau(mcmc = mcmc)
  expect_s3_class(out, "ggplot")
})
