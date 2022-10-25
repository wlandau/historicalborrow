test_that("hb_plot_weighted()", {
  set.seed(1)
  data <- hb_sim_independent(n_continuous = 2)$data
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
  summary <- hb_summary_weighted(
    mcmc_pool = mcmc_pool,
    mcmc_independent = mcmc_independent,
    data = data,
    weights = c(0, 0.25, 0.5, 0.75, 1)
  )
  out1 <- hb_plot_weighted(
    summary = summary,
    outcome = "response"
  )
  out2 <- hb_plot_weighted(
    summary = summary,
    outcome = "diff",
    highlight_weights = c(0.25, 0.75)
  )
  expect_s3_class(out1, "ggplot")
  expect_s3_class(out2, "ggplot")
})
