test_that("hb_ml_pool()", {
  data <- hb_sim_pool(n_continuous = 2)$data
  mcmc <- hb_mcmc_pool(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  out <- suppressWarnings(hb_ml_pool(mcmc = mcmc, data = data))
  expect_s3_class(out, "bridge")
})
