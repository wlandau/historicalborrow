test_that("hb_ml_independent()", {
  data <- hb_sim_independent(n_continuous = 2)$data
  mcmc <- hb_mcmc_independent(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  out <- suppressWarnings(hb_ml_independent(mcmc = mcmc, data = data))
  expect_s3_class(out, "bridge")
})
