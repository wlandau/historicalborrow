test_that("hb_mll_independent()", {
  set.seed(0)
  data <- hb_sim_independent(n_study = 2)$data
  mcmc <- hb_mcmc_independent(
    data,
    n_chains = 1,
    n_adapt = 100,
    n_warmup = 50,
    n_iterations = 50
  )
  mcmc$.chain <- NULL
  mcmc$.iteration <- NULL
  mcmc$.draw <- NULL
  for (col in colnames(mcmc)) {
    mcmc[[col]] <- rnorm(n = nrow(mcmc))
  }
  mcmc[["sigma[1]"]] <- runif(n = nrow(mcmc), min = 0, max = 1)
  mcmc[["sigma[2]"]] <- runif(n = nrow(mcmc), min = 0, max = 1)
  out <- suppressWarnings(hb_mll_independent(mcmc = mcmc, data = data))
  expect_s3_class(out, "bridge")
})
