jags_mcmc <- function(
  file,
  variables,
  data_list,
  n_chains,
  n_adapt,
  n_warmup,
  n_iterations,
  quiet,
  system_file = TRUE
) {
  inits <- replicate(
    n_chains,
    list(
      ".RNG.name" = "base::Mersenne-Twister",
      ".RNG.seed" = sample.int(n = 1e6, size = 1)
    ),
    simplify = FALSE
  )
  if (system_file) {
    file <- file.path("jags", file)
    file <- system.file(file, package = "historicalborrow", mustWork = TRUE)
  }
  wrap <- if_any(quiet, capture.output, identity)
  wrap({
    model <- rjags::jags.model(
      file = file,
      data = data_list,
      n.chains = n_chains,
      n.adapt = n_adapt,
      inits = inits
    )
    stats::update(model, n.iter = n_warmup, quiet = quiet)
    coda <- rjags::coda.samples(
      model = model,
      variable.names = variables,
      n.iter = n_iterations
    )
  })
  tibble::as_tibble(posterior::as_draws_df(coda))
}
