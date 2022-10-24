hb_ml <- function(mcmc, data_list, quiet) {
  lower_bounds <- list()
  upper_bounds <- list()
  for (name in colnames(mcmc)) {
    if (any(grepl(pattern = "^sigma", x = name))) {
      lower_bounds[[name]] <- 0
      upper_bounds[[name]] <- data_list$s_sigma
    } else {
      lower_bounds[[name]] <- -Inf
      upper_bounds[[name]] <- Inf
    }
  }
  out <- bridgesampling::bridge_sampler(
    samples = as.matrix(mcmc),
    log_posterior = hb_lp_benchmark,
    data = data_list,
    lb = lower_bounds,
    ub = upper_bounds,
    silent = quiet,
    verbose = !quiet
  )
  structure(exp(out$logml), bridge = out)
}

hb_lp_benchmark <- function(sample, data) {
  sample <- tibble::as_tibble(as.list(sample))
  alpha <- as.numeric(dplyr::select(sample, tidyselect::starts_with("alpha")))
  delta <- as.numeric(dplyr::select(sample, tidyselect::starts_with("delta")))
  beta <- as.numeric(dplyr::select(sample, tidyselect::starts_with("beta")))
  sigma <- as.numeric(dplyr::select(sample, tidyselect::starts_with("sigma")))
  x_alpha <- data$x_alpha %*% alpha
  x_delta <- data$x_delta %*% delta
  x_beta <- if_any(
    length(beta) > 0L,
    data$x_beta %*% beta,
    matrix(0, nrow = nrow(x_alpha), ncol = ncol(x_alpha))
  )
  mean <- as.numeric(x_alpha + x_delta + x_beta)
  sd <- sigma[data$study]
  lp_alpha <- sum(dnorm(x = alpha, mean = 0, sd = data$s_alpha, log = TRUE))
  lp_delta <- sum(dnorm(x = delta, mean = 0, sd = data$s_delta, log = TRUE))
  lp_beta <- if_any(
    length(beta) > 0L,
    sum(dnorm(x = beta, mean = 0, sd = data$s_beta, log = TRUE)),
    0
  )
  lp_y <- sum(dnorm(x = data$y, mean = mean, sd = sd, log = TRUE))
  lp_alpha + lp_delta + lp_beta + lp_y
}
