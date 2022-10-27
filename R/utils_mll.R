hb_mll <- function(mcmc, data_list, args) {
  lower_bounds <- list()
  upper_bounds <- list()
  mcmc_alpha <- dplyr::select(mcmc, tidyselect::starts_with("alpha"))
  mcmc_not_alpha <- dplyr::select(mcmc, -tidyselect::starts_with("alpha"))
  data_list$post_means <- tibble::as_tibble(as.list(colMeans(mcmc_not_alpha)))
  for (name in colnames(mcmc_alpha)) {
    lower_bounds[[name]] <- -Inf
    upper_bounds[[name]] <- Inf
  }
  args$samples <- as.matrix(mcmc_alpha)
  args$log_posterior <- hb_lp_benchmark
  args$data <- data_list
  args$lb <- lower_bounds
  args$ub <- upper_bounds
  args$silent <- args$silent %|||% TRUE
  args$verbose <- args$verbose %|||% FALSE
  do.call(what = bridgesampling::bridge_sampler, args = args)
}

hb_lp_benchmark <- function(sample, data) {
  sample <- tibble::as_tibble(as.list(sample))
  sample <- bind_cols(sample, data$post_means)
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
  missing <- is.na(data$y)
  lp_y <- sum(
    stats::dnorm(
      x = data$y[!missing],
      mean = mean[!missing],
      sd = sd[!missing],
      log = TRUE
    )
  )
  lp_alpha + lp_delta + lp_beta + lp_y
}
