hb_sim_grid <- function(n_study, n_group, n_patient) {
  out <- tidyr::expand_grid(
    study = seq_len(n_study),
    group = seq_len(n_group),
    patient = seq_len(n_patient)
  )
  out <- dplyr::filter(out, study == max(study) | group == min(group))
  out$patient <- seq_len(nrow(out))
  out
}

hb_sim_x_beta <- function(
  n_continuous,
  n_binary,
  data,
  x_alpha,
  x_delta
) {
  out <- list()
  for (study in sort(unique(data$study))) {
    out[[study]] <- hb_sim_x_beta_study(
      n_continuous = n_continuous,
      n_binary = n_binary,
      data = data[data$study == study,, drop = FALSE], # nolint
      x_alpha = x_alpha[data$study == study,, drop = FALSE], # nolint
      x_delta = x_delta[data$study == study,, drop = FALSE] # nolint
    )
  }
  out <- as.matrix(Matrix::bdiag(out))
  studies <- rep(sort(unique(data$study)), each = n_continuous + n_binary)
  covariates <- c(
    sprintf("continuous%s", seq_len(n_continuous)),
    sprintf("binary%s", seq_len(n_binary))
  )
  covariates <- rep(covariates, times = length(unique(studies)))
  colnames(out) <- sprintf(
    "covariate_study%s_%s",
    studies,
    covariates
  )
  out
}

hb_sim_x_beta_study <- function(
  n_continuous,
  n_binary,
  data,
  x_alpha,
  x_delta
) {
  out <- NULL
  try_x_beta <- 0
  x_alpha <- drop_zero_columns(x_alpha)
  x_delta <- drop_zero_columns(x_delta)
  while (is.null(out) || !is_full_rank(cbind(x_alpha, x_delta, out))) {
    try_x_beta <- try_x_beta + 1
    stopifnot(try_x_beta < 1000)
    out <- matrix(numeric(0), nrow = nrow(data), ncol = 0)
    for (index in seq_len(n_continuous)) {
      x <- stats::rnorm(n = nrow(data), mean = 0, sd = 1)
      out <- cbind(out, x)
    }
    for (index in seq_len(n_binary)) {
      x <- sample(c(0L, 1L), size = nrow(data), replace = TRUE)
      out <- cbind(out, x)
    }
  }
  out
}

hb_sim_response <- function(
  data,
  x_alpha,
  x_delta,
  x_beta,
  alpha,
  delta,
  beta,
  sigma
) {
  out <- x_alpha %*% alpha +
    x_delta %*% delta +
    x_beta %*% beta +
    stats::rnorm(n = nrow(data), mean = 0, sd = sigma[data$study])
  as.numeric(out)
}
