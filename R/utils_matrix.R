get_x_alpha <- function(data) {
  out <- matrix(NA_integer_, nrow = nrow(data), ncol = 0)
  for (study in sort(unique(data$study))) {
    x <- as.integer(data$study == study & data$group == min(data$group))
    out <- cbind(out, x)
  }
  colnames(out) <- NULL
  out
}

get_x_alpha_pool_or_mixture <- function(data) {
  matrix(
    as.integer(data$group == min(data$group)),
    nrow = nrow(data),
    ncol = 1
  )
}

get_x_delta <- function(data) {
  combos <- dplyr::distinct(data, study, group)
  combos <- dplyr::filter(combos, group != min(group))
  out <- matrix(NA_integer_, nrow = nrow(data), ncol = 0)
  for (index in seq_len(nrow(combos))) {
    study <- combos$study[index]
    group <- combos$group[index]
    x <- as.integer(data$study == study & data$group == group)
    out <- cbind(out, x)
  }
  colnames(out) <- NULL
  out
}

get_x_beta <- function(data, x_alpha, x_delta) {
  terms <- grep("^covariate", colnames(data), value = TRUE)
  if (!length(terms)) {
    return(matrix(0, nrow = nrow(data), ncol = 0))
  }
  formula <- as.formula(paste("~0 + ", paste(terms, collapse = " + ")))
  colnames(x_alpha) <- paste0("x_alpha", seq_len(ncol(x_alpha)))
  colnames(x_delta) <- paste0("x_delta", seq_len(ncol(x_delta)))
  x_beta <- model.matrix(formula, data = data)
  x_all <- cbind(x_alpha, x_delta, x_beta)
  out_list <- list()
  for (study in sort(unique(data$study))) {
    index_study <- data$study == study
    index_study_na <- if_any(
      "response" %in% colnames(data),
      index_study & !is.na(data$response),
      index_study
    )
    index_study <- which(index_study)
    index_study_na <- which(index_study_na)
    x_study <- x_all[index_study,, drop = FALSE] # nolint
    x_study_na <- x_all[index_study_na,, drop = FALSE] # nolint
    columns <- beta_only(columns_full_rank(x_study_na))
    out_study <- x_study[, columns, drop = FALSE]
    for (column in seq_len(ncol(out_study))) {
      out_study[, column] <- as.numeric(
        scale(
          out_study[, column],
          center = TRUE,
          scale = sd(out_study[, column]) > .Machine$double.eps
        )
      )
    }
    colnames(out_study) <- sprintf("study%s_%s", study, colnames(out_study))
    out_list[[study]] <- out_study
  }
  out <- as.matrix(Matrix::bdiag(out_list))
  colnames(out) <- unlist(lapply(out_list, colnames))
  hb_warn_identifiable(
    response = if_any(
      "response" %in% colnames(data),
      data$response,
      rep(0, nrow(data))
    ),
    x_alpha = x_alpha,
    x_delta = x_delta,
    x_beta = out
  )
  out
}

columns_full_rank <- function(x) {
  x <- drop_zero_columns(x)
  columns <- colnames(x)
  qr <- base::qr(x)
  rank <- qr$rank
  columns[qr$pivot[seq_len(rank)]]
}

beta_only <- function(x) {
  grep("^x_alpha|^x_delta", x, value = TRUE, invert = TRUE)
}

is_full_rank <- function(x) {
  qr(x)$rank == ncol(x)
}

drop_zero_columns <- function(x) {
  sums <- colSums(abs(x))
  x[, sums > epsilon, drop = FALSE]
}

epsilon <- sqrt(.Machine$double.eps)
