test_that("sim hierarchical data", {
  set.seed(0)
  out <- hb_sim_hierarchical(
    n_study = 2,
    n_group = 3,
    n_patient = 5,
    n_continuous = 2,
    n_binary = 2
  )$data
  expect_equal(dim(out), c(20, 12))
  expect_equal(out$study, rep(c(1, 2, 2, 2), each = 5))
  expect_equal(out$group, rep(c(1, 1, 2, 3), each = 5))
  expect_equal(out$patient, seq_len(20))
  expect_true(is.numeric(out$response))
  expect_false(anyNA(out$response))
  cols <- c(
    "covariate_study1_continuous1",
    "covariate_study1_continuous2",
    "covariate_study1_binary1",
    "covariate_study1_binary2"
  )
  for (col in cols) {
    expect_true(any(abs(out[[col]][seq_len(5)]) > 0))
    expect_equal(out[[col]][seq(6, 20)], rep(0, 15))
  }
  cols <- c(
    "covariate_study2_continuous1",
    "covariate_study2_continuous2",
    "covariate_study2_binary1",
    "covariate_study2_binary2"
  )
  for (col in cols) {
    expect_true(any(abs(out[[col]][seq(6, 20)]) > 0))
    expect_equal(out[[col]][seq(1, 5)], rep(0, 5))
  }
  cols <- c(
    "covariate_study1_binary1",
    "covariate_study1_binary2",
    "covariate_study2_binary1",
    "covariate_study2_binary2"
  )
  for (col in cols) {
    expect_equal(sort(unique(out[[col]])), sort(unique(c(0, 1))))
  }
  cols <- c(
    "covariate_study1_continuous1",
    "covariate_study1_continuous2",
    "covariate_study2_continuous1",
    "covariate_study2_continuous2"
  )
  for (col in cols) {
    expect_false(identical(sort(unique(out[[col]])), sort(unique(c(0, 1)))))
  }
})

test_that("sim hierarchical parameters", {
  set.seed(0)
  out <- hb_sim_hierarchical(
    n_study = 2,
    n_group = 3,
    n_patient = 5,
    n_continuous = 2,
    n_binary = 2
  )$parameters
  lapply(out, function(x) expect_true(is.numeric(x)))
  expect_equal(length(out$delta), 2)
  expect_equal(length(out$beta), 8)
  expect_equal(length(out$sigma), 2)
  expect_equal(length(out$alpha), 2)
})

test_that("sim hierarchical matrices", {
  set.seed(0)
  out <- hb_sim_hierarchical(
    n_study = 2,
    n_group = 3,
    n_patient = 5,
    n_continuous = 2,
    n_binary = 2
  )$matrices
  x_alpha <- cbind(
    c(rep(1, 5), rep(0, 15)),
    c(rep(0, 5), rep(1, 5), rep(0, 10))
  )
  dimnames(out$x_alpha) <- NULL
  expect_equal(out$x_alpha, x_alpha)
  x_delta <- replicate(2, matrix(1, nrow = 5), simplify = FALSE)
  x_delta <- as.matrix(Matrix::bdiag(x_delta))
  x_delta <- rbind(matrix(0, nrow = 10, ncol = 2), x_delta)
  dimnames(out$x_delta) <- NULL
  expect_equal(out$x_delta, x_delta)
  out <- tibble::as_tibble(out$x_beta)
  cols <- c(
    "study1_covariate_study1_continuous1",
    "study1_covariate_study1_continuous2",
    "study1_covariate_study1_binary1",
    "study1_covariate_study1_binary2",
    "study2_covariate_study2_continuous1",
    "study2_covariate_study2_continuous2",
    "study2_covariate_study2_binary1",
    "study2_covariate_study2_binary2"
  )
  for (col in cols[seq_len(4)]) {
    expect_true(any(abs(out[[col]][seq_len(5)]) > 0))
    expect_equal(out[[col]][seq(6, 20)], rep(0, 15))
  }
  for (col in cols[seq(5, 8)]) {
    expect_true(any(abs(out[[col]][seq(6, 20)]) > 0))
    expect_equal(out[[col]][seq_len(5)], rep(0, 5))
  }
  cols <- c(
    "study1_covariate_study1_binary1",
    "study1_covariate_study1_binary2",
    "study2_covariate_study2_binary1",
    "study2_covariate_study2_binary2"
  )
  for (col in cols) {
    expect_equal(length(unique(out[[col]])), 3)
  }
  cols <- c(
    "study1_covariate_study1_continuous1",
    "study1_covariate_study1_continuous2",
    "study2_covariate_study2_continuous1",
    "study2_covariate_study2_continuous2"
  )
  for (col in cols) {
    expect_gt(length(unique(out[[col]])), 3)
  }
})
