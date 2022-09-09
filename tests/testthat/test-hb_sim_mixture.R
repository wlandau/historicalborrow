test_that("sim mixture data", {
  set.seed(0)
  out <- hb_sim_mixture(
    n_study = 2,
    n_group = 3,
    n_patient = 5,
    n_continuous = 2,
    n_binary = 2
  )$data
  expect_equal(dim(out), c(15, 8))
  expect_equal(out$study, rep(1, 15))
  expect_equal(out$group, rep(c(1, 2, 3), each = 5))
  expect_equal(out$patient, seq_len(15))
  expect_true(is.numeric(out$response))
  expect_false(anyNA(out$response))
  cols <- c(
    "covariate_study1_continuous1",
    "covariate_study1_continuous2",
    "covariate_study1_binary1",
    "covariate_study1_binary2"
  )
  for (col in cols) {
    expect_true(any(out[[col]][seq_len(15)] > 0))
  }
  cols <- c(
    "covariate_study1_binary1",
    "covariate_study1_binary2"
  )
  for (col in cols) {
    expect_equal(sort(unique(out[[col]])), sort(unique(c(0, 1))))
  }
  cols <- c(
    "covariate_study1_continuous1",
    "covariate_study1_continuous2"
  )
  for (col in cols) {
    expect_false(identical(sort(unique(out[[col]])), sort(unique(c(0, 1)))))
  }
})

test_that("sim mixture parameters", {
  set.seed(0)
  out <- hb_sim_mixture(
    n_study = 2,
    n_group = 3,
    n_patient = 5,
    n_continuous = 2,
    n_binary = 2
  )$parameters
  lapply(out, function(x) expect_true(is.numeric(x)))
  expect_equal(length(out$delta), 2)
  expect_equal(length(out$beta), 4)
  expect_equal(length(out$sigma), 1)
  expect_equal(length(out$alpha), 1)
  expect_equal(length(out$pi), 1)
  expect_equal(length(out$omega), 2)
})

test_that("sim mixture matrices", {
  set.seed(0)
  out <- hb_sim_mixture(
    n_study = 2,
    n_group = 3,
    n_patient = 5,
    n_continuous = 2,
    n_binary = 2
  )$matrices
  expect_equal(out$x_alpha, matrix(rep(c(1L, 0L, 0L), each = 5), nrow = 15))
  x_delta <- replicate(3, matrix(1, nrow = 5), simplify = FALSE)
  x_delta <- as.matrix(Matrix::bdiag(x_delta))[, c(2, 3)]
  expect_equal(out$x_delta, x_delta)
  out <- tibble::as_tibble(out$x_beta)
  cols <- c(
    "study1_covariate_study1_continuous1",
    "study1_covariate_study1_continuous2",
    "study1_covariate_study1_binary1",
    "study1_covariate_study1_binary2"
  )
  for (col in cols) {
    expect_true(any(out[[col]][seq_len(15)] > 0))
  }
  cols <- c(
    "study1_covariate_study1_binary1",
    "study1_covariate_study1_binary2"
  )
  for (col in cols) {
    expect_equal(length(unique(out[[col]])), 2)
  }
  cols <- c(
    "study1_covariate_study1_continuous1",
    "study1_covariate_study1_continuous2"
  )
  for (col in cols) {
    expect_gt(length(unique(out[[col]])), 2)
  }
})
