test_that("get_x_alpha()", {
  data <- tibble::tibble(study = c(1, 1, 2, 2), group = c(1, 2, 1, 2))
  x <- get_x_alpha(data)
  dimnames(x) <- NULL
  exp <- matrix(c(1, 0, 0, 0, 0, 0, 1, 0), ncol = 2)
  expect_equal(x, exp)
})

test_that("get_x_delta()", {
  data <- tibble::tibble(
    study = c(1, 1, 1, 2, 2, 3, 3),
    group = c(1, 1, 2, 1, 2, 1, 3)
  )
  x <- get_x_delta(data)
  dimnames(x) <- NULL
  exp <- cbind(
    c(0, 0, 1, rep(0, 4)),
    c(rep(0, 4), 1, 0, 0),
    c(rep(0, 6), 1)
  )
  expect_equal(x, exp)
})

test_that("get_x_beta() null case", {
  data <- tibble::tibble(
    study = c(1, 1, 1, 2, 2, 3, 3),
    group = c(1, 1, 2, 1, 2, 1, 3)
  )
  x <- get_x_alpha(data)
  x_delta <- get_x_delta(data)
  x <- get_x_beta(data, x_alpha, x_beta)
  expect_equal(x, matrix(0, nrow = nrow(data), ncol = 0))
})

test_that("get_x_beta() non-null but repeating", {
  data <- tibble::tibble(
    study = c(1, 1, 1, 1, 2, 2, 2, 2),
    group = c(1, 1, 2, 2, 1, 1, 1, 1),
    covariate_a = seq_len(8),
    covariate_b = c(rep(0, 7), 1),
    nope = seq_len(8)
  )
  x_alpha <- get_x_alpha(data)
  x_delta <- get_x_delta(data)
  x <- get_x_beta(data, x_alpha, x_delta)
  expect_equal(dim(x), c(8, 3))
  expect_equal(
    sort(colnames(x)),
    sort(c("study1_covariate_a", "study2_covariate_a", "study2_covariate_b"))
  )
  expect_equal(
    x[, 1, drop = TRUE],
    c(as.numeric(scale(seq_len(4))), rep(0, 4))
  )
  expect_equal(
    x[, 2, drop = TRUE],
    c(rep(0, 4), as.numeric(scale(seq_len(4))))
  )
  expect_equal(
    x[, 3, drop = TRUE],
    c(rep(0, 4), as.numeric(scale(c(0, 0, 0, 1))))
  )
})

test_that("get_x_beta() non-null non-repeating", {
  data <- tibble::tibble(
    study = c(1, 1, 1, 1, 2, 2, 2, 2),
    group = c(1, 1, 2, 2, 1, 1, 1, 1),
    covariate_a = c(seq_len(4), c(1, 2, 1, 2)),
    covariate_b = c(0, 0, 1, 0, 1, 0, 0, 0)
  )
  x_alpha <- get_x_alpha(data)
  x_delta <- get_x_delta(data)
  x <- get_x_beta(data, x_alpha, x_delta)
  expect_equal(dim(x), c(8, 4))
  expect_equal(
    sort(colnames(x)),
    sort(
      c(
        "study1_covariate_a",
        "study1_covariate_b",
        "study2_covariate_a",
        "study2_covariate_b"
      )
    )
  )
  expect_equal(
    x[, 1, drop = TRUE],
    c(as.numeric(scale(seq_len(4))), rep(0, 4))
  )
  expect_equal(
    x[, 2, drop = TRUE],
    c(as.numeric(scale(c(0, 0, 1, 0))), rep(0, 4))
  )
  expect_equal(
    x[, 3, drop = TRUE],
    c(rep(0, 4), as.numeric(scale(c(1, 2, 1, 2))))
  )
  expect_equal(
    x[, 4, drop = TRUE],
    c(rep(0, 4), as.numeric(scale(c(1, 0, 0, 0))))
  )
})

test_that("get_x_beta() with all of a factor level missing", {
  data <- tibble::tibble(
    response = c(NA_real_, rep(0, 7)),
    study = c(1, 1, 1, 1, 2, 2, 2, 2),
    group = c(1, 1, 2, 2, 1, 1, 1, 1),
    covariate_level = c(1, 0, 0, 0, 1, 0, 0, 0)
  )
  x_alpha <- get_x_alpha(data)
  x_delta <- get_x_delta(data)
  # Without accounting for the NA in the response,
  # x would have 2 columns instead of 1.
  x <- get_x_beta(data, x_alpha, x_delta)
  expect_equal(dim(x), c(8, 1))
  expect_equal(
    sort(colnames(x)),
    sort("study2_covariate_level")
  )
  expect_equal(
    x[, 1, drop = TRUE],
    c(0, 0, 0, 0, 1.5, -0.5, -0.5, -0.5)
  )
})
