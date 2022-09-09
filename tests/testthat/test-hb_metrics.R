test_that("hb_metrics()", {
  borrow <- tibble::tibble(
    group = 1,
    response_mean = 1.5,
    response_variance = 1.75
  )
  pool <- tibble::tibble(
    group = 1,
    response_mean = 1,
    response_variance = 1.3
  )
  independent <- tibble::tibble(
    group = 1,
    response_mean = 2,
    response_variance = 1.8
  )
  out <- hb_metrics(
    borrow = borrow,
    pool = pool,
    independent = independent
  )
  expect_equal(
    out,
    tibble::tibble(
      mean_shift_ratio = 0.5,
      variance_shift_ratio = 0.1
    )
  )
})
