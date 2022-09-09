test_that("hb_s_tau()", {
  expect_equal(hb_s_tau(precision_ratio = 0.5, sigma = 1, n = 1), 2)
  expect_equal(hb_s_tau(precision_ratio = 0.5, sigma = 1, n = 100), 0.2)
  expect_equal(
    hb_s_tau(precision_ratio = 0.67, sigma = 2.89, n = 100),
    2 * 2.89 * sqrt((1 / 100) * ((1 / 0.67) - 1))
  )
})
