test_that("hb_weight_bma()", {
  out <- hb_weight_bma(
    mll_pool = 0,
    mll_independent = 0,
    prior_weight = 0.5
  )
  expect_equal(out, 0.5)
  out <- hb_weight_bma(
    mll_pool = log(1.11),
    mll_independent = log(0.75),
    prior_weight = 0.6
  )
  expect_equal(out, 0.689441)
})
