test_that("hb_bma_weight()", {
  out <- hb_bma_weight(
    ml_pool = 1,
    ml_independent = 1,
    prior_weight = 0.5
  )
  expect_equal(out, 0.5)
  out <- hb_bma_weight(
    ml_pool = 1.11,
    ml_independent = 0.75,
    prior_weight = 0.6
  )
  expect_equal(out, 0.689441)
})
