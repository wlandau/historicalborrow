test_that("as_index_min() on characters", {
  x <- c("d", "d", "z", "a", "b", "b")
  out <- as_index_min(x, min = "b")
  expect_true(is.integer(out))
  expect_equal(out, c(3L, 3L, 4L, 2L, 1L, 1L))
})

test_that("as_index_min() on numerics", {
  x <- c(3.7, 3.7, 9999, 1.2, 2.2, 2.2)
  out <- as_index_min(x, min = 2.2)
  expect_true(is.integer(out))
  expect_equal(out, c(3L, 3L, 4L, 2L, 1L, 1L))
})

test_that("as_index_max() on characters", {
  x <- c("d", "d", "z", "a", "b", "b")
  out <- as_index_max(x, max = "b")
  expect_true(is.integer(out))
  expect_equal(out, c(2L, 2L, 3L, 1L, 4L, 4L))
})

test_that("as_index_max() on numerics", {
  x <- c(3.7, 3.7, 9999, 1.2, 2.2, 2.2)
  out <- as_index_max(x, max = 2.2)
  expect_true(is.integer(out))
  expect_equal(out, c(2L, 2L, 3L, 1L, 4L, 4L))
})

test_that("hb_data()", {
  data <- tibble::tibble(
    trial = rep(c("study_current", "study_historical"), each = 4),
    arm = rep(c("zzz", "treatment", rep("zzz", 2)), each = 2),
    subject = paste0("patient_", seq_len(8)),
    outcome = c(rnorm(n = 7), NA_real_),
    block1 = seq_len(8),
    block2 = seq_len(8),
    block3 = seq_len(8)
  )
  out <- hb_data(
    data = data,
    response = "outcome",
    study = "trial",
    study_reference = "study_current",
    group = "arm",
    group_reference = "zzz",
    patient = "subject",
    covariates = c("block1", "block3")
  )
  expect_equal(dim(out), c(8, 9))
  expect_equal(
    sort(colnames(out)),
    sort(
      c(
        "response",
        "study",
        "study_label",
        "group",
        "group_label",
        "patient",
        "patient_label",
        "covariate_block1",
        "covariate_block3"
      )
    )
  )
  exp <- data[match(data$subject, out$patient_label), ]
  expect_equal(exp$outcome, out$response)
  expect_equal(out$covariate_block1, exp$block1)
  expect_equal(out$covariate_block3, exp$block3)
  expect_equal(exp$arm, c(rep("zzz", 6), rep("treatment", 2)))
  expect_equal(out$group, c(rep(1, 6), rep(2, 2)))
  expect_equal(out$patient_label, exp$subject)
  expect_equal(out$study, rep(c(1, 2), each = 4))
  expect_equal(exp$trial, rep(c("study_historical", "study_current"), each = 4))
})

test_that("hb_data() on different reference levels", {
  data <- tibble::tibble(
    trial = rep(c("study_current", "study_historical"), each = 4),
    arm = rep(c("zzz", "treatment", rep("zzz", 2)), each = 2),
    subject = paste0("patient_", seq_len(8)),
    outcome = c(rnorm(n = 7), NA_real_),
    block1 = seq_len(8),
    block2 = seq_len(8),
    block3 = seq_len(8)
  )
  out <- hb_data(
    data = data,
    response = "outcome",
    study = "trial",
    study_reference = "study_historical",
    group = "arm",
    group_reference = "treatment",
    patient = "subject",
    covariates = "block2"
  )
  expect_equal(dim(out), c(8, 8))
  expect_equal(
    sort(colnames(out)),
    sort(
      c(
        "response",
        "study",
        "study_label",
        "group",
        "group_label",
        "patient",
        "patient_label",
        "covariate_block2"
      )
    )
  )
  exp <- data[match(data$subject, out$patient_label), ]
  expect_equal(exp$outcome, out$response)
  expect_equal(out$covariate_block2, exp$block2)
  expect_equal(exp$arm, c(rep("treatment", 2), rep("zzz", 6)))
  expect_equal(out$group, c(rep(1, 2), rep(2, 6)))
  expect_equal(out$patient_label, exp$subject)
  expect_equal(out$study, rep(c(1, 2), each = 4))
  expect_equal(exp$trial, rep(c("study_current", "study_historical"), each = 4))
})
