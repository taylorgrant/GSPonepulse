test_that("as_gt_onepulse creates a gt table", {
  testthat::skip_if_not_installed("gt")

  result <- tibble::tibble(
    answer = c("Yes", "No"),
    n = c(60, 40),
    frac = c(0.60, 0.40),
    total = c(100, 100)
  )

  table <- gt_onepulse(result)

  expect_s3_class(table, "gt_tbl")
})
