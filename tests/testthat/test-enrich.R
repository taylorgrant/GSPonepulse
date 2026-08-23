test_that("enrich_onepulse returns data without optional demographics", {
  data <- tibble::tibble(
    `Age range` = c(20, 35, 65)
  )

  attr(data, "survey_year") <- 2026

  result <- enrich_onepulse(data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("Cohort" %in% names(result))
})
