test_that("crosstab stores its confidence level", {
  data <- tibble::tibble(
    q_1 = c(
      rep("Yes", 40),
      rep("No", 10),
      rep("Yes", 10),
      rep("No", 40)
    ),
    group = rep(
      c("A", "B"),
      each = 50
    )
  )

  result <- crosstab_onepulse(
    data,
    "q_1",
    "group",
    alpha = 0.05
  )

  expect_equal(
    attr(result, "confidence_level"),
    0.95
  )

  expect_true(
    all(c("table", "sig", "letters", "bases") %in% names(result))
  )
})
