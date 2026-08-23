test_that("recognized Likert questions can be boxed", {
  data <- tibble::tibble(
    q_1 = c(
      "Strongly disagree",
      "Disagree",
      "Neutral",
      "Agree",
      "Strongly agree"
    )
  )

  result <- summarise_onepulse(
    data,
    "q_1",
    box = TRUE
  )

  expect_equal(
    as.character(result$answer),
    c("Top 2 Box", "Middle", "Bottom 2 Box")
  )

  expect_equal(
    result$n,
    c(2, 1, 2)
  )
})
