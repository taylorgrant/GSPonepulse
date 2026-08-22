test_that("summarise_onepulse handles single select questions", {
  data <- tibble::tibble(
    q_1 = c(
      "Agree",
      "Agree",
      "Neutral",
      "Strongly agree"
    )
  )

  result <- summarise_onepulse(
    data,
    "q_1"
  )

  expect_equal(
    sum(result$n),
    4
  )

  expect_equal(
    sum(result$frac),
    1
  )
})


test_that("summarise_onepulse orders known Likert scales", {
  data <- tibble::tibble(
    q_1 = c(
      "Agree",
      "Strongly disagree",
      "Neutral",
      "Strongly agree",
      "Disagree"
    )
  )

  result <- summarise_onepulse(
    data,
    "q_1"
  )

  expect_equal(
    as.character(result$answer),
    c(
      "Strongly disagree",
      "Disagree",
      "Neutral",
      "Agree",
      "Strongly agree"
    )
  )
})

test_that("summarise_onepulse handles multiselect questions", {
  data <- tibble::tibble(
    q_2_1 = c("Safe", NA, "Safe", NA),
    q_2_2 = c("Exciting", "Exciting", NA, NA)
  )

  result <- summarise_onepulse(
    data,
    "q_2"
  )

  expect_equal(
    result$frac[result$answer == "Safe"],
    0.50
  )

  expect_equal(
    result$frac[result$answer == "Exciting"],
    0.50
  )

  expect_true(
    all(result$total == 4)
  )
})

test_that("summarise_onepulse errors when question is missing", {
  data <- tibble::tibble(
    q_1 = c("Yes", "No")
  )

  expect_error(
    summarise_onepulse(data, "q_99"),
    "not found"
  )
})
