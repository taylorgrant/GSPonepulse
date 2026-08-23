test_that("boxing leaves non-Likert questions unchanged", {
  data <- tibble::tibble(
    q_1 = c("Apple", "Apple", "Banana")
  )

  result <- summarise_onepulse(
    data,
    "q_1",
    box = TRUE
  )

  expect_equal(
    result$answer,
    c("Apple", "Banana")
  )
})

test_that("non-Likert answers are sorted by descending percentage", {
  data <- tibble::tibble(
    q_1 = c("Alpha", "Zulu", "Zulu", "Zulu")
  )

  result <- summarise_onepulse(
    data,
    "q_1"
  )

  expect_equal(
    result$answer,
    c("Zulu", "Alpha")
  )
})
