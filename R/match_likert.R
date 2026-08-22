#' Match Responses to a Known Likert Scale
#'
#' Compares a vector of survey responses against a dictionary of known
#' Likert-style response scales. When a matching scale is found, the responses
#' are returned as an ordered factor using the scale's intended response order.
#'
#' Response text and dictionary values are normalized before matching to reduce
#' mismatches caused by differences in capitalization or extra whitespace.
#'
#' If no unambiguous matching scale is found, the normalized responses are
#' returned as a character vector.
#'
#' @param x A character vector of survey responses.
#' @param likert_dictionary A named list of known Likert-style response scales.
#'
#' @return An ordered factor when a matching scale is identified; otherwise a
#'   character vector containing the normalized responses.
#'
#' @examples
#' \dontrun{
#' match_likert(
#'   c(
#'     "Agree",
#'     "Strongly agree",
#'     "Neutral"
#'   ),
#'   likert_dictionary
#' )
#' }
match_likert <- function(x, likert_dictionary) {
  # Normalize responses
  x_clean <- x |>
    stringr::str_squish() |>
    stringr::str_to_sentence()

  # Normalize dictionary
  dict_clean <- purrr::map(
    likert_dictionary,
    ~ .x |>
      stringr::str_squish() |>
      stringr::str_to_sentence()
  )

  vals <- unique(stats::na.omit(x_clean))

  matches <- names(
    purrr::keep(
      dict_clean,
      ~ all(vals %in% .x)
    )
  )

  if (length(matches) == 1) {
    factor(
      x_clean,
      levels = dict_clean[[matches]],
      ordered = TRUE
    )
  } else {
    x_clean
  }
}
