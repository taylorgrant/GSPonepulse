#' Summarise a OnePulse Survey Question
#'
#' Creates an overall summary table for a OnePulse survey question.
#' Both single-select and multi-select questions are supported.
#'
#' For single-select questions, percentages represent the share of respondents
#' selecting each response option. For multi-select questions, percentages
#' represent the share of respondents selecting each option and may therefore
#' sum to more than 100 percent.
#'
#' Known Likert scales are automatically detected and displayed in their
#' intended ordinal order.
#'
#' @param data A cleaned OnePulse survey data frame, typically created by
#'   [clean_onepulse()].
#' @param q A character string identifying the question to summarize, such
#'   as `"q_1"` or `"q_2"`.
#'
#' @return A tibble containing:
#' \describe{
#'   \item{answer}{The response option.}
#'   \item{n}{The number of respondents selecting the response option.}
#'   \item{frac}{The proportion of respondents selecting the response option.}
#'   \item{total}{The respondent base used to calculate the proportion.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summarise_onepulse(data, "q_1")
#' summarise_onepulse(data, "q_2")
#' }
summarise_onepulse <- function(data, q, box = FALSE) {
  cols <- names(data)[
    names(data) == q |
      startsWith(names(data), paste0(q, "_"))
  ]

  if (length(cols) == 0) {
    stop(
      "Question '",
      q,
      "' was not found in the data.",
      call. = FALSE
    )
  }

  if (length(cols) == 1) {
    data |>
      dplyr::transmute(
        answer = .data[[cols]]
      ) |>
      dplyr::filter(!is.na(answer)) |>
      dplyr::mutate(
        answer = stringr::str_replace_all(answer, ";", ","),
        answer = match_likert(
          answer,
          likert_dictionary
        ),
        answer = if (box && length(cols) == 1) {
          collapse_likert(answer)
        } else {
          answer
        }
      ) |>
      dplyr::count(answer) |>
      dplyr::mutate(
        frac = n / sum(n),
        total = sum(n)
      )
  } else {
    data |>
      dplyr::select(dplyr::all_of(cols)) |>
      tidyr::pivot_longer(
        dplyr::everything(),
        values_to = "answer"
      ) |>
      dplyr::filter(!is.na(answer)) |>
      dplyr::mutate(
        answer = stringr::str_replace_all(answer, ";", ","),
        answer = match_likert(
          answer,
          likert_dictionary
        )
      ) |>
      dplyr::count(answer) |>
      dplyr::mutate(
        frac = n / nrow(data),
        total = nrow(data)
      )
  }
}
