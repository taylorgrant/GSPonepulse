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
#' intended ordinal order. When `box = TRUE`, recognized five-point Likert
#' scales in single-select questions are collapsed into `Top 2 Box`, `Middle`,
#' and `Bottom 2 Box`. Unrecognized scales and multi-select questions are
#' returned in their original format.
#'
#' @param data A cleaned OnePulse survey data frame, typically created by
#'   [clean_onepulse()].
#' @param q A character string identifying the question to summarize, such
#'   as `"q_1"` or `"q_2"`.
#' @param box Logical. If `TRUE`, recognized five-point Likert scales in
#'   single-select questions are collapsed into `Top 2 Box`, `Middle`, and
#'   `Bottom 2 Box`. Defaults to `FALSE`.
#'
#' @return A tibble containing:
#' \describe{
#'   \item{answer}{The response option or collapsed box category.}
#'   \item{n}{The number of respondents selecting the response option or
#'   falling within the box category.}
#'   \item{frac}{The proportion of respondents selecting the response option
#'   or falling within the box category.}
#'   \item{total}{The respondent base used to calculate the proportion.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summarise_onepulse(data, "q_1")
#' summarise_onepulse(data, "q_2")
#' summarise_onepulse(data, "q_2", box = TRUE)
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

  # Preserve the original factor order for single-column questions
  factor_levels <- NULL

  if (length(cols) == 1 && is.factor(data[[cols]])) {
    factor_levels <- levels(data[[cols]]) |>
      stringr::str_replace_all(";", ",") |>
      match_likert(likert_dictionary) |>
      as.character()
  }

  result <- if (length(cols) == 1) {
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
        answer = if (box) {
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

  if (!is.null(factor_levels) && !box) {
    result |>
      dplyr::mutate(
        answer = factor(
          answer,
          levels = factor_levels,
          ordered = TRUE
        )
      ) |>
      dplyr::arrange(answer)
  } else {
    result |>
      dplyr::arrange(
        dplyr::desc(frac),
        answer
      )
  }
}
