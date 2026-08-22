#' Clean OnePulse Survey Data
#'
#' Cleans and standardizes the column names in a OnePulse survey export
#' and creates a table of contents containing the survey question numbers
#' and question wording.
#'
#' OnePulse question columns are renamed to standardized question identifiers
#' such as `q_1` for single-select questions and `q_2_1`, `q_2_2`, and so on
#' for multi-select questions.
#'
#' @param data A OnePulse survey data frame, typically created by
#'   [read_onepulse()].
#'
#' @return A named list containing:
#' \describe{
#'   \item{data}{The cleaned OnePulse survey data with standardized question
#'   column names.}
#'   \item{toc}{A table of contents containing question numbers and question
#'   wording.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- read_onepulse("survey.csv")
#' clean <- clean_onepulse(raw)
#'
#' data <- clean$data
#' toc <- clean$toc
#' }
clean_onepulse <- function(data) {
  survey <- data |>
    janitor::clean_names() |>
    dplyr::select(-dplyr::matches("comments|sentiment")) |>
    dplyr::rename_with(
      .fn = ~ ifelse(
        stringr::str_detect(.x, "^q_\\d"), # only touch q_* columns
        stringr::str_extract(.x, "^q_\\d+(?:_\\d+)?"), # keep q_7  or q_7_1
        .x
      ),
      .cols = dplyr::starts_with("q_")
    )

  question_toc <- function(data) {
    tibble::tibble(raw = names(data)) |>
      dplyr::filter(stringr::str_detect(raw, "^Q\\(")) |>

      # Remove comment columns
      dplyr::filter(
        !stringr::str_detect(raw, "^Q\\(\\d+\\) Comments"),
        !stringr::str_detect(raw, "^Q\\(\\d+\\) Sentiment")
      ) |>

      dplyr::mutate(
        # Pull main question number:
        # Q(2_1) -> 2
        # Q(1)   -> 1
        q_num = stringr::str_match(
          raw,
          "^Q\\((\\d+)"
        )[, 2],

        # Multiselects have wording inside [Question: ...]
        # Single selects have wording immediately after Q(#)
        question = dplyr::if_else(
          stringr::str_detect(raw, "\\[Question:"),

          stringr::str_match(
            raw,
            "\\[Question:\\s*(.*?)\\]$"
          )[, 2],

          stringr::str_remove(
            raw,
            "^Q\\(\\d+\\)\\s*"
          )
        )
      ) |>

      dplyr::distinct(q_num, question) |>

      dplyr::mutate(
        q_num = as.integer(q_num),
        question = stringr::str_replace_all(question, ";", ",")
      ) |>

      dplyr::arrange(q_num) |>

      dplyr::transmute(
        q = paste0('q_', q_num),
        question = paste0(
          "Q",
          q_num,
          ". ",
          question
        )
      )
  }

  svy_q <- question_toc(data)

  attr(survey, "question_toc") <- svy_q

  list(survey = survey, svy_q = svy_q)
}
