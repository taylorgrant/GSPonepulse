#' Read OnePulse Survey Data
#'
#' Reads one or more OnePulse CSV exports into a single tibble. When
#' multiple files are supplied, the files are checked for consistent
#' column structures before being combined. Survey metadata is read
#' from the export header and the survey year is stored as an attribute
#' for use by downstream OnePulse functions.
#'
#' @param file_loc A character vector containing the path or paths to
#'   OnePulse CSV export files.
#'
#' @return A tibble containing the combined OnePulse survey data, with
#'   survey metadata stored as attributes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_onepulse("survey.csv")
#'
#' data <- read_onepulse(c(
#'   "survey_part1.csv",
#'   "survey_part2.csv"
#' ))
#' }
read_onepulse <- function(file_loc) {
  # Read metadata from first file
  survey_years <- file_loc |>
    purrr::map_chr(
      ~ readr::read_lines(.x, n_max = 1)
    ) |>
    stringr::str_extract(
      "\\b(19|20)\\d{2}\\b"
    ) |>
    as.integer()

  if (dplyr::n_distinct(survey_years) > 1) {
    stop(
      "OnePulse files are from different survey years: ",
      paste(unique(survey_years), collapse = ", "),
      call. = FALSE
    )
  }

  survey_year <- unique(survey_years)

  if (length(file_loc) == 1) {
    tmp <- readr::read_csv(
      file_loc,
      skip = 3,
      show_col_types = FALSE
    )
  } else {
    dfs <- file_loc |>
      purrr::map(
        ~ readr::read_csv(
          .x,
          skip = 3,
          show_col_types = FALSE
        )
      )

    # Check that all files have identical columns
    same_cols <- purrr::map_lgl(
      dfs[-1],
      ~ identical(names(dfs[[1]]), names(.x))
    )

    if (!all(same_cols)) {
      stop(
        "OnePulse files do not have identical column names."
      )
    }

    tmp <- dplyr::bind_rows(dfs)
  }

  # Store survey year as metadata
  attr(tmp, "survey_year") <- survey_year

  tmp
}
