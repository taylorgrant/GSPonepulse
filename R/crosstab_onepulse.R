#' Create a OnePulse Crosstab
#'
#' Creates a crosstab for a OnePulse survey question by a single-select
#' grouping variable. Both single-select and multi-select survey questions
#' are supported.
#'
#' For each response option, pairwise proportion tests are conducted across
#' crosstab groups. P-values are adjusted for multiple comparisons using the
#' Holm method. Significant differences are displayed using column letters,
#' where a letter indicates that the proportion is significantly higher than
#' the proportion in the referenced column.
#'
#' Known Likert scales are automatically detected and displayed in their
#' intended ordinal order.
#'
#' @param data A cleaned OnePulse survey data frame, typically created by
#'   [clean_onepulse()].
#' @param q A character string identifying the question to summarize, such
#'   as `"q_1"` or `"q_2"`.
#' @param cross A character string identifying the single-select variable
#'   to use as the crosstab, such as `"Gender"` or `"Age Decile"`.
#' @param alpha Numeric significance level used for pairwise proportion
#'   tests. Defaults to `0.05`.
#'
#' @return A named list containing:
#' \describe{
#'   \item{table}{A wide-format crosstab containing percentages and
#'   significance letters.}
#'   \item{sig}{Pairwise proportion test results, including raw and
#'   Holm-adjusted p-values.}
#'   \item{letters}{A lookup table mapping crosstab groups to significance
#'   letters.}
#'   \item{bases}{The respondent base size for each crosstab group.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- crosstab_onepulse(
#'   data,
#'   "q_2",
#'   "Gender"
#' )
#'
#' result$table
#' result$bases
#' }
crosstab_onepulse <- function(data, q, cross, alpha = 0.05, box = FALSE) {
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

  if (!cross %in% names(data)) {
    stop(
      "Crosstab variable '",
      cross,
      "' was not found in the data.",
      call. = FALSE
    )
  }
  # Build long counts --------------------------------------

  if (length(cols) == 1) {
    long <- data |>
      dplyr::filter(
        !is.na(.data[[q]]),
        !is.na(.data[[cross]])
      ) |>
      dplyr::transmute(
        !!rlang::sym(cross) := .data[[cross]],
        answer = .data[[q]]
      ) |>
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
      dplyr::count(
        !!rlang::sym(cross),
        answer
      ) |>
      dplyr::group_by(
        !!rlang::sym(cross)
      ) |>
      dplyr::mutate(
        total = sum(n),
        frac = n / total
      ) |>
      dplyr::ungroup()
  } else {
    base <- data |>
      dplyr::filter(
        !is.na(.data[[cross]])
      ) |>
      dplyr::count(
        !!rlang::sym(cross),
        name = "total"
      )

    long <- data |>
      dplyr::select(
        !!rlang::sym(cross),
        dplyr::all_of(cols)
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(cols),
        values_to = "answer"
      ) |>
      dplyr::filter(
        !is.na(answer)
      ) |>
      dplyr::mutate(
        answer = stringr::str_replace_all(answer, ";", ","),
        answer = match_likert(
          answer,
          likert_dictionary
        )
      ) |>
      dplyr::count(
        !!rlang::sym(cross),
        answer
      ) |>
      dplyr::left_join(
        base,
        by = cross
      ) |>
      dplyr::mutate(
        frac = n / total
      )
  }

  # Preserve Likert ordering -------------------------------

  all_answers <- if (is.factor(long$answer)) {
    levels(long$answer)
  } else {
    unique(long$answer)
  }

  all_groups <- unique(long[[cross]])

  # Group bases --------------------------------------------

  bases <- long |>
    dplyr::distinct(
      !!rlang::sym(cross),
      total
    )

  # Add zero-count combinations ----------------------------

  long <- tidyr::expand_grid(
    answer = all_answers,
    group = all_groups
  ) |>
    dplyr::rename(
      !!cross := group
    ) |>
    dplyr::left_join(
      long,
      by = c("answer", cross)
    ) |>
    dplyr::left_join(
      bases,
      by = cross,
      suffix = c("", "_base")
    ) |>
    dplyr::mutate(
      n = tidyr::replace_na(n, 0L),
      total = dplyr::coalesce(
        total,
        total_base
      ),
      frac = n / total
    ) |>
    dplyr::select(
      -dplyr::any_of("total_base")
    )

  # Column letters -----------------------------------------

  group_lookup <- tibble::tibble(
    group = as.character(all_groups),
    letter = LETTERS[
      seq_along(all_groups)
    ]
  )

  # Pairwise significance tests ----------------------------

  pairs <- utils::combn(
    as.character(all_groups),
    2,
    simplify = FALSE
  )

  sig <- purrr::map_dfr(
    all_answers,
    function(ans) {
      tmp <- long |>
        dplyr::filter(
          answer == ans
        )

      tests <- purrr::map_dfr(
        pairs,
        function(pair) {
          g1 <- pair[1]
          g2 <- pair[2]

          x1 <- tmp |>
            dplyr::filter(
              as.character(.data[[cross]]) == g1
            )

          x2 <- tmp |>
            dplyr::filter(
              as.character(.data[[cross]]) == g2
            )

          test <- stats::prop.test(
            x = c(
              x1$n,
              x2$n
            ),
            n = c(
              x1$total,
              x2$total
            )
          )

          tibble::tibble(
            answer = ans,
            group1 = g1,
            group2 = g2,
            frac1 = x1$frac,
            frac2 = x2$frac,
            p_value = test$p.value
          )
        }
      )

      tests |>
        dplyr::mutate(
          p_adj = stats::p.adjust(
            p_value,
            method = "holm"
          ),
          significant = p_adj < alpha
        )
    }
  )

  # Convert significance into letters ----------------------

  sig_letters <- sig |>
    dplyr::filter(
      significant
    ) |>
    dplyr::mutate(
      higher_group = dplyr::if_else(
        frac1 > frac2,
        group1,
        group2
      ),
      lower_group = dplyr::if_else(
        frac1 > frac2,
        group2,
        group1
      )
    ) |>
    dplyr::left_join(
      group_lookup,
      by = c(
        "lower_group" = "group"
      )
    ) |>
    dplyr::transmute(
      answer,
      group = higher_group,
      letter
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(
      answer,
      group
    ) |>
    dplyr::summarise(
      sig_letter = paste(
        sort(letter),
        collapse = ""
      ),
      .groups = "drop"
    )

  # Display table ------------------------------------------

  display <- long |>
    dplyr::mutate(
      group = as.character(
        .data[[cross]]
      )
    ) |>
    dplyr::left_join(
      sig_letters,
      by = c(
        "answer",
        "group"
      )
    ) |>
    dplyr::mutate(
      value = paste0(
        scales::percent(
          frac,
          accuracy = 1
        ),
        dplyr::if_else(
          is.na(sig_letter),
          "",
          paste0(
            " ",
            sig_letter
          )
        )
      )
    ) |>
    dplyr::select(
      answer,
      group,
      value
    ) |>
    tidyr::pivot_wider(
      names_from = group,
      values_from = value
    )

  # Clean bases for Excel footer ---------------------------

  bases <- bases |>
    dplyr::mutate(
      group = as.character(
        .data[[cross]]
      )
    ) |>
    dplyr::select(
      group,
      total
    )

  # Return --------------------------------------------------

  result <- list(
    table = display,
    sig = sig,
    letters = group_lookup,
    bases = bases
  )

  attr(
    result,
    "confidence_level"
  ) <- 1 - alpha

  result
}
