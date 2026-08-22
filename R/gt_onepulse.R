#' Convert a OnePulse Result to a GT Table
#'
#' @param x The result of [summarise_onepulse()] or
#'   [crosstab_onepulse()].
#' @param title Optional table title.
#' @param subtitle Optional table subtitle.
#'
#' @return An object of class `gt_tbl`.
#'
#' @export
gt_onepulse <- function(
  x,
  title = NULL,
  subtitle = NULL
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop(
      "Package 'gt' is required. Install it with install.packages('gt').",
      call. = FALSE
    )
  }

  # Overall summary -----------------------------------------

  if (
    inherits(x, "data.frame") &&
      all(c("answer", "n", "frac", "total") %in% names(x))
  ) {
    base <- unique(x$total)

    tab <- x |>
      dplyr::select(
        answer,
        n,
        frac
      ) |>
      gt::gt() |>
      gt::cols_label(
        answer = "Answer",
        n = "n",
        frac = "Percent"
      ) |>
      gt::fmt_percent(
        columns = "frac",
        decimals = 0
      ) |>
      gt::cols_align(
        align = "left",
        columns = "answer"
      ) |>
      gt::cols_align(
        align = "center",
        columns = c("n", "frac")
      ) |>
      gt::tab_source_note(
        source_note = paste0(
          "Base: n = ",
          paste(base, collapse = ", ")
        )
      )
  } else if (
    is.list(x) &&
      all(c("table", "letters", "bases") %in% names(x))
  ) {
    # Crosstab -------------------------------------------------

    base_note <- paste0(
      x$bases$group,
      " (n = ",
      x$bases$total,
      ")",
      collapse = "  |  "
    )

    column_labels <- c(
      list(
        answer = "Answer"
      ),
      stats::setNames(
        as.list(
          paste0(
            x$letters$group,
            "<br>(",
            x$letters$letter,
            ")"
          )
        ),
        x$letters$group
      )
    )

    confidence_level <- attr(
      x,
      "confidence_level"
    )

    confidence_label <- scales::percent(
      confidence_level,
      accuracy = 1
    )

    tab <- x$table |>
      gt::gt() |>
      gt::cols_label(
        .list = column_labels,
        .fn = gt::md
      ) |>
      gt::cols_align(
        align = "left",
        columns = "answer"
      ) |>
      gt::tab_source_note(
        source_note = paste0(
          "Base: ",
          base_note
        )
      ) |>
      gt::tab_source_note(
        source_note = gt::md(
          paste0(
            "_",
            "Letters indicate a significantly higher proportion ",
            "than the referenced column at a ",
            confidence_label,
            " confidence level.",
            "_"
          )
        )
      )
  } else {
    stop(
      "`x` must be the result of summarise_onepulse() ",
      "or crosstab_onepulse().",
      call. = FALSE
    )
  }

  # Optional heading ----------------------------------------

  if (!is.null(title)) {
    tab <- tab |>
      gt::tab_header(
        title = title,
        subtitle = subtitle
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          align = 'left',
          weight = 'bold',
          size = gt::px(16)
        ),
        locations = gt::cells_title(c("title"))
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          align = 'left',
          size = gt::px(14)
        ),
        locations = gt::cells_title(c("subtitle"))
      )
  }

  # Styling the table ----------------------------------------

  tab <- tab |>
    gt::tab_style(
      style = gt::cell_text(weight = 'bold', size = gt::px(13)),
      locations = list(
        gt::cells_column_labels(
          columns = gt::everything()
        )
      )
    ) |>
    # align column headers
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = list(
        gt::cells_column_labels(
          columns = !gt::all_of("answer")
        )
      )
    ) |>
    # font size in table
    gt::tab_style(
      style = gt::cell_text(size = gt::px(12)),
      locations = gt::cells_body(
        columns = gt::everything()
      )
    ) |>
    # font alignment
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(
        columns = !gt::all_of("answer")
      )
    ) |>
    # font choice
    gt::opt_table_font(
      font = c(
        "Avenir Next",
        "Helvetica Neue",
        "Arial",
        "sans-serif"
      )
    )

  tab |>
    gt::opt_row_striping() |>
    gt::tab_options(
      table.font.size = gt::px(11),
      data_row.padding = gt::px(5)
    )
}
