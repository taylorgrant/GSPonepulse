#' Write OnePulse Summary Tables to Excel
#'
#' Creates an Excel workbook containing overall summaries and crosstabs for
#' selected OnePulse survey questions.
#'
#' Each question is written to its own worksheet. The worksheet contains an
#' overall summary followed by one table for each requested crosstab variable.
#' Crosstab tables include significance lettering and respondent base sizes.
#'
#' @param data A cleaned OnePulse survey data frame, typically created by
#'   [clean_onepulse()] and optionally [enrich_onepulse()].
#' @param questions A character vector of question identifiers to include,
#'   such as `c("q_1", "q_2", "q_3")`.
#' @param cross_vars A character vector of single-select variables to use as
#'   crosstabs, such as `c("Gender", "Age Decile")`.
#' @param file A character string giving the output path for the Excel workbook.
#'
#' @return Invisibly returns the path to the written Excel workbook.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_onepulse(
#'   data,
#'   questions = paste0("q_", 1:6),
#'   cross_vars = c("Gender", "Age Decile"),
#'   file = "OnePulse_tables.xlsx"
#' )
#' }
write_onepulse <- function(data, questions, cross_vars, file) {
  wb <- openxlsx::createWorkbook()

  # Styles ---------------------------------------------------

  title_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fontSize = 12
  )

  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#E7E6E6",
    border = "Bottom"
  )

  note_style <- openxlsx::createStyle(
    fontSize = 9,
    fontColour = "#666666",
    textDecoration = "italic"
  )

  pct_style <- openxlsx::createStyle(
    numFmt = "0%"
  )

  base_style <- openxlsx::createStyle(
    textDecoration = "italic",
    fontColour = "#666666",
    border = "Top"
  )

  # Write workbook -------------------------------------------

  for (q in questions) {
    openxlsx::addWorksheet(
      wb,
      sheetName = q
    )

    current_row <- 1

    # Overall summary ----------------------------------------

    openxlsx::writeData(
      wb,
      sheet = q,
      x = "Overall",
      startRow = current_row
    )

    openxlsx::addStyle(
      wb,
      sheet = q,
      style = title_style,
      rows = current_row,
      cols = 1
    )

    current_row <- current_row + 1

    overall <- summarise_onepulse(data, q)

    openxlsx::writeData(
      wb,
      sheet = q,
      x = overall,
      startRow = current_row,
      headerStyle = header_style
    )

    # Format overall frac column as percentage
    if ("frac" %in% names(overall)) {
      frac_col <- which(
        names(overall) == "frac"
      )

      openxlsx::addStyle(
        wb,
        sheet = q,
        style = pct_style,
        rows = (current_row + 1):(current_row + nrow(overall)),
        cols = frac_col,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    # Move below overall table
    current_row <-
      current_row +
      nrow(overall) +
      3

    # Crosstabs ----------------------------------------------

    for (cross in cross_vars) {
      result <- crosstab_onepulse(
        data,
        q,
        cross
      )

      tab <- result$table
      letters <- result$letters

      # Crosstab title ---------------------------------------

      openxlsx::writeData(
        wb,
        sheet = q,
        x = paste0("By ", cross),
        startRow = current_row
      )

      openxlsx::addStyle(
        wb,
        sheet = q,
        style = title_style,
        rows = current_row,
        cols = 1
      )

      current_row <- current_row + 1

      # Significance letter key ------------------------------

      key <- paste0(
        letters$letter,
        " = ",
        letters$group,
        collapse = "   |   "
      )

      openxlsx::writeData(
        wb,
        sheet = q,
        x = key,
        startRow = current_row
      )

      openxlsx::addStyle(
        wb,
        sheet = q,
        style = note_style,
        rows = current_row,
        cols = 1
      )

      current_row <- current_row + 1

      # Main crosstab ----------------------------------------

      openxlsx::writeData(
        wb,
        sheet = q,
        x = tab,
        startRow = current_row,
        headerStyle = header_style
      )

      # Base footer ------------------------------------------

      base_row <- current_row + nrow(tab) + 1

      base_footer <- tibble::tibble(
        answer = "Base (n)"
      )

      for (g in names(tab)[-1]) {
        base_value <- result$bases |>
          dplyr::filter(group == g) |>
          dplyr::pull(total)

        # Protect against unmatched labels
        if (length(base_value) == 0) {
          base_value <- NA_integer_
        }

        base_footer[[g]] <- base_value
      }

      openxlsx::writeData(
        wb,
        sheet = q,
        x = base_footer,
        startRow = base_row,
        colNames = FALSE
      )

      openxlsx::addStyle(
        wb,
        sheet = q,
        style = base_style,
        rows = base_row,
        cols = 1:ncol(tab),
        gridExpand = TRUE,
        stack = TRUE
      )

      # Move below crosstab + base row -----------------------

      current_row <-
        base_row + 3
    }

    # Column widths ------------------------------------------

    openxlsx::setColWidths(
      wb,
      sheet = q,
      cols = 1,
      widths = 35
    )

    openxlsx::setColWidths(
      wb,
      sheet = q,
      cols = 2:20,
      widths = 14
    )
  }

  # Save workbook --------------------------------------------

  openxlsx::saveWorkbook(
    wb,
    file,
    overwrite = TRUE
  )
}
