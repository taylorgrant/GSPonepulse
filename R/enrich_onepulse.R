#' Enrich OnePulse Demographic Data
#'
#' Creates standardized demographic variables from the demographic fields
#' included in OnePulse survey exports. Derived variables include age groups,
#' generation, household income groups, political affiliation, and other
#' demographic classifications used in OnePulse analysis.
#'
#' Generation is derived using the survey year stored by
#' [read_onepulse()] and the respondent's reported age.
#'
#' @param data A OnePulse survey data frame, typically created by
#'   [read_onepulse()].
#'
#' @return The input data frame with additional analysis-ready demographic
#'   variables appended.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_onepulse("survey.csv")
#' data <- enrich_onepulse(data)
#' }
enrich_onepulse <- function(data) {
  survey_year <- attr(
    data,
    "survey_year"
  )

  if (is.null(survey_year)) {
    stop(
      "Survey year not found. Data must be imported with read_onepulse().",
      call. = FALSE
    )
  }

  if ("Age range" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        `Age range` = as.numeric(`Age range`),
        Cohort = dplyr::case_when(
          `Age range` <= 24 ~ "18-24",
          `Age range` > 24 & `Age range` <= 34 ~ "25-34",
          `Age range` > 34 & `Age range` <= 44 ~ "35-44",
          `Age range` > 44 & `Age range` <= 54 ~ "45-54",
          `Age range` > 54 & `Age range` <= 64 ~ "55-64",
          `Age range` > 64 ~ "65+"
        ),
        Cohort = factor(
          Cohort,
          levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
        ),
        yob = survey_year - `Age range`,
        Generation = dplyr::case_when(
          yob < 2013 & yob > 1996 ~ "Gen Z",
          yob < 1997 & yob > 1980 ~ "Millennials",
          yob < 1981 & yob > 1964 ~ "Gen X",
          yob < 1965 & yob > 1945 ~ "Boomers"
        ),
        Generation = factor(
          Generation,
          levels = c("Gen Z", "Millennials", "Gen X", "Boomers")
        ),
        `Age Decile` = dplyr::case_when(
          `Age range` <= 19 ~ "<20",
          `Age range` >= 20 & `Age range` <= 29 ~ "20-29",
          `Age range` >= 30 & `Age range` <= 39 ~ "30-39",
          `Age range` >= 40 & `Age range` <= 49 ~ "40-49",
          `Age range` >= 50 & `Age range` <= 59 ~ "50-59",
          `Age range` >= 60 ~ "60+",
        ),
        `Age Decile` = factor(
          `Age Decile`,
          levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60+")
        )
      ) |>
      dplyr::select(-yob) |>
      dplyr::relocate(Cohort, .after = "Age range") |>
      dplyr::relocate(Generation, .after = "Cohort") |>
      dplyr::relocate(`Age Decile`, .after = "Generation")
  }

  if ("Parent" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        Parent = factor(Parent)
      )
  }

  if ("Political Views" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        partisanship = dplyr::case_when(
          stringr::str_detect(
            `Political Views`,
            "Conservative"
          ) ~ "Conservative",
          stringr::str_detect(`Political Views`, "Liberal") ~ "Liberal",
          TRUE ~ "Centrist"
        ),
        partisanship = factor(
          partisanship,
          levels = c("Conservative", "Centrist", "Liberal")
        )
      ) |>
      dplyr::relocate(partisanship, .after = `Political Views`)
  }

  if ("Home location" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        `Home location` = factor(
          `Home location`,
          levels = c("West", "Midwest", "Northeast", "South")
        )
      )
  }

  if ("Combined household income" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        hhi = dplyr::case_when(
          `Combined household income` %in%
            c("$0 - $24;999", "$25;000 - $49;999") ~
            "< $50k",
          `Combined household income` %in% c("$50;000 - $74;999") ~
            "$50k - $75k",
          `Combined household income` %in% c("$75;000 - $99;999") ~
            "$75k - $100k",
          `Combined household income` %in%
            c("$100;000 - $124;999", "$125;000 - $149;999") ~
            "$100k - $150k",
          `Combined household income` %in%
            c("$150;000 - $174;999", "$175;000 - $199;999") ~
            "$150k - $200k",
          TRUE ~ "$200k+"
        ),
        hhi = factor(
          hhi,
          levels = c(
            "< $50k",
            "$50k - $75k",
            "$75k - $100k",
            "$100k - $150k",
            "$150k - $200k",
            "$200k+"
          )
        )
      ) |>
      dplyr::relocate(hhi, .after = `Combined household income`)
  }

  if ("Ethnicity" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        Ethnicity = factor(
          Ethnicity,
          levels = c(
            "White",
            "Hispanic/Latino",
            "Black",
            "Asian",
            "American Indian/Alaskan Native",
            "Other"
          )
        )
      )
  }

  if ("Home geography" %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        `Home geography` = factor(
          `Home geography`,
          levels = c("Urban", "Suburban", "Rural")
        )
      )
  }
}
