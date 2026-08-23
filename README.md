
# GSPonepulse

`GSPonepulse` provides a standardized workflow for importing, cleaning,
enriching, analyzing, and exporting survey data from OnePulse.

The package handles the repetitive parts of OnePulse analysis while
producing consistent question summaries, crosstabs, significance
testing, Excel workbooks, and presentation-ready `gt` tables.

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("taylorgrant/GSPonepulse")
```

Then load the package:

``` r
library(GSPonepulse)
```

## Standard workflow

A typical OnePulse analysis follows four steps:

``` r
library(GSPonepulse)

# 1. Read the OnePulse export
raw <- read_onepulse("data/survey.csv")

# 2. Add standardized demographic variables
enriched <- enrich_onepulse(raw)

# 3. Clean question names and create the question table of contents
clean <- clean_onepulse(enriched)

data <- clean$survey
toc <- clean$svy_q

# 4. Create Excel summary tables (do not include open ends!)
write_onepulse(
  data,
  questions = paste0("q_", 1:6),
  cross_vars = c(
    "gender",
    "age_decile"
  ),
  file = "output/OnePulse_tables.xlsx"
)
```

Demographic enrichment should occur before cleaning because
`clean_onepulse()` standardizes the column names in the final analysis
data.

## Reading OnePulse data

`read_onepulse()` imports a OnePulse CSV export while retaining survey
metadata needed by downstream functions.

``` r
raw <- read_onepulse("survey.csv")
```

If there are multiple surveys using the exact same question wording,
they can be combined in a single call:

``` r
raw <- read_onepulse(
  c(
    "survey_part1.csv",
    "survey_part2.csv"
  )
)
```

When multiple files are supplied, their column structures are checked
before the respondent records are combined. The survey year is stored as
metadata for use by `enrich_onepulse()`.

## Enriching demographics

`enrich_onepulse()` creates standardized analysis variables from the
demographic fields available in a OnePulse export.

``` r
enriched <- enrich_onepulse(raw)
```

Depending on the fields available in the survey, derived variables can
include:

- age cohort and age decile;
- generation;
- education group;
- household income group;
- partisanship; and
- standardized geographic and demographic factors.

Generation is calculated using the survey year captured by
`read_onepulse()`, rather than the current calendar year.

## Cleaning survey data

`clean_onepulse()` standardizes column names and converts OnePulse
question names into concise identifiers.

For example:

``` text
Q(1) Before today; how familiar were you...
```

becomes:

``` text
q_1
```

Multi-select columns such as:

``` text
Q(2_1) Innovative[Question: ...]
Q(2_2) Convenient[Question: ...]
Q(2_3) Exciting[Question: ...]
```

become:

``` text
q_2_1
q_2_2
q_2_3
```

The function returns the cleaned survey and a de-duplicated question
table of contents:

``` r
clean <- clean_onepulse(enriched)

data <- clean$survey
toc <- clean$svy_q
```

The table of contents is also retained as survey metadata so that
`write_onepulse()` can place the full question wording at the top of
each Excel worksheet.

## Summarizing questions

`summarise_onepulse()` creates an overall summary for a single-select or
multi-select question.

``` r
summarise_onepulse(
  data,
  "q_1"
)
```

For a multi-select question, pass the question stem rather than an
individual response column:

``` r
summarise_onepulse(
  data,
  "q_2"
)
```

The function automatically identifies all columns beginning with `q_2_`.

For single-select questions, percentages sum to 100%. For multi-select
questions, each percentage represents the proportion of respondents
selecting that option, so percentages may sum to more than 100%.

### Likert scales and box scores

Known Likert-style response scales are automatically recognized and
displayed in their intended ordinal order.

``` text
Strongly disagree
Disagree
Neutral
Agree
Strongly agree
```

Set `box = TRUE` to collapse a recognized five-point Likert scale into
`Top 2 Box`, `Middle`, and `Bottom 2 Box`:

``` r
summarise_onepulse(
  data,
  "q_3",
  box = TRUE
)
```

Only recognized five-point, single-select Likert questions are
collapsed. Unrecognized scales and multi-select questions retain their
original response format.

## Creating crosstabs

`crosstab_onepulse()` creates a crosstab using a single-select grouping
variable. Both single-select and multi-select survey questions are
supported.

``` r
result <- crosstab_onepulse(
  data,
  "q_2",
  "gender"
)

result$table
```

The returned object contains:

``` r
result$table # formatted crosstab
result$sig # pairwise significance tests
result$letters # column-letter lookup
result$bases # respondent bases by group
```

Box scores can also be applied before the crosstab and significance
tests are calculated:

``` r
result <- crosstab_onepulse(
  data,
  "q_3",
  "generation",
  box = TRUE
)
```

### Significance testing

Pairwise proportion tests are conducted across crosstab groups within
each response option. P-values are adjusted using the Holm method, with
a default significance level of `alpha = 0.05`.

Significant differences are shown using column letters:

``` text
             Male (A)    Female (B)
Safe             35%          48% A
```

`48% A` indicates that the value in column B is significantly higher
than the value in column A.

The significance level can be changed when needed:

``` r
crosstab_onepulse(
  data,
  "q_2",
  "gender",
  alpha = 0.10
)
```

## Writing Excel tables

`write_onepulse()` creates a formatted Excel workbook containing overall
summaries and crosstabs for the requested questions.

``` r
write_onepulse(
  data,
  questions = paste0("q_", 1:6),
  cross_vars = c(
    "gender",
    "age_decile",
    "generation"
  ),
  file = "OnePulse_tables.xlsx"
)
```

Each question receives its own worksheet containing:

1.  the full question wording;
2.  an overall response summary;
3.  one table for each requested crosstab;
4.  column-letter keys for significance testing; and
5.  respondent bases for each crosstab group.

Open-ended questions should generally be omitted from `questions`. For
example, if `q_4` is open-ended:

``` r
questions <- paste0("q_", c(1:3, 5:6))
```

Set `box = TRUE` to box every recognized Likert question included in the
workbook:

``` r
write_onepulse(
  data,
  questions = paste0("q_", 1:6),
  cross_vars = c("gender", "age_decile"),
  file = "OnePulse_tables_boxed.xlsx",
  box = TRUE
)
```

Alternatively, provide question identifiers to box only selected
questions:

``` r
write_onepulse(
  data,
  questions = paste0("q_", 1:6),
  cross_vars = c("gender", "age_decile"),
  file = "OnePulse_tables.xlsx",
  box = c("q_3", "q_5", "q_6")
)
```

## Creating presentation-ready tables

`gt_onepulse()` converts an existing summary or crosstab result into a
formatted `gt` table.

``` r
summary_table <- summarise_onepulse(
  data,
  "q_3",
  box = TRUE
) |>
  gt_onepulse(
    title = "Q3. How likely are you to consider this product?",
    subtitle = "Overall"
  )
```

For crosstabs, significance letters and respondent bases carry through
to the formatted table:

``` r
crosstab_table <- crosstab_onepulse(
  data,
  "q_3",
  "generation",
  box = TRUE
) |>
  gt_onepulse(
    title = "Q3. How likely are you to consider this product?",
    subtitle = "By generation"
  )
```

The resulting table can be saved using `gt::gtsave()`:

``` r
gt::gtsave(
  crosstab_table,
  "q3_by_generation.png",
  zoom = 2
)
```

The `gt` package is required only for this optional table-formatting
workflow.

## Batch creation of `gt` tables

The following code can be used to summarise, convert, and save each (non
open-ended) question to a `gt` table in one consistent pipeline.

Create the output directory beforehand:

``` r
dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)
```

``` r
paste0("q_", 1:3) |> # input the questions to work through
  purrr::walk(\(question) {
    toc <- attr(data, "question_toc")

    title <- toc$question[
      match(question, toc$q)
    ]

    summarise_onepulse(
      data,
      question,
      box = TRUE
    ) |>
      gt_onepulse(
        title = title,
        subtitle = "Overall"
      ) |>
      gt::gtsave(
        filename = paste0(
          question,
          "_overall.png"
        ),
        path = "output/tables",
        zoom = 2
      )
  })
```

For crosstabs with significance lettering:

``` r
paste0("q_", 1:5) |>
  purrr::walk(\(question) {
    toc <- attr(data, "question_toc")

    title <- toc$question[
      match(question, toc$q)
    ]

    crosstab_onepulse(
      data,
      question,
      cross = "generation",
      box = TRUE
    ) |>
      gt_onepulse(
        title = title,
        subtitle = "By generation"
      ) |>
      gt::gtsave(
        filename = paste0(
          question,
          "_by_generation.png"
        ),
        path = "output/tables",
        zoom = 2
      )
  })
```

## Main functions

| Function | Purpose |
|----|----|
| `read_onepulse()` | Import one or more OnePulse CSV exports |
| `enrich_onepulse()` | Create standardized demographic groupings |
| `clean_onepulse()` | Standardize column names and create a question TOC |
| `summarise_onepulse()` | Summarize single- or multi-select questions |
| `crosstab_onepulse()` | Create crosstabs with significance testing |
| `write_onepulse()` | Export formatted survey tables to Excel |
| `gt_onepulse()` | Convert analysis output into a formatted `gt` table |
