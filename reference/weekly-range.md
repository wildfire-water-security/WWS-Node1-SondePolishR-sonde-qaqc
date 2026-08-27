# View data by a specified time length

Used to adjust plotting to show periods to better examine details.

## Usage

``` r
weekly_range_sidebar_UI(id)

weekly_range_buttons_UI(id)

weekly_range_server(id, sondeproj, data_ver, view_state)
```

## Arguments

- id:

  the shiny ID of the module

- sondeproj:

  A \`reactiveVal\` holding the current dataset.

- data_ver:

  A \`reactiveVal\` holding a number used to track when new data is
  added to trigger resets.

- view_state:

  A \`reactiveVal\` holding a list of items specifying the view state: -
  dates: The range of dates being viewed via the date selector -
  period_view: Logical if the period view is being used - period_length:
  Length of period view - period_n: The period number to view.

## Value

a reactive of length two with the min and max dates.
