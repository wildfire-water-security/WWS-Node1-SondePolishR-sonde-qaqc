# View data by a specified time length

Used to adjust plotting to show periods to better examine details.

## Usage

``` r
weekly_range_sidebar_UI(id)

weekly_range_buttons_UI(id)

weekly_range_server(id, sondeproj, period_view, dates, p_length, data_ver)
```

## Arguments

- id:

  the shiny ID of the module

- sondeproj:

  A \`reactiveVal\` holding the current dataset.

- period_view:

  Should data be viewed by period?

- dates:

  The date range to view the data.

- p_length:

  The length of the period to view.

- data_ver:

  A \`reactiveVal\` holding a number used to track when new data is
  added to trigger resets.

## Value

a reactive of length two with the min and max dates.
