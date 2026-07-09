# View data by a specified time length

Used to adjust plotting to show periods to better examine details.

## Usage

``` r
weekly_range_sidebar_UI(id)

weekly_range_buttons_UI(id)

weekly_range_server(id, min_date, max_date)
```

## Arguments

- id:

  the shiny ID of the module

- min_date:

  Minimum date in the \`sondeproj\`.

- max_date:

  Maximum date in the \`sondeproj\`.

## Value

a reactive of length two with the min and max dates.
