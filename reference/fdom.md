# Correct fDOM data for temperature and turbidity

The fDOM signal can be affected but sediment and temperature, but these
effects can be corrected for.

## Usage

``` r
fdom_UI(id)

fdom_server(id, sondeproj, data_ver, y_var, period_view, dates, p_length)
```

## Arguments

- id:

  An ID string passed to shiny::NS(), used for namespacing UI
  inputs/outputs.

- sondeproj:

  A \`reactiveVal\` holding the current dataset.

- data_ver:

  A \`reactiveVal\` holding a number used to track when new data is
  added to trigger resets.

- y_var:

  Y-variable to plot on the y-axis.

- period_view:

  Should data be viewed by period?

- dates:

  The date range to view the data.

- p_length:

  The length of the period to view.
