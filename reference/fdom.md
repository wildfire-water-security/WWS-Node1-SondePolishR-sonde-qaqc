# Correct fDOM data for temperature and turbidity

The fDOM signal can be affected but sediment and temperature, but these
effects can be corrected for.

## Usage

``` r
fdom_UI(id)

fdom_server(id, sondeproj, data_ver, y_var, view_state)
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

- view_state:

  A \`reactiveVal\` holding a list of items specifying the view state: -
  abs_dates: The absolute range of dates within the dataset - dates: The
  range of dates being viewed via the date selector - period_view:
  Logical if the period view is being used - period_length: Length of
  period view - period_n: The period number to view.
