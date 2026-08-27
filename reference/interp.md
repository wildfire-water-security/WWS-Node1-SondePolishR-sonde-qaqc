# Fill in data gaps using interpolation

Attempts to fill gaps less than the max length using the specified
interpolation method.

## Usage

``` r
interp_UI(id)

interp_server(id, sondeproj, data_ver, y_var, view_state, current_mod)
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

- current_mod:

  The name of the current module being viewed.
