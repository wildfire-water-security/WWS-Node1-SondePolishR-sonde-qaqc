# Flag data identified as outliers either manually or view methods

Looks for "weird" data where there are large spikes within a short
period that are likely unrealistic and caused by instrument malfunction
or a bubble near the sensor.

## Usage

``` r
outlier_UI(id)

outlier_server(id, sondeproj, data_ver, y_var, view_state)
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
