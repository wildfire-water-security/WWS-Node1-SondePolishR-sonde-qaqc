# Visualize dataset by analyte

Plots loaded dataset with options to explore full dataset or view the
data in weekly sections. Plots are created using `plotly` so they are
interactive. Module also allows the user to view the changes over the
dataset versions via row selection in a table via the `log`.

## Usage

``` r
explore_data_UI(id)

explore_data_server(
  id,
  sondeproj,
  data_ver,
  y_var,
  period_view,
  dates,
  p_length
)
```

## Arguments

- id:

  An ID string passed to shiny::NS(), used for namespacing UI
  inputs/outputs.

- sondeproj:

  A `reactiveVal` holding the current dataset.

- data_ver:

  A `reactiveVal` holding a number used to track when new data is added
  to trigger resets.

- y_var:

  Y-variable to plot on the y-axis.

- period_view:

  Should data be viewed by period?

- dates:

  The date range to view the data.

- p_length:

  The length of the period to view.

## Value

Invisible NULL
