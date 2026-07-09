# Visualize dataset by analyte

Plots loaded dataset with options to explore full dataset or view the
data in weekly sections. Plots are created using `plotly` so they are
interactive. Module also allows the user to view the changes over the
dataset versions via row selection in a table via the `log`.

## Usage

``` r
explore_data_UI(id)

explore_data_server(id, sondeproj, data_ver, y_var)
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

## Value

Invisible NULL
