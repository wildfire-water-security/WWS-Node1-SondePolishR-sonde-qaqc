# Visualize dataset by analyte

Plots loaded dataset with options to explore full dataset or view the
data in weekly sections. Plots are created using `plotly` so they are
interactive. Module also allows the user to view the changes over the
dataset versions via row selection in a table via the `log`.

## Usage

``` r
explore_data_UI(id)

explore_data_server(id, sondeproj, data_ver, y_var, view_state)
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

- view_state:

  A `reactiveVal` holding a list of items specifying the view state:

  - abs_dates: The absolute range of dates within the dataset

  - dates: The range of dates being viewed via the date selector

  - period_view: Logical if the period view is being used

  - period_length: Length of period view

  - period_n: The period number to view.

## Value

Invisible NULL
