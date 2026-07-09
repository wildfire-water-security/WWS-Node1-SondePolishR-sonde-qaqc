# Address any additive shifts

Plots loaded dataset, user can select a group of points and apply a
additive shift to the data to correct for shifts.

## Usage

``` r
additive_UI(id)

additive_server(id, sondeproj, data_ver, y_var)
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
