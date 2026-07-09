# Fill in data gaps using interpolation

Attempts to fill gaps less than the max length using the specified
interpolation method.

## Usage

``` r
interp_UI(id)

interp_server(id, sondeproj, data_ver, y_var)
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
