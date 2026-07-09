# Flag data as questionable

Allows the user to mark data that looks for "weird" and can be viewed as
a plotting option or used to auto-select as an outlier.

## Usage

``` r
quality_UI(id)

quality_server(id, sondeproj, data_ver, y_var)
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
