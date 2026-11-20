# Keep track of visualized version and edits

Keeps track of the version selected and returns the correct data to plot
with any edits to view.

## Usage

``` r
data_version_UI(id)

data_version_server(id, sondeproj, y_var, dd, username)
```

## Arguments

- id:

  the shiny ID of the module

- sondeproj:

  A `reactiveVal` holding the current dataset.

- y_var:

  A `reactiveVal` holding the Y-variable to plot on the y-axis.

- dd:

  A `reactiveVal` holding the name of the dd in the changelog.

- username:

  A `reactiveVal` holding the name of the analyst for the changelog

## Value

A list containing:

- `current`: Reactive expression containing the version of the data
  corresponding to the selected changelog row.

- `changed`: Reactive expression containing the previous values for \#'
  the selected parameter when changes are being displayed.
