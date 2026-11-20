# Allow interactive review of data changes

Creates UI and backend to accept/reject changes, including adding notes.

## Usage

``` r
data_review_side(id)

data_review_bottom(id)

data_review_server(id, sondeproj, mode, y_var, username)
```

## Arguments

- id:

  the shiny ID of the module

- sondeproj:

  A `reactiveVal` holding the current dataset.

- mode:

  A `reactiveVal` with the review mode (analyst, reviewer, everything)

- y_var:

  A `reactiveVal` holding the Y-variable to plot on the y-axis.

- username:

  A `reactiveVal` holding the name of the analyst for the changelog

## Value

the diff name that's selected for showing the changed points
