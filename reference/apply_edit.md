# Log edits to a `sondeproj`

Uses a list of edit parameters to update a `sondeproj` with the
specified edits. Makes changes to the data, changelog, and flags.

## Usage

``` r
apply_edit(proj, edit, username)
```

## Arguments

- proj:

  A `sondeproj` object holding sonde data.

- edit:

  A list of length six:

  - data: new updated data as a `data.frame`

  - rows: vector of indices that were changed

  - y_var: parameter being edited

  - step: name of the editing step for the changelog

  - note: an optional note to add to the changelog

  - flag: character flag to use for edits to the data

- username:

  the username of the person who made the change

## Value

A `sondeproj` object with edits made.

## Examples

``` r
data <- example_data
data$fDOM_QSU[1:4] <- NA
rows <- c(1:4)
rows[1:4] <- TRUE
edit <- list(data = example_data,
             rows = rows,
             y_var = "fDOM_QSU",
             step = "outlier removal",
             note = "example edit",
             flag = "RM07")
updated_proj <- apply_edit(example_sondeproj, edit, "Smith")
```
