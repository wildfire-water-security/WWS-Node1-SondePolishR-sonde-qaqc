# Log edits to a \`sondeproj\`

Uses a list of edit parameters to update a \`sondeproj\` with the
specified edits. Makes changes to the data, changelog, and flags.

## Usage

``` r
apply_edit(proj, edit)
```

## Arguments

- proj:

  A \`sondeproj\` object holding sonde data.

- edit:

  A list of length six: - data: new updated data as a \`data.frame\` -
  rows: logical vector which specifies rows changed as TRUE - y_var:
  parameter being edited - step: name of the editing step for the
  changelog - note: an optional note to add to the changelog - flag:
  character flag to use for edits to the data - changetype: character
  specifying where to add flag, either "flag_rm", "flag_chg", or
  "flag_add"

## Value

A \`sondeproj\` object with edits made.

## Examples

``` r
data <- example_data
data$fDOM_QSU[1:4] <- NA
rows <- rep(FALSE, nrow(data))
rows[1:4] <- TRUE
edit <- list(data = example_data,
             rows = rows,
             y_var = "fDOM_QSU",
             step = "outlier removal",
             note = "example edit",
             flag = "RM07",
             changetype = "flag_rm")
updated_proj <- apply_edit(example_sondeproj, edit)
```
