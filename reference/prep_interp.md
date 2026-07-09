# Prepare interpolation dataset

Completes the date-time sequence to add missing rows for missing
datetimes. Also creates an interpolation dataset where any duplicates
have been condensed to a single value. In the case of two different
values, they will be set to NA for the purposes of interpolation to not
interpolate with questionable data.

## Usage

``` r
prep_interp(proj)
```

## Arguments

- proj:

  A `sondeproj` object holding sonde data.

## Value

a list of length two:

- fill: `data.frame` based on `proj$data` with missing `datetime` values
  added.

- interp: `data.frame` based on `proj$data` with duplicates condensed to
  a single value.

## Examples

``` r
interp_dfs <- prep_interp(example_sondeproj)
```
