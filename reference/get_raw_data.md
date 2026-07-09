# Get raw data from sonde project

Includes any raw data merges and returns the raw data without any
changes. Useful for viewing data before changes were made. Will include
all raw data, even if additional datasets were merged partway through
cleaning.

## Usage

``` r
get_raw_data(proj)
```

## Arguments

- proj:

  A \`sondeproj\` object.

## Value

a \`data.frame\` with raw data.

## Examples

``` r
raw <- get_raw_data(example_sondeproj)
```
