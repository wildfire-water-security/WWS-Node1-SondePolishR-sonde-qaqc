# Guess the number of rows to skip to get correct headers

Guess the number of rows to skip to get correct headers

## Usage

``` r
get_skip(file, encoding = NULL)
```

## Arguments

- file:

  file path to sonde data (.xlsx or .csv)

- encoding:

  the file encoding if file is a .csv, will guess if NULL

## Value

a numeric indicating the number of rows to skip

## Examples

``` r
file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-csv-data1.csv")
get_skip(file)
#> [1] 8

file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-usb-example.csv")
get_skip(file)
#> [1] 16
```
