# Try to guess csv file encoding

Turns out it's actually quite challenging to load in `.csv` file when it
has a weird encoding. This will use
[guess_encoding](https://readr.tidyverse.org/reference/encoding.html)
first to see if it can figure it out, if not, it will test the two
likely encoding: UTF-16LE or Windows-1252.

## Usage

``` r
get_encoding(file)
```

## Arguments

- file:

  file path to sonde data

## Value

A character with the file encoding

## Examples

``` r
file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-csv-data1.csv")
get_encoding(file)
#> [1] "Windows-1252"
```
