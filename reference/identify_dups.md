# Identify duplicated observations with sonde data

Uses the interval-rounded datetime (`DateTime_rd`) observations to
identify periods of duplicated observations. Based on if they match and
the length of the section it identifies a likely cause of the
duplication.

## Usage

``` r
identify_dups(data)
```

## Arguments

- data:

  a `data.frame` with Sonde data.

## Value

a `data.frame` with the following columns with a row for each duplicated
period:

- start: starting datetime of the duplicated data

- end: ending datetime of the duplicated data

- duptype: either "same file" or "multiple files" indicating the type of
  duplication

- ndif: number of points with the duplicated section that are not
  exactly the same

- length: number of points in the duplicated section

- perc_dif: percentage of the data that's different

- likely_issue: based on information about the duplicates, a guess about
  the cause of the duplicate

- user_note: user input information about the duplicated section

## Examples

``` r
identify_dups(example_data)
#> NULL
```
