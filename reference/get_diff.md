# Track differences between sonde data

Compares the values between the two \`data.frames\` and creates a small
\`diff\` object that stores the datetimes of the change and the new and
old values. Used to move between versions. Note that this function does
not currently support addition and removal of columns, but row
additions/subtractions are supported.

## Usage

``` r
get_diff(olddata, newdata, id = "DateTime", ignore = NA)
```

## Arguments

- olddata:

  a \`data.frame\` with the original data.

- newdata:

  a \`data.frame\` with the revised data.

- id:

  name of the column name used to match observations between \`olddata\`
  and \`newdata\`. Must be convertible to a number.

- ignore:

  a character vector including any column names to not track.

## Value

A \`diff\` object with a named list item for each column being tracked.
Each list item will either be \`NULL\` if there were no changes to that
column or have the following structure: - op_type: a character
describing the type of change made - id: the id values for the change
made - new_data: the values of the changed values in \`newdata\` -
old_data: the values of the changed values in \`olddata\`

## Examples

``` r
data1 <- example_data[1:10,]
data2 <- data1
data2$fDOM_QSU[1:4] <- NA
get_diff(data1, data2)
#> $Index
#> NULL
#> 
#> $DupNum
#> NULL
#> 
#> $FileName
#> NULL
#> 
#> $Date
#> NULL
#> 
#> $Time_HH_mm_ss
#> NULL
#> 
#> $DateTime_rd
#> NULL
#> 
#> $Site_Name
#> NULL
#> 
#> $Battery_V
#> NULL
#> 
#> $fDOM_QSU
#> # A tibble: 4 × 4
#>   DateTime              old   new op_type     
#>   <dttm>              <dbl> <dbl> <chr>       
#> 1 2024-07-31 12:00:00  9.95    NA data_removed
#> 2 2024-07-31 12:15:00  9.87    NA data_removed
#> 3 2024-07-31 12:30:00  9.72    NA data_removed
#> 4 2024-07-31 12:45:00  9.69    NA data_removed
#> 
#> $ODO_mg_L
#> NULL
#> 
#> $pH
#> NULL
#> 
#> $SpCond_uS_cm
#> NULL
#> 
#> $Temp_C
#> NULL
#> 
#> $Turbidity_FNU
#> NULL
#> 
#> attr(,"class")
#> [1] "diff"
```
