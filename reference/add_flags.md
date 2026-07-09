# Get skeleton flagging dataframe

Works to provide a skeleton for the flag data (if no parameter or flag
names are provided).

## Usage

``` r
add_flags(proj, data)
```

## Arguments

- proj:

  a `sondeproj` object to add the flags to. If it has existing flags
  they will be merged with the new flags from `data` with existing
  flags.

- data:

  a data.frame with sonde data

## Value

a data.frame

- if `par` and `flag_name` are `NULL` it will return a `data.frame` with
  the same number of rows as `data` but with a blank column for each
  parameter in the `data.frame` only the index, datetimme, datetime_rd,
  and DupNum columns.

## Examples

``` r
#add flag columns
updated_proj <- add_flags(example_sondeproj, example_data)
colnames(updated_proj$flags$flag_rm)
#>  [1] "Index"         "DupNum"        "DateTime"      "DateTime_rd"  
#>  [5] "fDOM_QSU"      "ODO_mg_L"      "pH"            "SpCond_uS_cm" 
#>  [9] "Temp_C"        "Turbidity_FNU"
```
