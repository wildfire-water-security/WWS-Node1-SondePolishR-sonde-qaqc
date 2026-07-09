# Reformats column names to be human readable

Reformats column names to be human readable

## Usage

``` r
nice_yvar(data)
```

## Arguments

- data:

  the dataframe you want to column names from

## Value

a named vector where the names are the human readable names and the
values are the column names

## Examples

``` r
nice_yvar(example_data)
#>                   fDOM (QSU)      Dissolved Oyxgen (mg/L) 
#>                   "fDOM_QSU"                   "ODO_mg_L" 
#>                           pH Specific Conductance (μS/cm) 
#>                         "pH"               "SpCond_uS_cm" 
#>             Temperature (°C)              Turbidity (FNU) 
#>                     "Temp_C"              "Turbidity_FNU" 
```
