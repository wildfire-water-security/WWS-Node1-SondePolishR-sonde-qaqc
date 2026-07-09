# Gets summary statistics for Sonde data

Used to quickly generate summary statistics about each parameter for a
given dataset.

## Usage

``` r
describe_data(data, precip = NULL)
```

## Arguments

- data:

  a `data.frame` with sonde dataset.

- precip:

  Optional precipitation dataset.

## Value

a `data.frame`

## Examples

``` r
describe_data(example_data)
#>                      Parameter   Mean Median  Maximum Minimum Std_Deviation
#> 1                   fDOM (QSU) 12.866 10.740  170.610  -2.560         5.318
#> 2      Dissolved Oyxgen (mg/L) 10.104 10.190   12.340   7.830         0.996
#> 3                           pH  7.566  7.490    8.370   7.080         0.208
#> 4 Specific Conductance (μS/cm) 59.131 66.300  145.400   0.100        12.714
#> 5             Temperature (°C) 12.938 12.498   24.557   3.783         5.322
#> 6              Turbidity (FNU)  3.212  0.900 1149.840  -0.160        14.668
#> 7      Precipitation (mm hr⁻¹)     NA     NA       NA      NA            NA
#>   Quantile_1st Quantile_3rd Number_NAs
#> 1        9.630       14.492          0
#> 2        9.320       10.850          0
#> 3        7.430        7.660          0
#> 4       46.600       68.800          0
#> 5        8.389       17.546          0
#> 6        0.710        1.750          0
#> 7           NA           NA         NA
```
