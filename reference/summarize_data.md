# Summarize data to a different time period

Used to take data and aggregate it to a different interval.

## Usage

``` r
summarize_data(data, frequency, sum_method = c("mean", "median", "max", "min"))
```

## Arguments

- data:

  a `data.frame` to summarize

- frequency:

  a `Period` object created using
  [`lubridate::period()`](https://lubridate.tidyverse.org/reference/period.html)
  specifying the time frame to summarize over

- sum_method:

  the summary method to use to summarize the data, can choose more than
  one

## Value

a \`data.frame where the summary method has been appended to each
parameter column name.

## Examples

``` r
summarize_data(example_sondeproj$data, lubridate::period(1, "month"), "mean")
#> # A tibble: 6 × 13
#>   DateTime_rd         ODO_mg_L_mean ODO_mg_L_flag SpCond_uS_cm_mean
#>   <dttm>                      <dbl> <chr>                     <dbl>
#> 1 2024-07-01 00:00:00          8.33 CH01                       63.4
#> 2 2024-08-01 00:00:00          8.85 NA                         66.8
#> 3 2024-09-01 00:00:00          9.43 NA                         68.9
#> 4 2024-10-01 00:00:00         10.3  NA                         69.6
#> 5 2024-11-01 00:00:00         10.7  NA                         45.8
#> 6 2024-12-01 00:00:00         11.4  NA                         43.3
#> # ℹ 9 more variables: SpCond_uS_cm_flag <chr>, Temp_C_mean <dbl>,
#> #   Temp_C_flag <chr>, Turbidity_FNU_mean <dbl>, Turbidity_FNU_flag <chr>,
#> #   fDOM_QSU_mean <dbl>, fDOM_QSU_flag <chr>, pH_mean <dbl>, pH_flag <chr>

#using multiple methods
summarize_data(example_sondeproj$data, lubridate::period(1, "month"), c("mean", "median", "max"))
#> # A tibble: 6 × 25
#>   DateTime_rd         fDOM_QSU_mean fDOM_QSU_median fDOM_QSU_max fDOM_QSU_flag
#>   <dttm>                      <dbl>           <dbl>        <dbl> <chr>        
#> 1 2024-07-01 00:00:00          9.72            9.73          9.9 RM01         
#> 2 2024-08-01 00:00:00         10.9            10.1         171.  NA           
#> 3 2024-09-01 00:00:00         11.8            10.5          28.9 NA           
#> 4 2024-10-01 00:00:00         13.0             9.69         32.8 NA           
#> 5 2024-11-01 00:00:00         16.9            15.2          34.4 NA           
#> 6 2024-12-01 00:00:00         11.8            10.7          22.0 NA           
#> # ℹ 20 more variables: ODO_mg_L_mean <dbl>, ODO_mg_L_median <dbl>,
#> #   ODO_mg_L_max <dbl>, ODO_mg_L_flag <chr>, pH_mean <dbl>, pH_median <dbl>,
#> #   pH_max <dbl>, pH_flag <chr>, SpCond_uS_cm_mean <dbl>,
#> #   SpCond_uS_cm_median <dbl>, SpCond_uS_cm_max <dbl>, SpCond_uS_cm_flag <chr>,
#> #   Temp_C_mean <dbl>, Temp_C_median <dbl>, Temp_C_max <dbl>,
#> #   Temp_C_flag <chr>, Turbidity_FNU_mean <dbl>, Turbidity_FNU_median <dbl>,
#> #   Turbidity_FNU_max <dbl>, Turbidity_FNU_flag <chr>
```
