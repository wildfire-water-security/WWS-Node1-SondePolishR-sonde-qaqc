# Summarize data to a different time period

Used to take data and aggregate it to a different interval.

## Usage

``` r
summarize_data(data, frequency, sum_method)
```

## Arguments

- data:

  a `data.frame` to summarize

- frequency:

  a `Period` object created using
  [`lubridate::period()`](https://lubridate.tidyverse.org/reference/period.html)
  specifying the time frame to summarize over

- sum_method:

  the summary method to use to summarize the data

## Value

a \`data.frame.

## Examples

``` r
summarize_data(example_data, lubridate::period(1, "month"), "mean")
#> # A tibble: 6 × 7
#>   DateTime_rd         ODO_mg_L SpCond_uS_cm Temp_C Turbidity_FNU fDOM_QSU    pH
#>   <dttm>                 <dbl>        <dbl>  <dbl>         <dbl>    <dbl> <dbl>
#> 1 2024-07-01 00:00:00     8.44         63.4  22.1          0.675     9.72  7.94
#> 2 2024-08-01 00:00:00     8.85         66.8  19.9          1.28     10.9   7.68
#> 3 2024-09-01 00:00:00     9.43         68.9  17.2          0.869    11.8   7.65
#> 4 2024-10-01 00:00:00    10.3          69.6  11.9          1.41     13.0   7.61
#> 5 2024-11-01 00:00:00    10.7          45.8   8.24         4.99     16.9   7.46
#> 6 2024-12-01 00:00:00    11.4          43.3   6.91         7.85     11.8   7.41
```
