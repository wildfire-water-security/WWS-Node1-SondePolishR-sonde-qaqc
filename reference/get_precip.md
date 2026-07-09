# Get hourly precipitation at data site

Precipitation is downloaded from [NASA
Power](https://power.larc.nasa.gov/) at an hourly scale based on the
provided coordinates.

## Usage

``` r
get_precip(data, lat, long)
```

## Arguments

- data:

  the data to get matching precipitation data for

- lat:

  the latitude to get precipitation data at

- long:

  the longitude to get precipitation data at

## Value

a data.frame with two columns:

- DateTime: The datetime (`POSIXct`) in the same timezone as the data,
  at an hourly resolution.

- Precip_mm_hr: Average MERRA-2 bias corrected total precipitation at
  the surface of the earth in mm per hour.

## Examples

``` r
data <- example_data[example_data$Date == "2024-08-01",]
get_precip(data, 43.96775, -122.63012)
#> Registered S3 methods overwritten by 'bit64':
#>   method               from  
#>   as.double.integer64  cheapr
#>   as.integer.integer64 cheapr
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── NASA/POWER Source Native Resolution Hourly Data  ────────────────────────────
#> Dates (month/day/year): 08/01/2024 through 08/02/2024 in UTC
#> Location: Latitude 43.9678 Longitude -122.6301
#> Elevation from MERRA-2: Average for 0.5 x 0.625 degree lat/lon region = 799.16
#> meters
#> The value for missing source data that cannot be computed or is outside of the
#> sources availability range: NA
#> Parameter(s):
#> ────────────────────────────────────────────────────────────────────────────────
#> Parameters:
#> PRECTOTCORR MERRA-2 Precipitation Corrected (mm/day)
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 24 × 2
#>    DateTime            Precip_mm_hr
#>    <dttm>                     <dbl>
#>  1 2024-08-01 00:00:00            0
#>  2 2024-08-01 01:00:00            0
#>  3 2024-08-01 02:00:00            0
#>  4 2024-08-01 03:00:00            0
#>  5 2024-08-01 04:00:00            0
#>  6 2024-08-01 05:00:00            0
#>  7 2024-08-01 06:00:00            0
#>  8 2024-08-01 07:00:00            0
#>  9 2024-08-01 08:00:00            0
#> 10 2024-08-01 09:00:00            0
#> # ℹ 14 more rows
```
