# Get hourly precipitation at data site

Precipitation is downloaded from [NASA
Power](https://power.larc.nasa.gov/) at an hourly scale based on the
provided coordinates.

## Usage

``` r
get_precip(data, lat, long, method, token = NULL)
```

## Arguments

- data:

  the data to get matching precipitation data for

- lat:

  the latitude to get precipitation data at

- long:

  the longitude to get precipitation data at

- method:

  method used to get data either "merra-2" or "nldas"

- token:

  only required for nldas method. see details for how to obtain this
  token.

## Value

a data.frame with two columns:

- DateTime: The datetime (`POSIXct`) in the same timezone as the data,
  at an hourly resolution.

- Precip_mm_hr: Average precipitation at the requested point in mm per
  hour.

## Examples

``` r
data <- example_data[example_data$Date == "2024-11-13",]
precip <- get_precip(data, 43.96775, -122.63012, "merra-2")
#> Registered S3 methods overwritten by 'bit64':
#>   method               from  
#>   as.double.integer64  cheapr
#>   as.integer.integer64 cheapr
```
