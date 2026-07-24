# Get hourly precipitation at data site

Precipitation is downloaded at an hourly scale based on the provided
coordinates.

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

## Details

There are two different datasets that can be downloaded via this
function:

- [Merra-2](https://gmao.gsfc.nasa.gov/gmao-products/merra-2/):
  Available from NASA power. This dataset is available across a global
  scale at a resolution of 0.5 x 0.625 degrees available from 1981 to
  near real time

- [NLDAS](https://ldas.gsfc.nasa.gov/nldas): Available from NASA
  Earthdata. This dataset is available across CONUS at a resolution of
  0.125 × 0.125 degrees available from 1981 to near real time. This data
  requires a token to access the data. See
  [here](https://urs.earthdata.nasa.gov/documentation/for_users/user_token)
  for directions on creating a token. Note that this token should be
  kept secret.

## Examples

``` r
data <- example_data[example_data$Date == "2024-11-13",]
precip <- get_precip(data, 43.96775, -122.63012, "merra-2")
#> Registered S3 methods overwritten by 'bit64':
#>   method               from  
#>   as.double.integer64  cheapr
#>   as.integer.integer64 cheapr
```
