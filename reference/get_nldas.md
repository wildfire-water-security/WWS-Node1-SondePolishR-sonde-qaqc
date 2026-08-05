# Download NLDAS Hourly Precipitation

Downloads precipitation data from
\[NLDAS\](https://ldas.gsfc.nasa.gov/nldas). Available from NASA
Earthdata. This dataset is available across CONUS at a resolution of
0.125 × 0.125 degrees available from 1981 to near real time. This data
requires a token to access the data. See
\[here\](https://urs.earthdata.nasa.gov/documentation/for_users/user_token)
for directions on creating a token. Note that this token should be kept
secret.

## Usage

``` r
get_nldas(token, lat, long, start, end)
```

## Arguments

- token:

  Character of token

- lat:

  the latitude to get precipitation data at

- long:

  the longitude to get precipitation data at

- start:

  Start date to get precipitation data (should be a date or datetime)

- end:

  End date to get precipitation data (should be a date or datetime)

## Value

a data.frame with two columns: - DateTime: The datetime (\`POSIXct\`) in
UTC, at an hourly resolution. - Precip_mm_hr: Average precipitation at
the requested point in mm per hour.

## Details

You can store your token (expires every two months) in you R environment
to reference it safely using: \`usethis::edit_r_environ()\` to open the
environ file. Add the text EARTHDATA_TOKEN = \###. Access your stored
token via: \`Sys.getenv("EARTHDATA_TOKEN")\`

## Examples

``` r
if(nzchar(Sys.getenv("earthdata_token"))){
  precip <- get_nldas(Sys.getenv("earthdata_token"), 43.96, -122.63,
                      as.Date("2024-07-31"), as.Date("2024-08-01"))
}
```
