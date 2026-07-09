# Convert field form data to out of water periods

Use the information contained in the project field form and use the
removal and return times to determine the times the sonde was out of the
water (OOW) for plotting and data QA/QC. Determines the start and end
times of when the sonde was out of the water. If a time is missing it
will try to guess using the visit time, next timepoint, or when no other
options will remove the full day. If \`Remove_Period\` is FALSE it will
ignore that period as an OOW period.

## Usage

``` r
get_oow(ff, tz, interval)
```

## Arguments

- ff:

  Field form \`data.frame\`.

- tz:

  Timezone for the times in the form.

- interval:

  Time (in minutes) between measurements, removes the measurement before
  and after OOW period for safety.

## Value

A \`data.frame\`with three columns: - \*\*site_code\*\*: The site code
associated with the OOW period. - \*\*start\*\*: The start of the OOW
period stored as a \`POSIXct\`. - \*\*end\*\*: The end of the OOW
period, stored as a \`POSIXct\`

## Examples

``` r
get_oow(example_sondeproj$fieldform, tz="Etc/GMT+8", interval=15)
#>   site_code               start                 end
#> 1       FAL 2024-07-30 23:45:00 2024-07-31 12:15:00
#> 2       FAL 2024-08-20 10:30:00 2024-08-20 11:30:00
#> 3       FAL 2024-10-23 13:30:00 2024-10-23 15:00:00
```
