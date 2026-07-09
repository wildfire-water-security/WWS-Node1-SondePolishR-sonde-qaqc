# Sample Precipitation Data

An example of a precipitation data file used to plot with sonde data to
help determine if peak values are real (particularly for fDOM and
turbidity).

## Usage

``` r
example_precip
```

## Format

A `data.frame` with the following columns:

- **DateTime**: The date and time of the observation.

- **Precip_mm_hr**: The hourly precipitation value for that hour in mm.

## Source

The average MERRA-2 bias corrected total precipitation at the surface of
the earth. NASA POWER Project. NASA Langley Research Center (LaRC),
Prediction Of Worldwide Energy Resources (POWER), accessed 2026-06-26.
https://power.larc.nasa.gov/parameters/?parameter=PRECTOTCORR
