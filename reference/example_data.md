# Sample Sonde Data

An example of the output from
[read_sonde](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/read_sonde.md)
function.

## Usage

``` r
example_data
```

## Format

An data.frame object with 8071 rows and 13 columns:

- **Index**: A number, 1 through the number of rows in the `data.frame`.

- **Date**: Date of measurement in the form YYYY-MM-DD.

- **Time_HH_mm_ss**: Time of measurement in the form hh:mm:ss.

- **DateTime**: Date and time of measurement as a POSIXct.

- **DateTime_rd**: Date and time of measurement as a POSIXct rounded to
  the nearest data interval value.

- **Site_Name**: Name of the site.

- **Battery_V**: Battery voltage when measurement was collected.

- **fDOM_QSU**: Fluorescent dissolved organic matter (fDOM) measured in
  Quinine Sulfate Units (QSU).

- **ODO_mg_L**: Dissolved oxygen measured in mg/L.

- **pH**: Dissolved oxygen measured in pH units.

- **SpCond_uS_cm**: Specific conductivity measured in µS/cm.

- **Temp_C**: Temperature measured in degrees C.

- **Turbidity_FNU**: Turbidity measured in Formazin Nephelometric Units
  (FNU).

## Source

Forest Ecohydrology and Watershed Science Lab (2024) Fall Creek:
2024-07-31 to 2024-10-23. Data set.
