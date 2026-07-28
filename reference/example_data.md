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

- **fDOM_QSU**/**fDOM_QSU_flag**: Fluorescent dissolved organic matter
  (fDOM) measured in Quinine Sulfate Units (QSU) and flags.

- **ODO_mg_L**/**ODO_mg_L_flag**: Dissolved oxygen measured in mg/L and
  flags.

- **pH**/**pH_flag**: Dissolved oxygen measured in pH units and flags.

- **SpCond_uS_cm**/**SpCond_uS_cm_flag**: Specific conductivity measured
  in µS/cm and flags.

- **Temp_C**/**Temp_C_flag**: Temperature measured in degrees C and
  flags.

- **Turbidity_FNU**/**Turbidity_FNU_flag**: Turbidity measured in
  Formazin Nephelometric Units (FNU) and flags.

## Source

Forest Ecohydrology and Watershed Science Lab (2024) Fall Creek:
2024-07-31 to 2024-10-23. Data set.
