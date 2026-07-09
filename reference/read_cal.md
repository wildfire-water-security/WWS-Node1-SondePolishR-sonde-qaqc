# Read sonde calibration check file

Loads a `.csv` file with calibration checks performed during field
visits. Ensures correct column classes and converts NA to R recognized
NA values.

## Usage

``` r
read_cal(file, tz)
```

## Arguments

- file:

  File path to calibration check data (.csv).

- tz:

  Timezone for the times in the form.

## Value

a `data.frame` with the following columns. The `file` to read in should
have the same structure:

- **Date**: The date of the site visit.

- **Site_Code**: The site name or site code.

- **Parameter**: The name of the parameter, should match column names
  from read_sonde.

- **Resident_Probe_Serial**: The serial number for the sonde located at
  the site.

- **Resident_Value**: The measured value for the parameter on the
  resident sonde.

- **Check_Probe_Serial**: The serial number for the sonde used to check
  the calibration. Should be freshly calibrated.

- **Check_Value**: The measured value for the parameter on the check
  sonde.

- **Notes**: Notes associated with the calibration check or data entry.

## Examples

``` r
file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-calcheck.csv")
calcheck <- read_cal(file, tz="Etc/GMT+8")
```
