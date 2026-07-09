# Sample Calibration Check Data

An example of a calibration check form which tracks the results of
calibration checks during field visits. This data is collected by
comparing measurements of the "resident" sonde directly next to a
freshly calibrated "check" sonde.

## Usage

``` r
example_calcheck
```

## Format

A `data.frame` with the following columns:

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
