# Read sonde field form

Loads a `.csv` file with field visit metadata. Ensures correct column
classes and cleans up extra notes.

## Usage

``` r
read_ff(file, tz)
```

## Arguments

- file:

  File path to field form data (.csv).

- tz:

  Timezone for the times in the form.

## Value

a `data.frame` with the following columns. The `file` to read in should
have the same structure:

- **Date**: The date of the site visit.

- **Site_Code**: The site name or site code.

- **Time**: The start time of the field visit (used to guess out of
  water periods when data is missing).

- **Start_Sonde_Serial**: The serial number for the sonde in the water
  at the start of the visit.

- **Start_Sonde_Name**: The sonde name for the sonde in the water at the
  start of the visit.

- **End_Sonde_Serial**: The serial number for the sonde in the water at
  the end of the visit.

- **End_Sonde_Name**: The sonde name for the sonde in the water at the
  end of the visit.

- **Removal_Time**: The time the sonde was removed from the water.

- **Return_Time**: The time the sonde was returned to the water.

- **Next_Timepoint**: The next timepoint that data will be collected.
  This should be the next "good" time point after the sonde has been
  returned to the water, but is sometimes the timepoint when the wiper
  is checked and the sonde is out of the water.

- **Data_Download**: Logical, was data downloaded at this visit?

- **Download_Device**: The name of the device data was downloaded to.

- **Remove_Period**: Logical, is there a data disruption that merits
  removing data during the out of water period. Used to skip periods
  where we have coarse out of water periods (missing times), but the
  data appears uninterrupted to prevent removing excess data.

- **Crew**: The names or initials of the people performing the field
  visit.

- **Weather**: A description of the weather during the field visit.

- **Notes**: Notes associated with the field visit.

- **DataEntry_Notes**: Additional notes from data entry. Can have any
  name that includes "Notes"

## Examples

``` r
file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-fieldform.csv")
fieldform <- read_ff(file, tz="Etc/GMT+8")
```
