# Sample Field Form

An example of a field form used to collect information about a field
visit to check and perform maintenance. Critically this information is
used to track periods when the sonde was out of the water.

## Usage

``` r
example_fieldform
```

## Format

A `data.frame` with the following columns:

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
