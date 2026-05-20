#' Sample Sonde Data
#'
#' An example of the output from \link[SondePolishR]{read_sonde} function.
#'
#' @md
#' @format An data.frame object with 8071 rows and 13 columns:
#'  - **Index**: A number, 1 through the number of rows in the `data.frame`.
#'  - **Date**: Date of measurement in the form YYYY-MM-DD.
#'  - **Time_HH_mm_ss**: Time of measurement in the form hh:mm:ss.
#'  - **DateTime**: Date and time of measurement as a POSIXct.
#'  - **DateTime_rd**: Date and time of measurement as a POSIXct rounded to the nearest data interval value.
#'  - **Site_Name**: Name of the site.
#'  - **Battery_V**: Battery voltage when measurement was collected.
#'  - **fDOM_QSU**: Fluorescent dissolved organic matter (fDOM) measured in Quinine Sulfate Units (QSU).
#'  - **ODO_mg_L**: Dissolved oxygen measured in mg/L.
#'  - **pH**: Dissolved oxygen measured in pH units.
#'  - **SpCond_uS_cm**: Specific conductivity measured in µS/cm.
#'  - **Temp_C**: Temperature measured in degrees C.
#'  - **Turbidity_FNU**: Turbidity measured in Formazin Nephelometric Units (FNU).
#' @source Forest Ecohydrology and Watershed Science Lab (2024) Fall Creek: 2024-07-31 to 2024-10-23. Data set.

"example_data"

#' Sample Sonde Project
#'
#' An example of a sonde project, which is a list type object that stores the data, associated metadata, and
#' tracks changes made to the data.
#'
#' @md
#' @format An object of class `sondeproj` with the following elements:
#' - **data**: A `data.frame` of the dataset, updated with any changes.
#' - **flags**: A list of length three:
#'  - **flag_rm**: A `data.frame` with the same measurement columns with _flag appended to the name and the same number of rows.
#'    Used to store flag values for values that were removed.
#'  - **flag_chg**: A `data.frame` with the same measurement columns with _flag appended to the name and the same number of rows.
#'    Used to store flag values for values that were altered.
#'  - **flag_add**: A `data.frame` with the same measurement columns with _flag appended to the name and the same number of rows.
#'    Used to store flag values for values that were added (likely through interpolation).
#' - **fieldform**: A `data.frame` with the field form data. See `example_fieldform` for details on the structure.
#' - **calcheck**: A `data.frame` with the calibration check data. See `example_calcheck` for details on the structure.
#' - **diffs**: A list of `data_diff` which stores the changes made to `data`. See `daff` package for more details on this structure.
#' - **changelog**: A `data.frame` which stores a summary of the changes made in each `data_diff`:
#'  - **datetime**: The date and time the change was made.
#'  - **parameter**:  The parameter that the change was made to.
#'  - **step**: The name of the processing step the change was made in.
#'  - **nchanged**: The number of points that were changed.
#'  - **note**: Notes added about the changes made.
#'  - **user**: The username of the person who made the change.
#'  - **diff_name**: The name of the associated `data.diff`.

"example_sondeproj"

#' Sample Field Form
#'
#' An example of a field form used to collect information about a field visit to
#' check and perform maintenance. Critically this information is used to track periods
#' when the sonde was out of the water.
#'
#' @md
#' @format A `data.frame` with the following columns:
#' - **Date**: The date of the site visit.
#' - **Site_Code**: The site name or site code.
#' - **Time_PST**: The start time of the field visit (used to guess out of water periods when data is missing).
#' - **Start_Sonde_Serial**: The serial number for the sonde in the water at the start of the visit.
#' - **Start_Sonde_Name**: The sonde name for the sonde in the water at the start of the visit.
#' - **End_Sonde_Serial**: The serial number for the sonde in the water at the end of the visit.
#' - **End_Sonde_Name**: The sonde name for the sonde in the water at the end of the visit.
#' - **Removal_Time_PST**: The time the sonde was removed from the water.
#' - **Return_Time_PST**: The time the sonde was returned to the water.
#' - **Next_Timepoint_PST**: The next timepoint that data will be collected.
#'  This should be the next "good" time point after the sonde has been returned to the water, but is sometimes the timepoint
#'  when the wiper is checked and the sonde is out of the water.
#' - **Data_Download**: Logical, was data downloaded at this visit?
#' - **Download_Device**: The name of the device data was downloaded to.
#' - **Remove_Period**: Logical, is there a data disruption that merits removing data during the out of water period. Used to skip periods
#' where we have coarse out of water periods (missing times), but the data appears uninterupted to prevent removing excess data.
#' - **Crew**: The names or initials of the people performing the field visit.
#' - **Weather**: A description of the weather during the field visit.
#' - **Notes**: Notes associated with the field visit.
#' - **DataEntry_Notes**: Additional notes from data entry. Can have any name that includes "Notes"

"example_fieldform"

#' Sample Calibration Check Data
#'
#' An example of a calibration check form which tracks the results of calibration
#' checks during field visits. This data is collected by comparing measurements of the
#' "resident" sonde directly next to a freshly calibrated "check" sonde.
#'
#' @md
#' @format A `data.frame` with the following columns:
#' - **Date**: The date of the site visit.
#' - **Site_Code**: The site name or site code.
#' - **Parameter**: The name of the parameter, should match column names from read_sonde.
#' - **Resident_Probe_Serial**: The serial number for the sonde located at the site.
#' - **Resident_Value**: The measured value for the parameter on the resident sonde.
#' - **Check_Probe_Serial**: The serial number for the sonde used to check the calibration. Should be freshly calibrated.
#' - **Check_Value**: The measured value for the parameter on the check sonde.
#' - **Notes**: Notes associated with the calibration check or data entry.

"example_calcheck"

