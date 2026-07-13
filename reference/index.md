# Package index

## Run Shiny App

Function for running the interactive shiny app.

- [`run_app()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/run_app.md)
  : Run the SondePolishR App

## Reading and Loading Data

Functions for loading sonde data and metadata.

- [`get_oow()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/get_oow.md)
  : Convert field form data to out of water periods
- [`get_precip()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/get_precip.md)
  : Get hourly precipitation at data site
- [`get_skip()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/get_skip.md)
  : Guess the number of rows to skip to get correct headers
- [`load_project()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/load_project.md)
  : Load and combine sonde project components
- [`read_cal()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/read_cal.md)
  : Read sonde calibration check file
- [`read_ff()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/read_ff.md)
  : Read sonde field form
- [`read_sonde()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/read_sonde.md)
  : Upload raw sonde data

## QA/QC Functions

Functions for tracking data quality and performing corrections.

- [`add_flags()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/add_flags.md)
  : Get skeleton flagging dataframe
- [`apply_drift_shift()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/apply_drift_shift.md)
  : Apply a drift correction to a parameter
- [`apply_dup_edits()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/apply_dup_edits.md)
  : Deals with duplicates in data and documents changes
- [`apply_edit()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/apply_edit.md)
  : Log edits to a \`sondeproj\`
- [`apply_interp()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/apply_interp.md)
  : Map interpolated data back to dataset
- [`correct_fdom()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/correct_fdom.md)
  : Apply fDOM temperature and turbidity corrections
- [`guess_shift()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/guess_shift.md)
  : Guess the amount to shift observations
- [`identify_dups()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/identify_dups.md)
  : Identify duplicated observations with sonde data
- [`identify_gaps()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/identify_gaps.md)
  : Identify missing observations with sonde data
- [`prep_interp()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/prep_interp.md)
  : Prepare interpolation dataset
- [`run_interp()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/run_interp.md)
  : Interpolate data gaps
- [`shift_points()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/shift_points.md)
  : Correct points via an absolute shift

## Version Control Functions

Functions for tracking changes to data.

- [`apply_diff()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/apply_diff.md)
  : Apply a diff object to a dataset
- [`get_diff()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/get_diff.md)
  : Track differences between sonde data
- [`get_raw_data()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/get_raw_data.md)
  : Get raw data from sonde project
- [`write_log()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/write_log.md)
  : Write to Change Log

## Data Export

Functions for plotting and exporting data.

- [`describe_data()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/describe_data.md)
  : Gets summary statistics for Sonde data
- [`nice_yvar()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/nice_yvar.md)
  : Reformats column names to be human readable
- [`plot_sonde()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/plot_sonde.md)
  : Create plotly object of requested sonde data
- [`summarize_data()`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/summarize_data.md)
  : Summarize data to a different time period

## Example Data

Example datasets and metadata.

- [`example_calcheck`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/example_calcheck.md)
  : Sample Calibration Check Data
- [`example_data`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/example_data.md)
  : Sample Sonde Data
- [`example_fieldform`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/example_fieldform.md)
  : Sample Field Form
- [`example_precip`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/example_precip.md)
  : Sample Precipitation Data
- [`example_sondeproj`](https://wildfire-water-security.github.io/WWS-Node1-SondePolishR-sonde-qaqc/reference/example_sondeproj.md)
  : Sample Sonde Project
