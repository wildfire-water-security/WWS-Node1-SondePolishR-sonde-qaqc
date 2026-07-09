# Load and combine sonde project components

Uses provided filepaths and to read in, clean/format, and merge together
components of a `sondeproj`.

## Usage

``` r
load_project(
  csv_path = NULL,
  csv_files = NULL,
  prj_path = NULL,
  ff_path = NULL,
  cc_path = NULL,
  tz = "Etc/GMT+8",
  site = NULL,
  update_pb = NULL
)
```

## Arguments

- csv_path:

  character vector with the filepaths to sonde data files, can be more
  than one.

- csv_files:

  names of the csv files, primarily used for shiny input which doesn't
  store filenames with paths.

- prj_path:

  character vector with the filepath to a sonde project.

- ff_path:

  character vector with the filepath to a field form file.

- cc_path:

  character vector with the filepath to a calibration check file.

- tz:

  the timezone for the sonde data

- site:

  the site name or site code.

- update_pb:

  takes a function used to update a progress bar in a shiny interface.

## Value

a `sondeproj` object. For more details on structure see
`example_sondeproj`

## Examples

``` r
file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-csv-data1.csv")
proj <- load_project(csv_path = file, csv_files = "example_file1")
#> Error in utf8::as_utf8(text): entry 9 has wrong Encoding; marked as "UTF-8" but invalid leading byte (0xB5) at position 68
```
