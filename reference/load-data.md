# Read in the dataset or project and set save path

Takes in a dataset as a `.csv` or a sonde project as an `.RDS` file via
file selection. If the data is a sonde project the save path will
default it it's existing path, otherwise the user will need to select a
save path with the file name based on the name of the data file.

## Usage

``` r
load_data_UI(id)

load_data_server(id, sondeproj, data_ver)
```

## Arguments

- id:

  An ID string passed to shiny::NS(), used for namespacing UI
  inputs/outputs.

- sondeproj:

  A `reactiveVal` holding the current dataset.

- data_ver:

  A `reactiveVal` holding a number used to track when new data is added
  to trigger resets.

## Value

The loaded data as a reactive object.
