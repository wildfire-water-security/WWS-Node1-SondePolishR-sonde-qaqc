# Read in the dataset or project and set save path

Takes in a dataset as a `.csv` or a sonde project as an `.RDS` file via
file selection. If the data is a sonde project the save path will
default it it's existing path, otherwise the user will need to select a
save path with the file name based on the name of the data file.

## Usage

``` r
load_data_UI(id)

load_data_server(id, sondeproj, data_ver, view_state)
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

- view_state:

  A `reactiveVal` holding a list of items specifying the view state:

  - abs_dates: The absolute range of dates within the dataset

  - dates: The range of dates being viewed via the date selector

  - period_view: Logical if the period view is being used

  - period_length: Length of period view

  - period_n: The period number to view.

## Value

The loaded data as a reactive object.
