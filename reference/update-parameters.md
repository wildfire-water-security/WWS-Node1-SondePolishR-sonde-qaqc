# Shiny module to update plotting parameters

Used to dynamically update the choices for selecting a y variable based
on the names in the data or a custom function.

## Usage

``` r
update_parms_UI(id, input_id = "y_var", text = "Select Parameter to Plot:")

update_parms_server(
  id,
  sondeproj,
  data_ver,
  y_var,
  input_id = "y_var",
  choices_fun = NULL
)
```

## Arguments

- id:

  An ID string passed to shiny::NS(), used for namespacing UI
  inputs/outputs.

- text:

  Text associated with the UI

- sondeproj:

  A `reactiveVal` holding the current dataset.

- data_ver:

  A `reactiveVal` holding a number used to track when new data is added
  to trigger resets.

- y_var:

  Y-variable to plot on the y-axis.

- choices_fun:

  Function used to determine the parameter choices, if `NULL` will use
  the column names of the data

## Value

the selected variable `y_var` as a reactive object
