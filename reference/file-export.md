# File save selector UI

File save selector UI

## Usage

``` r
save_path_UI(id, button_label = "Export")

save_path_server(
  id,
  data,
  startname = "sonde_export",
  label = "Choose Location",
  title = "Select save path",
  filetype = ".csv"
)
```

## Arguments

- id:

  module id

- data:

  data to save to specified file on click

- startname:

  the default name for the file

- label:

  button label

- title:

  dialog title

- filetype:

  file extension
