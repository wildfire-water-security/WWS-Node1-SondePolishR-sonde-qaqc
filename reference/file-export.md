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
  filetype = reactiveVal(".csv"),
  username,
  type,
  data_ver
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

- username:

  A `reactiveVal` holding the name of the analyst for the changelog

- type:

  a `reactiveVal` holding a character specifying the type of data being
  saved options include "sondeproj", "data", "dups", "gaps",
  "changelog", "precip", "calcheck", "fieldform", "change-report"

- data_ver:

  A `reactiveVal` holding a number used to track when new data is added
  to trigger resets.
