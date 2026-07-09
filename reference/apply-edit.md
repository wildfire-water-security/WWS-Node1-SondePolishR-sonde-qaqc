# Shiny module to added flags and save changes to data

The UI creates a button to allow user to save the changes made to the
data. The server function will provide a message to the user to let them
know the changes (or lack of changes made), add flags to the dataset,
and save the dataset as a new version.

## Usage

``` r
apply_edit_UI(id, note = NULL)

apply_edit_server(id, sondeproj, edit)
```

## Arguments

- id:

  the shiny ID of the action button

- sondeproj:

  A \`reactiveVal\` holding the current \`sondeproj\`.

- edit:

  A \`reactiveVal\` holding a list of length six: - data: new updated
  data as a \`data.frame\` - rows: logical vector which specifies rows
  changed a TRUE - y_var: parameter being edited - step: name of the
  editing step for the changelog - note: an optional note to add to the
  changelog - flag: character flag to use for edits to the data
