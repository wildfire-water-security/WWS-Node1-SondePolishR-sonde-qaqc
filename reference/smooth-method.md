# Apply a smoothing correction to a data file

Used to smooth out messy data based on optical interference.

## Usage

``` r
smooth_UI(id)

smooth_server(
  id,
  sondeproj,
  y_var,
  plot,
  plot_data,
  currplot,
  curredit,
  currmethod,
  index
)
```

## Arguments

- id:

  the shiny ID of the module

- sondeproj:

  A \`reactiveVal\` holding the current dataset.

- y_var:

  A \`reactiveVal\` holding the y-variable being plotted.

- plot:

  A \`reactiveVal\` holding a basic plot before things were added.

- plot_data:

  A \`reactiveVal\` holding the current data being plotted.

- currplot:

  A \`reactiveVal\` holding the current main plot.

- curredit:

  A \`reactiveVal\` holding the current edit object.

- currmethod:

  A \`reactiveVal\` holding the current correction method.

- index:

  A \`reactiveVal\` holding the indices of the selected points within
  the full dataset.

## Value

Updates the values of \`currplot\` and \`curredit\` to be current with
the method.
