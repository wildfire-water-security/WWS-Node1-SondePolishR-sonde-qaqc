# Apply a drift correction to a data file

Used to account for instrument drift by applying a linear correction to
a data file. Uses differences between check and resident sonde
measurements when available as the default correction amount.

## Usage

``` r
drift_UI(id, sondeproj)

drift_server(
  id,
  sondeproj,
  y_var,
  plot,
  plot_data,
  currplot,
  curredit,
  currmethod
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

## Value

Updates the values of \`currplot\` and \`curredit\` to be current with
the method.
