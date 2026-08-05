# Visualize main data plot

Comes with an adjustable y-axis to adjust the values being viewed.

## Usage

``` r
main_plot_UI(id)

main_plot_server(
  id,
  sondeproj,
  plot_obj,
  plot_data,
  y_var,
  sel_mode = FALSE,
  plot_exist = reactiveVal(),
  startmin = reactiveVal(),
  startmax = reactiveVal()
)
```

## Arguments

- id:

  the shiny ID of the module

- sondeproj:

  A \`reactiveVal\` holding the current dataset.

- plot_obj:

  Plotly object to plot.

- plot_data:

  A \`reactiveVal\` holding the current dataset to plot.

- y_var:

  A \`reactiveVal\` holding the Y-variable to plot on the y-axis.

- sel_mode:

  Logical, should selection mode be turned on as default?

- plot_exist:

  A \`reactiveVal\` indicating if the plot exists or not to prevent
  warnings about plot obj not being registered.

## Value

a plot of the data.
