# Specify Plotting Options

Used to create and return selections about what to include in the plot.

## Usage

``` r
plot_options_UI(id, start_val = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))

plot_options_server(id)
```

## Arguments

- id:

  the shiny ID of the module

- start_val:

  a vector the same length as the list with the initial values to use
  for the plotting options.

## Value

a list of length 5:

- points: should points be plotted?

- line: should line be plotted? -files: should points be colored by
  file? -oow: should out of water periods be plotted? -calcheck: should
  cal check be plotted?
