# Specify Plotting Options

Used to create and return selections about what to include in the plot.

## Usage

``` r
plot_options_UI(id)

plot_options_server(id)
```

## Arguments

- id:

  the shiny ID of the module

## Value

a list of length 5:

- points: should points be plotted?

- line: should line be plotted? -files: should points be colored by
  file? -oow: should out of water periods be plotted? -calcheck: should
  cal check be plotted?
