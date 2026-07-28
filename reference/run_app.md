# Run the SondePolishR App

Opens a window with the interactive Shiny App to interactively load,
view, correct, and export sonde data.

## Usage

``` r
run_app(default_path = getwd())
```

## Arguments

- default_path:

  Default filepath used for saving data to, defaults to current working
  directory.

## Value

Shiny App

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
  library(SondePolishR)
  run_app()
}} # }
```
