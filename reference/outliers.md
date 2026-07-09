# Flag data identified as outliers either manually or view methods

Looks for "weird" data where there are large spikes within a short
period that are likely unrealistic and caused by instrument malfunction
or a bubble near the sensor.

## Usage

``` r
outlier_UI(id)

outlier_server(id, sondeproj, data_ver, y_var)
```

## Arguments

- id:

  An ID string passed to shiny::NS(), used for namespacing UI
  inputs/outputs.

- sondeproj:

  A \`reactiveVal\` holding the current dataset.

- data_ver:

  A \`reactiveVal\` holding a number used to track when new data is
  added to trigger resets.

- y_var:

  Y-variable to plot on the y-axis.
