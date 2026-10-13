# Generate a change report from a sonde project

Use the version control information to create a `.pdf` report

## Usage

``` r
generate_report(proj, username, export_path)
```

## Arguments

- proj:

  the `sondeproj` you want to generate the report for

- username:

  the username of the person generating the report

- export_path:

  the path including the file name to save the report to, should end
  with a `.pdf`

## Value

a `.pdf` document with a page for each line in the change log of the
`sondeproj`.

## Examples

``` r
temp_report <- withr::local_tempfile(fileext = ".pdf")
generate_report(example_sondeproj, "Smith", temp_report)
#> Warning: error in running command
#> ! sh: 1: xelatex: not found
#> Error: LaTeX failed to compile file20cd76892c22.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See file20cd76892c22.log for more info.
```
