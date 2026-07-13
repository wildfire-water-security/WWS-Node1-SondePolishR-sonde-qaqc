# Write to Change Log

The change log is a component of a `sondeproj` object used to keep track
of the changes made to the initial raw sonde data.

## Usage

``` r
write_log(
  sondeproj,
  par,
  step,
  n,
  note = "",
  diff_name = NULL,
  datetime = Sys.time(),
  user = Sys.info()[["user"]],
  return = "df"
)
```

## Arguments

- sondeproj:

  the sonde project to get the change log from. If `NULL` will return a
  blank change log structure.

- par:

  the name of the parameter modified

- step:

  a description of the type of change made

- n:

  the number of points modified

- note:

  a note from the analyst about the change made

- diff_name:

  the associated diff object

- datetime:

  the date and time the change was made

- user:

  the username of the person who made the change

- return:

  either `df` or `sondeproj` to specify if the changelog only should be
  returned or the `sondeproj` with the change log updated.

## Value

If `return` is `df` returns the changelog as a `data.frame`. If `return`
is `sondeproj` the `sondeproj` is returned with the change log updated.

## Examples

``` r
write_log(NULL, "Cond_S_cm", "physical limits", 5, "making an example", "diff1")
#>              datetime parameter            step n_changed              note
#> 1 2026-07-13 20:03:01 Cond_S_cm physical limits         5 making an example
#>     user diff_name
#> 1 runner     diff1
```
