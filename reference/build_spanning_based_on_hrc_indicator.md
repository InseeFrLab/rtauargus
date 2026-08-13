# Build a data frame of indicators without equations

Internal helper that aggregates rows from `df_without_group` that have a
non-`NA` `hrc_indicator`, and appends them to `df_spannings`. The result
is used to represent response variables that are linked by a hierarchy
but not by any equation.

## Usage

``` r
build_spanning_based_on_hrc_indicator(df_without_group, df_spannings)
```

## Arguments

- df_without_group:

  A data frame containing the rows of `df_spannings_eq` that do not
  belong to any equation group (`group` is `NA`). Must contain the
  following columns: `table_name`, `field`, `hrc_field`, `indicator`,
  and `hrc_indicator`.

- df_spannings:

  A data frame derived from `df_metadata_long` with renamed spanning and
  indicator variables. It is used as the base to which the newly built
  rows are appended via `bind_rows`.

## Value

A data frame with one row per `table_name` for the non-`NA`
`hrc_indicator` rows, appended to `df_spannings` and sorted by
`table_name`. The returned columns are:

- table_name:

  Name of the table.

- field:

  Last value of `field` within the group.

- hrc_field:

  Last value of `hrc_field` within the group.

- spanning:

  Uppercase `hrc_indicator` suffixed with `^h`.

- hrc_spanning:

  Last value of `hrc_indicator` within the group.

- indicator:

  Last value of `indicator` within the group.

- hrc_indicator:

  Last value of `hrc_indicator` within the group.
